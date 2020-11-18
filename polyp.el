;;; polyp.el --- Nested modal keybindings -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/minad/polyp

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Nested modal keybindings

;;; Code:

(require 'cl-lib)

;;;; Variables

(defvar display-line-numbers)
(defvar display-fill-column-indicator)
(defvar tab-line-format)
(defvar golden-ratio-mode)

(cl-defstruct (polyp- (:constructor polyp--make) (:copier nil)) name handler prev status depth)

(defvar polyp--window nil
  "Current Polyp window.")

(defvar polyp--window-update nil
  "Polyp window update status.")

(defconst polyp--window-buffer " *polyp*")
(with-eval-after-load 'ace-window
  (defvar aw-ignored-buffers)
  (push polyp--window-buffer aw-ignored-buffers))

(defvar polyp--active nil
  "Polyp is active.")

(defvar polyp--stack nil
  "Head of Polyp stack.")

(defvar polyp--status-cache nil
  "Cached status information shown in the mode line.")

(defvar polyp--status-minibuffer nil
  "Minibuffer depths.")

;;;; Customization

(defgroup polyp nil
  "Polyp customizations."
  :group 'bindings
  :prefix "polyp-")

(defcustom polyp-desc-markup
  '(("_" . font-lock-function-name-face)
    ("*" . font-lock-constant-face)
    ("=" . font-lock-keyword-face))
  "Highlighting characters and fonts used for the Polyp descriptions."
  :type 'alist
  :group 'polyp)

(defcustom polyp-bind-key
  'bind-key
  "Function which Polyp uses used to define key bindings."
  :type 'symbol
  :group 'polyp)

(defvar polyp-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector help-char) #'polyp--help-command)
    (define-key map [?\C-g] #'polyp--keyboard-quit)
    (define-key map [?\e ?\e ?\e] #'polyp--keyboard-escape-quit)
    (define-key map [?\C-u] #'polyp--universal-argument)
    (define-key map [?u] #'polyp--universal-argument)
    (define-key map [?-] #'polyp--negative-argument)
    (define-key map [kp-subtract] #'polyp--negative-argument)
    (dotimes (n 10)
      (define-key map (vector (intern (format "kp-%s" n))) #'polyp--digit-argument)
      (define-key map (vector (+ ?0 n)) #'polyp--digit-argument))
    map)
  "Keymap used as parent keymap for the transient maps.")

;;;; Utilities

(defsubst polyp--active-name ()
  "Name of current Polyp, which must be active."
  (cl-assert (and polyp--active polyp--stack (= (polyp--depth polyp--stack) (recursion-depth))))
  (polyp--name polyp--stack))

(defsubst polyp--inactive-name ()
  "Name of current Polyp, which must be inactive."
  (cl-assert (and (not polyp--active) polyp--stack (= (polyp--depth polyp--stack) (recursion-depth))))
  (polyp--name polyp--stack))

(defsubst polyp--should-reactivate ()
  "Return t if Polyp on the stack must be reactivated."
  (cl-assert (and (not polyp--active)
                  (or (not polyp--stack) (<= (polyp--depth polyp--stack) (recursion-depth)))))
  (and polyp--stack (= (polyp--depth polyp--stack) (recursion-depth))))

(defun polyp--reject (keys map)
  "Remove all KEYS from property MAP."
  (let ((res))
    (while map
      (if (memq (car map) keys)
          (setq map (cddr map))
        (push (car map) res)
        (setq map (cdr map))))
    (nreverse res)))

(defsubst polyp--as-list (x)
  "Return value X as list."
  (if (listp x) x (list x)))

;;;; Window functions (derived from lv.el by abo-abo)

(defun polyp--window-make ()
  "Create Polyp window."
  (with-selected-window
      (setq polyp--window (let ((ignore-window-parameters t))
                            (split-window (frame-root-window) -1 'below)))
    (if (get-buffer polyp--window-buffer)
        (switch-to-buffer polyp--window-buffer 'norecord)
      (switch-to-buffer polyp--window-buffer 'norecord)
      (fundamental-mode)
      (set-window-hscroll polyp--window 0)
      (setq window-size-fixed t
            mode-line-format nil
            header-line-format nil
            tab-line-format nil
            cursor-type nil
            display-line-numbers nil
            display-fill-column-indicator nil)
      (set-window-dedicated-p polyp--window t)
      (set-window-parameter polyp--window 'no-other-window t))))

(defun polyp--window-show (str)
  "Show STR in Polyp window."
  (let (deactivate-mark golden-ratio-mode)
    (unless (window-live-p polyp--window) (polyp--window-make))
    (with-selected-window polyp--window
      (unless (string= (buffer-string) str)
        (delete-region (point-min) (point-max))
        (insert str)
        (setq-local window-min-height (cl-count ?\n str))
        (setq truncate-lines (> window-min-height 1))
        (let ((window-resize-pixelwise t)
              window-size-fixed)
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun polyp--window-hide ()
  "Hide Polyp window."
  (when (window-live-p polyp--window)
    (let ((buf (window-buffer polyp--window)))
      (delete-window polyp--window)
      (kill-buffer buf))))

;;;; Description string parsing

(defsubst polyp--desc-toggle (flag)
  "Generate a toggle string depending on FLAG."
  (concat "(" (if flag #("•" 0 1 (face success)) " ") ")"))

(defmacro polyp--desc-toggle! (flag)
  "Macro used to generate a toggle for FLAG."
  (if (symbolp flag)
      `(polyp--desc-toggle (and (boundp ',flag) ,flag))
    `(polyp--desc-toggle ,flag)))

(defun polyp--desc-colorize (str)
  "Colorize the string STR according to `polyp-desc-markup'."
  (let* ((chars (apply #'concat (mapcar #'car polyp-desc-markup)))
         (regexp (format "\\([%s]\\)\\([^%s]+\\)\\1" chars chars))
         (str (replace-regexp-in-string "\\^\\|^\n" "" str)))
    (save-match-data
      (while (string-match regexp str)
        (setq str (replace-match
                   (propertize (match-string 2 str) 'face
                               (cdr (assoc (match-string 1 str) polyp-desc-markup)))
                   t nil str))))
    str))

(defun polyp--desc-parse (desc)
  "Parse the description string DESC."
  (let ((str "") fields)
    (setq desc (replace-regexp-in-string "%t(" "%(polyp--desc-toggle! " desc))
    (save-match-data
      (while (string-match "\\(%[^(`]*\\)[(`]" desc)
        (let ((s (match-string 1 desc))
              (r (read-from-string (replace-regexp-in-string "^`" " " (substring desc (match-end 1))))))
          (setq str (concat str (substring desc 0 (match-beginning 0)) "%s")
                desc (substring desc (+ (match-end 1) (cdr r))))
          (push (if (string= s "%") (car r) `(format ,s ,(car r))) fields))))
    (cons (polyp--desc-colorize (concat str desc)) (nreverse fields))))

;;;; Main code

(defun polyp--restore-hook ()
  "Post-command hook used by `polyp--restore'."
  (remove-hook 'post-command-hook #'polyp--restore-hook)
  (polyp--restore))

(defun polyp--restore ()
  "Restore Polyp."
  (cond
   (polyp--active nil)
   (overriding-terminal-local-map (add-hook 'post-command-hook #'polyp--restore-hook))
   ((polyp--should-reactivate) (funcall (polyp--inactive-name) 'on))))

(defmacro polyp--protect (body)
  "Suspend and restore the Polyp around BODY."
  `(unwind-protect (catch 'exit ,body) (polyp--restore)))

(defmacro polyp--call (cmd)
  "Call Polyp function CMD, which can be a symbol, a key string or a sexp."
  `(polyp--protect
    ,(cond
      ((symbolp cmd)
       `(call-interactively (setq this-command #',cmd)))
      ((stringp cmd)
       `(let ((bind (key-binding ,(kbd cmd))))
          (if (commandp bind t)
              (call-interactively (setq this-command bind))
            (setq unread-command-events
                  (append
                   ',(mapcar (lambda (x) (cons t x)) (listify-key-sequence (kbd cmd)))
                   unread-command-events)))))
      (t cmd))))

(defsubst polyp--valid-keys (keys)
  "Return t if KEYS is part of the Polyp keymap."
  (eq this-command (lookup-key (symbol-value (polyp--active-name)) keys)))

(defun polyp--handler-ignore ()
  "Polyp event handler. Foreign keys are ignored."
  (let ((keys (this-single-command-keys)))
    (unless (polyp--valid-keys keys)
      ;; Ignore command
      (setq this-command #'ignore)
      (message "%s is undefined" (key-description keys)))))

(defun polyp--handler-run ()
  "Polyp event handler. Foreign keys are executed."
  (when (and (not (polyp--valid-keys (this-single-command-keys))) this-command)
    ;; Suspend current Polyp, run command.
    (funcall (polyp--active-name) 'off)
    (let ((foreign this-command))
      (setq this-command
            (lambda (&optional arg)
              (interactive "P")
              (setq this-command foreign
                    current-prefix-arg arg)
              (polyp--protect (call-interactively foreign)))))))

(defun polyp--handler-quit ()
  "Polyp event handler. The Polyp is left on a foreign key press."
  (let ((keys (this-single-command-keys)))
    (unless (polyp--valid-keys keys)
      ;; Quit current Polyp, reexecute command.
      (funcall (polyp--active-name) 'quit)
      (when (polyp--should-reactivate) (funcall (polyp--inactive-name) 'on))
      (setq this-command #'ignore
            unread-command-events
            (append
             (mapcar (lambda (x) (cons t x))
                     (append (if prefix-arg
                                 ;; HACK: For some reason this-command-keys does not include the prefix, add it manually.
                             (listify-key-sequence (format "\C-u%s" (prefix-numeric-value prefix-arg))))
                             (listify-key-sequence keys)))
             unread-command-events)))))

(defun polyp--bind-keys (map keys cmd)
  "Bind a list of KEYS to CMD in the keymap MAP."
  (mapcar (lambda (k) `(,polyp-bind-key ,(vconcat (kbd k)) #',cmd ,map)) keys))

(defun polyp--cmd-enter (name cmd)
  "Generate enter command for Polyp named NAME.
The command CMD is executed after showing the Polyp description."
  `(,(format "Enter Polyp `%s' and call `%s'." name cmd)
    (interactive)
    (,name 'enter)
    (polyp--call ,cmd)))

(defun polyp--cmd-quit (name cmd)
  "Generate quit command for Polyp named NAME.
The command CMD is executed after quitting the Polyp."
  `(,(format "Quit Polyp `%s' and call `%s'." name cmd)
    (interactive)
    (,name 'quit)
    (polyp--call ,cmd)))

(defun polyp--opt-hook (opts name)
  "Get hook option NAME from OPTS plist."
  (if-let (x (plist-get opts name))
      (if (symbolp x) `((,x)) `(,x))))

;;;###autoload
(defmacro polyp (name &rest body)
  "Define a Polyp with a given NAME.

The description can contain syntax highlighting.

The first argument of the BODY can be an optional description string.

After that, the following keyword arguments can be specified:

- :outer-map Keymap used for the outer bindings.
- :base-map  Base keymap used for the transient bindings.
- :bind      Bindings to which this Polyp is bound in the outer keymap.
- :enter     Action to perform before entering the Polyp.
- :quit      Action to perform after quitting the Polyp.
- :on        Action to perform when Polyp is activated.
- :off       Action to perform when Polyp is deactivated.
- :update    Action to perform after each action, when Polyp is active.
- :handler   Specifies the Polyp handler, which handles foreign keys.
- :status    Specifies the status string shown in the mode-line.
- :which-key Enable which-key popup.

Then a list of key bindings can be given of the form:

    (\"key\" cmd \"outer-key1\" \"outer-key2\"...)
    ((\"key1\" \"key2\") cmd \"outer-key1\" \"outer-key2\"...)
    (\"key\" cmd :quit)
    ((\"key1\" \"key2\") cmd :quit)

The keys are bound to the transient map of the Polyp, while
the outer keys are added to both the transient map and the outer map.
The bindings which specify :quit, quit the polyp."
  (declare (indent defun))
  (let* ((opts (if (stringp (car body)) (cdr body) body))
         (desc (if (stringp (car body)) (car body)))
         (body (polyp--reject '(:enter :quit :on :off :update :handler :bind :base-map :outer-map :status :which-key) opts))
         (desc-quit '((when polyp--window-update (polyp--window-hide) (setq polyp--window-update nil))))
         (desc-update (if desc
                          (pcase-let ((`(,desc . ,fields) (polyp--desc-parse desc)))
                            (if fields
                                `((polyp--window-show (format ,desc ,@fields))
                                  (setq polyp--window-update ',name))
                              `((unless (eq polyp--window-update ',name)
                                  (polyp--window-show ,desc)
                                  (setq polyp--window-update ',name)))))
                        desc-quit))
         (opt-outer-map (or (plist-get opts :outer-map) 'global-map))
         (opt-base-map (or (plist-get opts :base-map) 'polyp-base-map))
         (opt-handler (intern (format "polyp--handler-%s" (or (plist-get opts :handler) 'quit))))
         (opt-status (or (plist-get opts :status) (format " %s " name)))
         (opt-which-key (plist-get opts :which-key))
         (opt-update `(,@desc-update
                       ,@(polyp--opt-hook opts :update)))
         (opt-on (polyp--opt-hook opts :on))
         (opt-off (polyp--opt-hook opts :off))
         (opt-quit `(,@desc-quit
                     ,@(polyp--opt-hook opts :quit)
                     ,@(if opt-which-key '((polyp--which-key-quit)))))
         (opt-enter `(,@(polyp--opt-hook opts :enter)
                      ,@(if opt-which-key '((polyp--which-key-enter)))))
         (opt-bind (polyp--as-list (plist-get opts :bind)))
         (used-names)
         (new (gensym)))
    `(progn
       ;; The main function of the Polyp.
       (defun ,name (&optional op)
         ,(format "Polyp `%s'." name)
         (interactive)
         (pcase-exhaustive op
           ('off
            (cl-assert (eq (polyp--active-name) ',name))
            ,@opt-off
            (internal-pop-keymap ,name 'overriding-terminal-local-map)
            (remove-hook 'pre-command-hook (polyp--handler polyp--stack))
            (setq polyp--active nil)
            (polyp--status-update))
           ('quit
            (when polyp--active (,name 'off))
            (cl-assert (eq (polyp--inactive-name) ',name))
            ,@opt-quit
            (setq polyp--stack (polyp--prev polyp--stack))
            (polyp--status-update))
           ('on
            (cl-assert (eq (polyp--inactive-name) ',name))
            (setq polyp--active t)
            (add-hook 'pre-command-hook (polyp--handler polyp--stack))
            (internal-push-keymap ,name 'overriding-terminal-local-map)
            ,@opt-on
            (polyp--status-update))
           ((or 'nil 'enter)
            ;; Switch off for 'enter, switch on for nil.
            ;; nil argument is passed if the polyp is called from outside.
            (if (and polyp--active (eq (polyp--active-name) ',name))
                (when (eq op 'enter) (,name 'off))
              (let ((,new (polyp--make :name ',name
                                       :handler #',opt-handler
                                       :prev polyp--stack
                                       :status ,opt-status
                                       :depth (recursion-depth))))
                (when polyp--active (funcall (polyp--active-name) 'off))
                (setq polyp--stack ,new)
                ,@opt-enter
                (unless (eq op 'enter) (,name 'on))))))
         (unless (eq op 'quit)
           ,@opt-update))

       ;; Create keymap which inherits from :base-map
       (setq ,name (make-composed-keymap (make-sparse-keymap) ,opt-base-map))

       ;; Bind main keys
       ,@(polyp--bind-keys opt-outer-map opt-bind name)

       ;; Generate code for the bindings
       ,@(mapcan
          (pcase-lambda (`(,keys ,cmd . ,enter))
            ;; Normalize keys
            (setq keys (mapcar (lambda (k) (key-description (kbd k))) (polyp--as-list keys)))
            (when (stringp cmd) (setq cmd (key-description (kbd cmd))))
            (let ((sym (intern (format "%s/%s" name cmd)))
                  (kw (and (= 1 (length enter)) (keywordp (car enter)))))
              ;; Ensure that function name is unique
              (when (memq sym used-names)
                (setq sym (intern (format "%s/%s[%s]" name cmd (car keys)))))
              (push sym used-names)
              `((defun ,sym ()
                  ,@(funcall (intern (concat
                                      "polyp--cmd-"
                                      (substring (symbol-name (if kw (car enter) :enter)) 1)))
                             name cmd))
                ,@(unless kw
                    (append (polyp--bind-keys name enter sym)
                            (polyp--bind-keys opt-outer-map enter sym)))
                ,@(polyp--bind-keys name keys sym))))
          body)
       ',name)))

;;;; Default commands

(defun polyp-repeat (&optional arg)
  "Repeat last Polyp command. The prefix argument can be overwritten by ARG."
  (interactive "p")
  (when (or (not last-command) (eq last-command 'polyp-repeat))
    (user-error "Nothing to repeat"))
  (setq current-prefix-arg (if (eq arg 1) last-prefix-arg arg)
        this-command last-command)
  (when (eq last-command 'self-insert-command)
    (setq last-command-event (char-before)))
  (let ((n (prefix-numeric-value current-prefix-arg)))
    (if (and current-prefix-arg (/= n 1))
        (message "Repeat %sx %s" n this-command)
      (message "Repeat %s" this-command)))
  (call-interactively this-command))

(defun polyp--help-command ()
  "Redirect to `prefix-help-command`."
  (interactive)
  (call-interactively prefix-help-command))

;; The functions `universal-argument', `digit-argument' and `negative-argument' must be
;; replicated for Polyp, since the Emacs functions push their own transient map.
;; This means that the Polyp keys like "u" do not work while the transient map is active.
(defun polyp--universal-argument (arg)
  "Replacement for `universal-argument', to be used by Polyp. Takes prefix ARG."
  (interactive "P")
  (prefix-command-preserve-state)
  (setq prefix-arg (cond
                    ((consp arg) (list (* 4 (car arg))))
                    ((eq arg '-) '(-4))
                    (t '(4)))))

(defun polyp--digit-argument (arg)
  "Replacement for `digit-argument', to be used by Polyp. Takes prefix ARG."
  (interactive "P")
  (prefix-command-preserve-state)
  (let* ((char (if (integerp last-command-event)
		   last-command-event
		 (get last-command-event 'ascii-character)))
	 (digit (- (logand char ?\177) ?0)))
    (setq prefix-arg (cond ((integerp arg)
                            (+ (* arg 10)
			       (if (< arg 0) (- digit) digit)))
                           ((eq arg '-)
                            (if (zerop digit) '- (- digit)))
                           (t
                            digit)))))

(defun polyp--negative-argument (arg)
  "Replacement for `negative-argument', to be used by Polyp. Takes prefix ARG."
  (interactive "P")
  (prefix-command-preserve-state)
  (setq prefix-arg (cond ((integerp arg) (- arg))
                         ((eq arg '-) nil)
                         (t '-))))

(defun polyp--keyboard-quit ()
  "Quit the current Polyp and restore the previous Polyp."
  (interactive)
  (funcall (polyp--active-name) 'quit)
  (polyp--call #'keyboard-quit))

(defun polyp--keyboard-escape-quit ()
  "Quit the current Polyp and restore the previous Polyp."
  (interactive)
  (funcall (polyp--active-name) 'quit)
  (polyp--call #'keyboard-escape-quit))

;;;; Status indicator in the mode line

(defsubst polyp--status-update ()
  "Update Polyp mode line status."
  (when polyp-mode
    (setq polyp--status-cache nil)
    (force-mode-line-update t)))

(defsubst polyp--status-color (str font)
  "Colorize status string STR. Take color from FONT."
  (propertize str 'face `(:foreground ,(face-attribute font :foreground))))

(defun polyp--status-recursion (n m)
  "Recursion indicator string for recursion depths from N to M."
  (let ((str ""))
    (while (< n m)
      (setq n (1+ n)
            str (concat
                 (if (memq n polyp--status-minibuffer)
                     (polyp--status-color "↲" 'font-lock-warning-face)
                   (polyp--status-color "⟲" 'font-lock-constant-face))
                 str)))
    str))

(defun polyp--status-compute ()
  "Compute the Polyp mode line status."
  (let* ((depth (recursion-depth))
         (last-depth depth)
         (str "")
         (p polyp--stack))
    (if (and polyp--status-cache (= (car polyp--status-cache) depth))
        (cdr polyp--status-cache)
      (while p
        (setq str (concat
                   str
                   (polyp--status-recursion (polyp--depth p) last-depth)
                   (polyp--status-color (polyp--status p)
                                        (if (and polyp--active (eq p polyp--stack))
                                            'font-lock-function-name-face 'font-lock-comment-face)))
              last-depth (polyp--depth p)
              p (polyp--prev p)))
      (setq str (concat str (polyp--status-recursion 0 last-depth)))
      (if (string= str "") (setq str "⊥"))
      (cdr (setq polyp--status-cache (cons depth (string-trim str)))))))

(defun polyp--minibuffer-setup ()
  "Minibuffer setup hook."
  (push (recursion-depth) polyp--status-minibuffer)
  ;; Entering the minibuffer is only allowed if there is no active Polyp.
  ;; This is important since the minibuffer introduces another recursion level.
  (when polyp--active (user-error "Active Polyp when entering the minibuffer")))

(defun polyp--minibuffer-exit ()
  "Minibuffer exit hook."
  (pop polyp--status-minibuffer))

(add-hook 'minibuffer-setup-hook #'polyp--minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'polyp--minibuffer-exit)

;;;###autoload
(define-minor-mode polyp-mode
  "Minor mode which shows the current Polyp in the mode-line."
  :global t
  (if (not polyp-mode)
      (setq mode-line-misc-info (assq-delete-all 'polyp-mode mode-line-misc-info))
    (push '(polyp-mode ("[" (:eval (polyp--status-compute)) "] ")) mode-line-misc-info)
    (polyp--status-update)))

;;;; * Which-key integration

(declare-function which-key--hide-popup "which-key" ())
(defvar which-key-show-transient-maps)
(defvar which-key-persistent-popup)
(defvar polyp--which-key-state nil)

(defsubst polyp--which-key-enter ()
  "Called when Polyp with which-key support is entered."
  (setq polyp--which-key-state (cons which-key-show-transient-maps which-key-persistent-popup)
        which-key-show-transient-maps t
        which-key-persistent-popup t))

(defsubst polyp--which-key-quit ()
  "Called when Polyp with which-key support is quitting."
  (let ((state (pop polyp--which-key-state)))
    (setq which-key-show-transient-maps (car state)
          which-key-persistent-popup (cdr state)))
  (unless polyp--which-key-state (which-key--hide-popup)))

(provide 'polyp)
;;; polyp.el ends here
