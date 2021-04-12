;;; polyp.el --- Nested transient menus -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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

;; Nested transient menus

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Variables

(defvar display-line-numbers)
(defvar display-fill-column-indicator)
(defvar tab-line-format)

(cl-defstruct (polyp- (:constructor polyp--make) (:copier nil)) name handler prev lighter)

(defvar polyp--window nil
  "Current Polyp window.")

(defvar polyp--window-update nil
  "Polyp window update status.")

(defconst polyp--window-buffer " *polyp*")

(defvar polyp--active nil
  "Polyp is active.")

(defvar polyp--stack nil
  "Head of Polyp stack.")

(defvar polyp--lighter-string nil
  "Lighter information shown in the mode line.")

;; Allow properties in polyp--lighter-string
(put 'polyp--lighter-string 'risky-local-variable t)

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
  :type 'alist)

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

(defun polyp--reject (keys map)
  "Remove all KEYS from property MAP."
  (let ((res))
    (while map
      (if (memq (car map) keys)
          (setq map (cddr map))
        (push (car map) res)
        (setq map (cdr map))))
    (nreverse res)))

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
  (let (deactivate-mark)
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

(defun polyp--desc-toggle (flag)
  "Generate a toggle string depending on FLAG."
  (concat "(" (if flag #("•" 0 1 (face success)) " ") ")"))

(defmacro polyp--desc-toggle! (flag)
  "Macro used to generate a toggle for FLAG."
  `(polyp--desc-toggle ,(if (symbolp flag) `(bound-and-true-p ,flag) flag)))

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
   (polyp--stack (funcall (polyp--name polyp--stack) 'on))))

(defmacro polyp--protect (body)
  "Suspend and restore the Polyp around BODY."
  `(unwind-protect ,body (polyp--restore)))

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

(defun polyp--valid-keys (keys)
  "Return t if KEYS is part of the Polyp keymap."
  (eq this-command (lookup-key (symbol-value (polyp--name polyp--stack)) keys)))

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
    (funcall (polyp--name polyp--stack) 'off)
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
      ;; Quit current Polyp.
      (while polyp--stack (funcall (polyp--name polyp--stack) 'quit)))))

(defun polyp--define-keys (map keys cmd &optional desc)
  "Bind a list of KEYS to (DESC . CMD) in the keymap MAP."
  (mapcar (lambda (k) `(define-key ,map ,(vconcat (kbd k))
                         ,(if desc `(cons ,(format "%s" desc) #',cmd) `#',cmd)))
          (if (listp keys) keys (list keys))))

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
(defmacro polyp-define (name &rest body)
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
- :lighter   Specifies the lighter string shown in the mode-line.

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
         (body (polyp--reject '(:enter :quit :on :off :update :handler :bind :base-map :outer-map :lighter) opts))
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
         (opt-lighter (or (plist-get opts :lighter) (format " %s " name)))
         (opt-update `(,@desc-update
                       ,@(polyp--opt-hook opts :update)))
         (opt-on (polyp--opt-hook opts :on))
         (opt-off (polyp--opt-hook opts :off))
         (opt-quit `(,@desc-quit
                     ,@(polyp--opt-hook opts :quit)))
         (opt-enter (polyp--opt-hook opts :enter))
         (opt-bind (plist-get opts :bind))
         (used-names))
    `(progn
       ;; Create keymap which inherits from :base-map
       (with-no-warnings (defvar ,name))
       (setq ,name (make-composed-keymap (make-sparse-keymap) ,opt-base-map))

       ;; The main function of the Polyp.
       (defun ,name (&optional op)
         ,(format "Polyp `%s'." name)
         (interactive)
         (pcase-exhaustive op
           ('off
            (cl-assert (and polyp--active (eq (polyp--name polyp--stack) ',name)))
            ,@opt-off
            (remove-hook 'pre-command-hook (polyp--handler polyp--stack))
            (setq overriding-terminal-local-map nil
                  polyp--active nil)
            (polyp--lighter-update))
           ('quit
            (when polyp--active (,name 'off))
            (cl-assert (and (not polyp--active) (eq (polyp--name polyp--stack) ',name)))
            ,@opt-quit
            (setq polyp--stack (polyp--prev polyp--stack))
            (polyp--lighter-update))
           ('on
            (cl-assert (and (not polyp--active) (eq (polyp--name polyp--stack) ',name)))
            (add-hook 'pre-command-hook (polyp--handler polyp--stack))
            (setq overriding-terminal-local-map ,name
                  polyp--active t)
            ,@opt-on
            (polyp--lighter-update))
           ((or 'nil 'enter)
            ;; Switch off for 'enter, switch on for nil.
            ;; nil argument is passed if the polyp is called from outside.
            (if (and polyp--active (eq (polyp--name polyp--stack) ',name))
                (when (eq op 'enter) (,name 'off))
              (when overriding-terminal-local-map
                (user-error "Cannot override keymap"))
              (let ((new-polyp (polyp--make :name ',name
                                            :handler #',opt-handler
                                            :prev polyp--stack
                                            :lighter ,opt-lighter)))
                (when polyp--active (funcall (polyp--name polyp--stack) 'off))
                (setq polyp--stack new-polyp))
              ,@opt-enter
              (unless (eq op 'enter) (,name 'on)))))
         (unless (eq op 'quit)
           ,@opt-update))

       ;; Bind main keys
       ,@(polyp--define-keys opt-outer-map opt-bind name)

       ;; Generate code for the bindings
       ,@(mapcan
          (pcase-lambda (`(,keys ,cmd . ,enter))
            ;; Normalize keys
            (when (stringp cmd) (setq cmd (key-description (kbd cmd))))
            (setq keys (mapcar (lambda (k) (key-description (kbd k))) (if (listp keys) keys (list keys))))
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
                    (append (polyp--define-keys name enter sym cmd)
                            (polyp--define-keys opt-outer-map enter sym cmd)))
                ,@(polyp--define-keys name keys sym cmd))))
          body)
       ',name)))

;;;; Default commands

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
  (funcall (polyp--name polyp--stack) 'quit)
  (polyp--call #'keyboard-quit))

(defun polyp--keyboard-escape-quit ()
  "Quit the current Polyp and restore the previous Polyp."
  (interactive)
  (funcall (polyp--name polyp--stack) 'quit)
  (polyp--call #'keyboard-escape-quit))

;;;; Polyp lighters in the mode line

;;;###autoload
(define-minor-mode polyp-mode
  "Minor mode which shows the current Polyp in the mode-line."
  :global t
  (setq mode-line-misc-info (assq-delete-all 'polyp--lighter-string mode-line-misc-info)
        polyp--lighter-string nil)
  (when polyp-mode
    (push '(polyp--lighter-string ("[" polyp--lighter-string "] ")) mode-line-misc-info))
  (polyp--lighter-update))

(defun polyp--lighter-update ()
  "Update Polyp mode line lighter."
  (when polyp-mode
    (let* ((str nil) (p polyp--stack))
      (while p
        (setq str (concat
                   str
                   (propertize
                    (polyp--lighter p)
                    'face
                    (if (and polyp--active (eq p polyp--stack))
                        'font-lock-function-name-face
                      'font-lock-comment-face)))
              p (polyp--prev p)))
      (setq polyp--lighter-string (and str (replace-regexp-in-string " +" " " (string-trim str))))
      (force-mode-line-update t))))

(provide 'polyp)
;;; polyp.el ends here
