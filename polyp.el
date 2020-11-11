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

(defvar display-line-numbers)
(defvar display-fill-column-indicator)
(defvar tab-line-format)
(defvar golden-ratio-mode)
(defvar polyp--foreign)

(cl-defstruct (polyp- (:constructor polyp--make) (:copier nil)) name clear prev)

(defvar polyp--window nil
  "Current Polyp window.")

(defvar polyp--update nil
  "Polyp window update status.")

(defvar polyp--active nil
  "Currently active Polyp.")

(defvar polyp-status nil
  "The Polyp status, which is shown in the mode-line if `polyp-mode' is enabled.")

(defgroup polyp nil
  "Polyp customizations."
  :group 'bindings
  :prefix "polyp-")

(defcustom polyp-highlight
  '(("_" . font-lock-function-name-face)
    ("*" . font-lock-constant-face)
    ("=" . font-lock-keyword-face))
  "Highlighting characters and fonts used for the Polyp descriptions."
  :type 'alist
  :group 'polyp)

(defcustom polyp-bind
  'bind-key
  "Function which Polyp uses used to define key bindings."
  :type 'symbol
  :group 'polyp)

(defcustom polyp-mode-line
  '("[" (:propertize polyp-status face warning) "]")
  "Polyp mode line format.
The current Polyp is shown in the mode-line if `polyp-mode' is enabled."
  :type 'sexp
  :group 'polyp)
;;;###autoload (put 'polyp-mode-line 'risky-local-variable t)

(defconst polyp--buffer-name " *polyp*")

(defvar polyp-base-map
  (let ((map (copy-keymap universal-argument-map)))
    (assq-delete-all 'switch-frame map))
  "Keymap used as parent keymap for the transient maps.")

;; taken from lv.el
(defun polyp--window-make ()
  "Create Polyp window."
  (setq polyp--window (let ((ignore-window-parameters t))
                        (split-window (frame-root-window) -1 'below)))
  (with-selected-window polyp--window
    (if (get-buffer polyp--buffer-name)
        (switch-to-buffer polyp--buffer-name 'norecord)
      (switch-to-buffer polyp--buffer-name 'norecord)
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

;; taken from lv.el
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

;; taken from lv.el
(defun polyp--window-hide ()
  "Hide Polyp window."
  (when (window-live-p polyp--window)
    (let ((buf (window-buffer polyp--window)))
      (delete-window polyp--window)
      (kill-buffer buf))))

(defmacro polyp--clearfun (name foreign)
  "Create clear function for Polyp NAME.
The FOREIGN argument specifies the behavior if a foreign key is pressed."
  `(lambda ()
     (cond
      ;; Always quit on handle-switch-frame/keyboard-quit.
      ((memq this-command '(handle-switch-frame keyboard-quit)) (,name 'quit))

      ;; Key found - keep the transient map alive.
      ((eq this-command (lookup-key ,name (this-single-command-keys))) nil)

      ;; Foreign key
      (t ,@(pcase-exhaustive foreign
             (`'ignore ;; Ignore command
              `((setq this-command 'ignore)
                (message "%s is undefined" (key-description (this-single-command-keys)))))
             (`'run ;; Suspend current Polyp, run command
              `((when this-command
                  (setq polyp--foreign this-command
                        this-command 'polyp--foreign))))
             (`nil ;; Quit current Polyp, reexecute command
              `((,name 'quit)
                (let ((cmd (key-binding (this-single-command-keys))))
                  (setq this-command (if (commandp cmd) cmd))))))))))

(defmacro polyp--save (body)
  "Save and restore the Polyp status around BODY."
  (let ((p (gensym)))
    `(let ((,p polyp--active))
       (when ,p (funcall (polyp--name ,p) 'off))
       (unwind-protect ,body (polyp--restore ,p)))))

(defun polyp--restore (p)
  "Restore Polyp P."
  (cond
   (polyp--active
    (let ((q polyp--active))
      (while (polyp--prev q)
         (setq q (polyp--prev q)))
      (setf (polyp--prev q) p)))
   (overriding-terminal-local-map
    (let ((n (make-symbol "polyp--restore")))
      (fset n (lambda ()
                (unless overriding-terminal-local-map
                  (remove-hook 'post-command-hook n)
                  (polyp--restore p))))
      (add-hook 'post-command-hook n)))
   (t
    (funcall (polyp--name (setq polyp--active p)) 'on))))

(defun polyp--foreign ()
  "Execute foreign command while Polyp is suspended."
  (interactive)
  (polyp--save (call-interactively (setq this-command polyp--foreign))))

(defun polyp--toggle (flag)
  "Generate a toggle string depending on FLAG."
  (concat "(" (if flag (propertize "•" 'face 'success) " ") ")"))

(defmacro polyp--toggle! (flag)
  "Macro used to generate a toggle for FLAG."
  (if (symbolp flag)
      `(polyp--toggle (and (boundp ',flag) ,flag))
    `(polyp--toggle ,flag)))

(defun polyp--colorize (str)
  "Colorize the string STR according to `polyp-highlight'."
  (let* ((chars (apply #'concat (mapcar #'car polyp-highlight)))
         (regexp (format "\\([%s]\\)\\([^%s]+\\)\\1" chars chars))
         (str (replace-regexp-in-string "\\^\\|^\n" "" str)))
    (save-match-data
      (while (string-match regexp str)
        (setq str (replace-match
                   (propertize (match-string 2 str) 'face
                               (cdr (assoc (match-string 1 str) polyp-highlight)))
                   t nil str))))
    str))

(cl-defun polyp--parse-desc (desc &aux (str "") fields)
  "Parse the description string DESC."
  (setq desc (replace-regexp-in-string "%t(" "%(polyp--toggle! " desc))
  (save-match-data
    (while (string-match "\\(%[^(]*\\)(" desc)
      (let ((s (match-string 1 desc))
            (r (read-from-string (substring desc (match-end 1)))))
        (setq str (concat str (substring desc 0 (match-beginning 0)) "%s")
              desc (substring desc (+ (match-end 1) (cdr r))))
        (push (if (string= s "%") (car r) `(format ,s ,(car r))) fields))))
  (cons (polyp--colorize (concat str desc)) (nreverse fields)))

(defun polyp--bind-keys (map keys fun)
  "Bind a list of KEYS to FUN in the keymap MAP."
  (mapcar (lambda (k) `(,polyp-bind ,k ',fun ,map)) (if (stringp keys) (list keys) keys)))

(cl-defun polyp--reject (keys map &aux res)
  "Remove all KEYS from property MAP."
  (while map
    (if (memq (car map) keys)
        (setq map (cddr map))
      (push (car map) res)
      (setq map (cdr map))))
  (nreverse res))

(defmacro polyp--call (fun)
  "Call Polyp function FUN."
  (cond
   ((symbolp fun)
    `(call-interactively (setq this-command ',fun)))
   ((stringp fun)
    `(let ((cmd (key-binding ,(kbd fun))))
       (if (commandp cmd)
           (call-interactively (setq this-command cmd))
         (setq unread-command-events ',(listify-key-sequence (kbd fun))))))
   (t fun)))

(defun polyp--enter (name fun)
  "Generate enter function for Polyp named NAME.
The function FUN is executed after showing the Polyp description."
  `(,(format "Show Polyp `%s' and call `%s'." name fun)
    (interactive)
    (,name)
    (polyp--save (polyp--call ,fun))))

(defun polyp--quit (name fun)
  "Generate quit function for Polyp named NAME.
The function FUN is executed after hiding the Polyp description."
  `(,(format "Hide Polyp `%s' and call `%s'." name fun)
    (interactive)
    (,name 'quit)
    (polyp--call ,fun)))

(defmacro polyp--name? (p)
  "Name of Polyp P or nil."
  (let ((q (gensym)))
    `(let ((,q ,p)) (and ,q (polyp--name ,q)))))

;;;###autoload
(defmacro defpolyp (name &rest body)
  "Define a Polyp with a given NAME.

The description can contain syntax highlighting.

The first argument of the BODY can be an optional description string.

After that, the following keyword arguments can be specified:

- :global-map Keymap used for the global bindings.
- :base-map   Base keymap used for the transient bindings.
- :bind       Bindings to which this Polyp is bound in the global keymap.
- :enter      Action to perform before entering the Polyp.
- :leave      Action to perform after quitting the Polyp.
- :on         Action to perform when Polyp is activated.
- :off        Action to perform when Polyp is deactivated.
- :foreign    Specifies the behavior if a foreign key is pressed.
- :status     Specifies the status string shown in the mode-line.

Then a list of key bindings can be given of the form:

    (\"key\" function \"global-key1\" \"global-key2\"...)
    ((\"key1\" \"key2\") function \"global-key1\" \"global-key2\"...)
    (\"key\" function :quit)
    ((\"key1\" \"key2\") function :quit)

The keys are bound to the transient map of the Polyp, while
the global keys are added to both the transient map and the global map.
The bindings which specify :quit, quit the polyp."
  (let* ((opts (if (stringp (car body)) (cdr body) body))
         (desc (if (stringp (car body)) (car body)))
         (opt-global-map (or (plist-get opts :global-map) 'global-map))
         (opt-base-map (or (plist-get opts :base-map) 'polyp-base-map))
         (opt-foreign (plist-get opts :foreign))
         (opt-enter (plist-get opts :enter))
         (opt-leave (plist-get opts :leave))
         (opt-on (plist-get opts :on))
         (opt-off (plist-get opts :off))
         (opt-bind (plist-get opts :bind))
         (opt-status (or (plist-get opts :status) (symbol-name name)))
         (desc-hide '((when polyp--update (polyp--window-hide) (setq polyp--update nil))))
         (desc-show desc-hide)
         (tmp (gensym)))

    ;; Process description
    (when desc
      (pcase-let ((`(,desc . ,fields) (polyp--parse-desc desc)))
        (setq desc-show (if fields
                            `((polyp--window-show (format ,desc ,@fields))
                              (setq polyp--update ',name))
                          `((unless (eq polyp--update ',name)
                              (polyp--window-show ,desc)
                              (setq polyp--update ',name)))))))

    `(progn
       ;; The main function of the Polyp.
       (defun ,name (&optional op)
         ,(format "Polyp `%s'." name)
         (interactive)
         (pcase op
           ('off
            (cl-assert (eq (polyp--name? polyp--active) ',name))
            ,@(if opt-off `(,opt-off))
            (internal-pop-keymap ,name 'overriding-terminal-local-map)
            (remove-hook 'pre-command-hook (polyp--clear polyp--active))
            (setq polyp--active nil
                  polyp-status nil))
           ('quit
            (cl-assert (eq (polyp--name? polyp--active) ',name))
            (let ((,tmp (polyp--prev polyp--active)))
              (,name 'off)
              ,@(if opt-leave `(,opt-leave))
              ,@desc-hide
              (when ,tmp (polyp--restore ,tmp))))
           (_
            (let ((,tmp polyp--active))
              (cl-assert (or (not (eq op 'on)) (eq (polyp--name? ,tmp) ',name)))
              (unless (or (eq op 'on) (eq (polyp--name? ,tmp) ',name))
                  (when ,tmp (funcall (polyp--name ,tmp) 'off))
                  ,@(if opt-enter `(,opt-enter))
                  (setq polyp--active (polyp--make :name ',name
                                                   :clear (polyp--clearfun ,name ,opt-foreign)
                                                   :prev ,tmp)
                        op 'on)))
            (when (eq op 'on)
              (add-hook 'pre-command-hook (polyp--clear polyp--active))
              (internal-push-keymap ,name 'overriding-terminal-local-map)
              (setq polyp-status ,opt-status)
              ,@(if opt-on `(,opt-on)))
            ,@desc-show)))

       ;; Create keymap which inherits from :base-map
       (setq ,name (make-composed-keymap (make-sparse-keymap) ,opt-base-map))

       ;; Bind main keys
       ,@(polyp--bind-keys nil opt-bind name)

       ;; Generate code for the bindings
       ,@(mapcan
          (pcase-lambda (`(,key ,fun . ,enter))
            (let ((sym (intern (format "%s/%s" name fun))))
              `((defun ,sym ()
                  ,@(if (equal enter '(:quit)) (polyp--quit name fun) (polyp--enter name fun)))
                ,@(unless (equal enter '(:quit))
                    (append (polyp--bind-keys name enter sym)
                            (polyp--bind-keys opt-global-map enter sym)))
                ,@(polyp--bind-keys name key sym))))
          (polyp--reject '(:enter :leave :on :off :foreign :bind :base-map :global-map) opts))
       ',name)))

;;;###autoload
(define-minor-mode polyp-mode
  "Minor mode which shows the current Polyp in the mode-line."
  :global t
  (if polyp-mode
      (push '(polyp-status ("" polyp-mode-line " ")) mode-line-misc-info)
    (setq mode-line-misc-info (assq-delete-all 'polyp-status mode-line-misc-info))))

(provide 'polyp)
;;; polyp.el ends here
