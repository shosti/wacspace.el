;;; wacspace.el --- The WACky WorkSPACE manager for emACS

;; Copyright © 2013 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>
;; URL: http://github.com/shosti/wacspace.el
;; Version: 0.4
;; Created: 26 March 2013
;; Keywords: workspace
;; Package-Requires: ((dash "1.2.0") (cl-lib "0.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides context-aware workspace management for Emacs.  See
;; http://github.com/shosti/wacspace.el for full documentation.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar wacs-regexp-buffer-switching t
  "Use regexp matching for buffer switching in `wacspace'.

When set to t, :buffer option will use a regexp match if a
buffer does not exist with the exact match.")

(defvar wacs-save-frame (display-graphic-p)
  "Save frame with `wacspace'.

When set to t, wacspace will save the frame configuration as
well as the window configuration.  Set to t by default in graphic
display and nil if Emacs is run in a terminal.")

(defvar wacs-main-buffer nil
  "The buffer from which wacspace was called.

Should not be set directly; will be automatically bound when
wacspace is called.")

(defvar wacs-project-base-file ".git"
  "Base file name in projects.

Wacspace will assume that project base directories have this
filename in them.  This variable be dynamically bound within
helper functions.  When set to nil, wacspace will assume that the
current directory is the base directory.")

(defvar wacs-end-of-buffer-modes '(eshell-mode shell-mode comint-mode)
  "Modes in which to scroll to the end of buffers.

Major modes where wacspace will scroll to the end of the
buffer after restoring or setting up.")

(defvar wacs-prefix-map
  "The prefix keymap for wacspace commands.

Bound to C-z by default.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Because it's used within autoloaded macros, wacs--frame-fns need to
;;be autoloaded

(defvar wacs--winconfs nil
  "The wacspace winconf alist.

Should not be altered manually—use `defwinconf' instead.")

;;;###autoload
(defvar wacs--frame-fns nil
  "The wacspace frame function alist.

Should not be altered manually—use `wacs-set-frame-fn' instead.")

(defvar wacs--saved-workspaces (make-hash-table :test 'equal)
  "The hash of saved workspaces.

Should not be altered manually—use `wacspace-save' instead.")

(defconst wacs--numeric-confs '(:default :1 :2 :3 :4 :5 :6 :7 :8 :9)
  "The numeric prefix configurations available to `wacspace'.")

(defvar wacs--project-name-fn nil
  "Function to determine the current project's name.

Should not be altered manually—use the :project-name-fn option
instead.")

(defvar wacs--open-projects nil
  "Alist with configuration for currently open projects.")

(defvar wacs--after-switch-fns (make-hash-table :test 'equal)
  "Hash table of functions to call before switching to projects.

Keys are project names, values are functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wacs-project-dir ()
  "Return the project directory of `wacs-main-buffer'.

Looks for `wacs-project-base-file'.  If not found, defaults to the
current directory."
  (-if-let (dir (buffer-file-name wacs-main-buffer))
    (let ((fname (file-name-directory dir)))
      (expand-file-name
       (-if-let* ((base-file wacs-project-base-file)
                  (project-dir (locate-dominating-file
                                fname
                                base-file)))
         project-dir
         (file-name-directory fname))))
    default-directory))

(defun wacs-project-name ()
  "Return the name of the current project."
  (if wacs--project-name-fn
      (funcall wacs--project-name-fn)
    (-> (wacs-project-dir)
      (split-string "/" t)
      (last)
      (car))))

(defun wacs-make-comint (name program &optional startfile &rest switches)
  "Make a project-specific comint buffer.

Acts as a drop-in replacement for `make-comint' (with equivalent
arguments NAME, PROGRAM, STARTFILE, and SWITCHES).  For best
results, use within `defwacspace' configurations."
  (interactive)
  (let ((default-directory (wacs-project-dir))
        (buffer-name (concat "*" name "*<" (wacs-project-name) ">")))
    (apply 'make-comint-in-buffer
           (append (list name buffer-name program startfile) switches))
    (make-comint-in-buffer name buffer-name program startfile switches)))

(defun wacs-eshell ()
  "Open an eshell in the main project directory."
  (let ((default-directory (wacs-project-dir))
        (eshell-buffer-name (concat "*eshell*<"
                                    (wacs-project-name)
                                    ">")))
    (eshell)))

(defun wacs-shell ()
  "Open a new shell in the main project directory."
  (let ((default-directory (wacs-project-dir)))
    (shell (concat "*shell*<" (wacs-project-name) ">"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions and macros ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wacs--eval-aux-cond (aux-cond)
  "Evaluate AUX-COND.

If passed a symbol, evaluate the symbol as a variable.  If passed
an inline lambda, funcall the lambda.  If passed a (:var VAR)
pair, evaluate VAR as a variable.  If passed a (:fn FN) pair,
funcall FN."
  (if (consp aux-cond)
      (let ((cond-val (cadr aux-cond)))
        (cl-case (car aux-cond)
          (:fn (funcall cond-val))
          (:var (when (boundp cond-val)
                  (symbol-value cond-val)))
          (t (funcall aux-cond))))
    (when (boundp aux-cond)
      (symbol-value aux-cond))))

(defun wacs--switch-to-window-with-buffer (buffer)
  "Switch to the window with BUFFER."
  (-each-while (window-list)
               (lambda (_) (not (equal (window-buffer) buffer)))
               (lambda (_) (other-window 1))))

(defun wacs--list->dotted-pair (list)
  "Change the current LIST pair and sub-list pair into dotted pairs."
  (let ((snd (cadr list)))
    (if (and (listp snd) (= (length snd) 2))
        (cons (car list) (wacs--list->dotted-pair snd))
      (cons (car list) (cadr list)))))

(defmacro wacs--alist-delete (key alist)
  "Delete KEY from alist ALIST."
  `(setq ,alist
         (cl-remove-if (lambda (entry)
                         (equal (car entry) ,key))
                       ,alist)))

(defmacro wacs--alist-put (key val alist)
  "Push (KEY . VAL) into alist ALIST.

If KEY already exists as a key in ALIST, delete the entry."
  `(progn (wacs--alist-delete ,key ,alist)
          (push (cons ,key ,val) ,alist)))

(defun wacs--alist-get (key alist)
  "Get element associated with KEY from ALIST."
  (cdr (assoc key alist)))

(defun wacs--u-prefix? (arg)
  "Test whether ARG is universal prefix argument."
  (equal arg '(4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation and Highlighting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'wacs--with-property 'lisp-indent-function 1)
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<def\\(wacspace\\|winconf\\|wacsalias\\(es\\)?\\)\\>" .
    'font-lock-keyword-face)))

;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;

(defun wacs--resolve-prefix (config arg)
  "Return the final configuration from CONFIG with prefix ARG."
  (let ((arg-key (if arg
                     (intern (concat ":" (number-to-string arg)))
                   :default)))
    (append (--filter (not (memq (car it) wacs--numeric-confs))
                      config)
            (wacs--alist-get arg-key config)
            (wacs--alist-get arg-key
                             (wacs--alist-get :default
                                              (get :default
                                                   'wacs-config))))))

(defun wacs--get-cond-config-from-alist (config-alist)
  "Get the first first configuration with a satisfied auxiliary condition from CONFIG-ALIST."
  (cl-dolist (aux-cond-pair config-alist)
    (unless (eq (car aux-cond-pair) :default)
      (when (wacs--eval-aux-cond (car aux-cond-pair))
        (cl-return (cdr aux-cond-pair))))))

(defun wacs--get-default-config-from-alist (config-alist)
  "Get the :default configuration for the current major mode from CONFIG-ALIST."
  (wacs--alist-get :default config-alist))

(defun wacs--get-aliased-config (entry)
  "Get the configuration pointed to by alias entry ENTRY."
  (wacs--alist-get (or (cdr entry) :default)
                   (get (car entry) 'wacs-config)))

(cl-defun wacs--get-config (&optional arg)
  "Get the config with prefix ARG associated with the current buffer.

First, search for a wacspace configuration with a satisfactory
auxiliary condition. Then, search for an alias with a
satisfactory auxiliary condition. Then, search for a
configuration without an auxiliary condition. Then, search for an
alias without an auxiliary condition. Then, search for the
default configuration with an auxiliary condition. Then, search
for the default configuration. Then give up. Whew."
  (let (mode-config mode-alias global-default-config)
    (let ((config
           (cl-block find-config
             (setq mode-config (get major-mode 'wacs-config))
             (-when-let (cond-config (wacs--get-cond-config-from-alist
                                      mode-config))
               (cl-return-from find-config cond-config))
             (setq mode-alias (get major-mode 'wacs-alias))
             (-when-let (cond-alias (wacs--get-cond-config-from-alist
                                     mode-alias))
               (cl-return-from find-config
                 (wacs--get-aliased-config cond-alias)))
             (-when-let (mode-default-config
                         (wacs--get-default-config-from-alist
                          mode-config))
               (cl-return-from find-config mode-default-config))
             (-when-let (mode-default-alias
                         (wacs--get-default-config-from-alist
                          mode-alias))
               (cl-return-from find-config
                 (wacs--get-aliased-config mode-default-alias)))
             (setq global-default-config
                   (get :default 'wacs-config))
             (-when-let (default-cond-config
                          (wacs--get-cond-config-from-alist
                           global-default-config))
               (cl-return-from find-config default-cond-config))
             (-when-let (default-config
                          (wacs--get-default-config-from-alist
                           global-default-config))
               (cl-return-from find-config default-config)))))
      (wacs--resolve-prefix config arg))))

(defun wacs--process-config (config)
  "Process CONFIG for inclusion in `wacs-config'."
  (let ((default-conf (-map 'wacs--list->dotted-pair
                            (wacs--alist-get :default config))))
    (unless default-conf
      (error
       "Please include a :default configuration for the wacspace"))
    (-map
     (lambda (entry)
       (cond ((eq (car entry) :default)
              (cons :default default-conf))
             ((memq (car entry) wacs--numeric-confs)
              (append (cons (car entry)
                            (-map 'wacs--list->dotted-pair (cdr entry)))
                      default-conf))
             (t (wacs--list->dotted-pair entry))))
     config)))

;;;###autoload
(defun wacs--push-config (mode aux-cond entry propname)
  "Given MODE and AUX-COND, push ENTRY with property name PROPNAME."
  (let ((aux-cond-key (or aux-cond :default))
        (mode-list (get mode propname)))
    (put mode propname
         (cons (cons aux-cond-key entry)
               (cl-delete-if (lambda (existing-pair)
                               (equal (car existing-pair) aux-cond-key))
                             mode-list)))
    t))

;;;###autoload
(cl-defmacro defwacspace ((mode &optional aux-cond) &body configuration)
  "Define a wacspace for a major mode and an optional auxiliary condition.

The auxiliary condition can be a variable (such as a minor mode),
an inline lambda, a (:fn FN) pair, or a (:var VAR) pair. For full
documentation of configuration options, see the README."
  (let ((entry (wacs--process-config configuration)))
    `(wacs--push-config ',mode ',aux-cond ',entry 'wacs-config)))

;;;###autoload
(cl-defmacro defwacsalias ((mode &optional aux-cond)
                           (other-mode &optional other-aux-cond))
  "Define a wacspace alias.

When wacspace is invoked with MODE and AUX-COND, it will run the
same way as it would for buffers in OTHER-MODE and
OTHER-AUX-COND."
  (let ((entry `(,other-mode . ,other-aux-cond)))
    `(wacs--push-config ',mode ',aux-cond ',entry 'wacs-alias)))

;;;###autoload
(cl-defmacro defwacsaliases ((&rest mode-pairs)
                             (other-mode &optional other-aux-cond))
  "Define multiple aliases for a mode.

MODE-PAIRS should be a list of (MODE AUX-COND) pairs. OTHER-MODE
and OTHER-AUX-COND are the mode and condition to alias to."
  (cons 'progn
        (append
         (-map (lambda (pair) `(defwacsalias ,pair
                            (,other-mode ,other-aux-cond)))
               mode-pairs))))

;;;###autoload
(cl-defmacro defwinconf (conf-name &body body)
  "Define a wacspace window configuration.

This is defined as a function (e.g. a sequence of window
splitting commands). The function need not stop with the original
window active."
  `(wacs--alist-put  ',conf-name
                     '(lambda () ,@body)
                     wacs--winconfs))

;;;###autoload
(defmacro wacs-set-frame-fn (frame fn)
  "Set the given FRAME parameter to FN.

FRAME and FN should be passed unquoted."
  `(wacs--alist-put ',frame ',fn wacs--frame-fns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wacs--run-winconf (conf-name)
  "Run winconf with name CONF-NAME."
  (delete-other-windows)
  (-if-let (winconf (wacs--alist-get conf-name wacs--winconfs))
    (let ((main-window (selected-window)))
      (funcall winconf)
      (select-window main-window)
      main-window)
    (error "No winconf with name: %s" conf-name)))

(defun wacs--set-frame (frame)
  "Set the frame using the function set for FRAME."
  (-if-let (frame-fn (wacs--alist-get frame wacs--frame-fns))
    (funcall frame-fn)
    (message "No frame fn specified for frame alignment %s" frame)))

(cl-defmacro wacs--with-property ((prop) &body body)
  "Helper macro for using properties within configurations."
  (let ((prop-keyword (intern (concat ":" (symbol-name prop)))))
    `(let ((,prop (wacs--alist-get ,prop-keyword config)))
       (when ,prop
         ,@body))))

(defun wacs--switch-to-buffer (buffer-string)
  "Switch to buffer with name BUFFER-STRING.

If `wacs-regexp-buffer-switching' is set to t, BUFFER-STRING is
interpreted as an unescaped regexp."
  (-if-let (buffer
            (car (--filter (string= buffer-string
                                    (buffer-name it))
                           (buffer-list))))
    (switch-to-buffer buffer)
    (if wacs-regexp-buffer-switching
        (-if-let (buffer
                  (car
                   (--filter
                    (string-match-p (regexp-quote buffer-string)
                                    (buffer-name it))
                    (buffer-list))))
          (switch-to-buffer buffer)
          (switch-to-buffer buffer-string))
      (switch-to-buffer buffer-string))))

(defun wacs--set-up-windows (config main-window)
  "Set up the windows according to the CONFIG.

MAIN-WINDOW is the window from which `wacspace' was called."
  (-each (-take (length (window-list))
                '(:main :aux1 :aux2 :aux3 :aux4 :aux5))
         (lambda (win-key)
           (-when-let (buffer-conf (wacs--alist-get win-key config))
             (select-window main-window)
             (other-window (string-to-number
                            (substring (symbol-name win-key) -1)))
             (cl-case (car buffer-conf)
               (:buffer (if (eq (cdr buffer-conf) :main)
                            (switch-to-buffer wacs-main-buffer)
                          (wacs--switch-to-buffer
                           (cdr buffer-conf))))
               (:cmd (funcall (cdr buffer-conf)))))
           (setq-local wacs-project-base-file
                       wacs-project-base-file)))
  (select-window main-window)
  (wacs--switch-to-window-with-buffer wacs-main-buffer))

(defun wacs--set-up-workspace (arg config)
  "Given ARG and CONFIG, set up the workspace."
  (let ((wacs-project-base-file
         (or (wacs--alist-get :base-file config)
             wacs-project-base-file
             (file-name-nondirectory (buffer-file-name))))
        (wacs--project-name-fn (wacs--alist-get :project-name-fn
                                                config)))
    (wacs--with-property (before)
      (save-window-excursion
        (funcall before)))
        (wacs--with-property (frame)
      (wacs--set-frame frame))
    (let ((main-window
           (wacs--with-property (winconf)
             (wacs--run-winconf winconf))))
      (wacs--set-up-windows config main-window))
    (wacs--with-property (after)
      (save-window-excursion
        (funcall after)))
    (wacs--with-property (after-switch)
      (puthash (wacs-project-name)
               after-switch
               wacs--after-switch-fns)
      (save-window-excursion
        (funcall after-switch)))
    (message "wacspace configured")))

(defun wacs--update-open-projects (buffer arg)
  "Update `wacs--open-projects' with BUFFER and ARG."
  (let ((project-name (wacs-project-name)))
    (wacs--alist-put project-name
                     (cons buffer arg)
                     wacs--open-projects)))

;;;###autoload
(defun wacspace (&optional arg)
  "Set up your Emacs workspace.

If there is a saved configuration with numeric prefix ARG,
restore that.  Otherwise, set up your workspace based on your
wacspace configuration.  If called with universal prefix
arg (C-u), force reconfiguration even if there is a saved
workspace."
  (interactive "P")
  (when (wacs--u-prefix? arg)
    (wacs-clear-saved (current-buffer))
    (setq arg nil))
  (unless (wacspace-restore arg)
    (-if-let* ((wacs-main-buffer (current-buffer))
               (config (wacs--get-config arg)))
      (progn (wacs--set-up-workspace arg config)
             (wacspace-save arg))
      (error
       "No wacspace configuration available for the current mode"))))

;;;###autoload
(defun wacspace-save (&optional arg)
  "Save the current window configuration with prefix ARG.

When wacspace is invoked in the future in any of the current
buffers with given prefix key, the current workspace will be
restored."
  (interactive "P")
  (let* ((config-symbol-alist
          (gethash (current-buffer)
                   wacs--saved-workspaces))
         (config (if wacs-save-frame
                     (current-frame-configuration)
                   (current-window-configuration)))
         (current-buffers (-map 'window-buffer (window-list)))
         (new-config-alist
          (wacs--alist-put (or arg :default)
                           (cons config current-buffers)
                           config-symbol-alist)))
    (--each (window-list)
      (puthash (window-buffer it)
               new-config-alist
               wacs--saved-workspaces))
    (wacs--update-open-projects (current-buffer) arg)
    (message "wacspace saved")))

;;;###autoload
(defun wacspace-restore (&optional arg)
  "Restore a window configuration saved with prefix key ARG.

Usually, you should call `wacspace' directly instead of this
function unless you want to skip the possibility of
configuration."
  (let ((buffer (current-buffer))
        (pos (point)))
    (ignore-errors
      (let* ((config (cadr
                      (assoc (or arg :default)
                             (gethash buffer
                                      wacs--saved-workspaces)))))
        (when config
          (if wacs-save-frame
              (set-frame-configuration config)
            (set-window-configuration config))
          (--each (window-list)
            (select-window it)
            (when (memq major-mode wacs-end-of-buffer-modes)
              (goto-char (point-max))))
          (wacs--switch-to-window-with-buffer buffer)
          (goto-char pos)
          (wacs--update-open-projects (current-buffer) arg)
          (message "wacspace restored")
          t)))))

(defun wacspace-switch-project ()
  "Quickly switch between open projects."
  (interactive)
  (if (null wacs--open-projects)
      (message "No open projects")
    (let* ((project-names (-map 'car wacs--open-projects))
           (project (completing-read "Project: " project-names
                                     nil t nil nil
                                     (if (= (length project-names) 1)
                                         (car project-names)
                                       (cadr project-names))))
           (config (wacs--alist-get project wacs--open-projects))
           (buffer (car config))
           (last-prefix (cdr config)))
      (set-buffer buffer)
      (wacspace last-prefix)
      (-when-let (after-switch (gethash project
                                         wacs--after-switch-fns))
        (funcall after-switch)))))

(defun wacs-clear-saved (&optional buffer)
  "Clear saved workspaces associated with BUFFER.

BUFFER can be a string or a buffer object.  If called
interactively, will clear saved workspaces associated with the
current buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (-each (gethash buffer wacs--saved-workspaces)
           (lambda (entry)
             (-each (cddr entry)
                    (lambda (buffer)
                      (remhash buffer wacs--saved-workspaces)))))))

(defun wacs-clear-all-saved ()
  "Clear all saved workspaces from this session."
  (interactive)
  (maphash (lambda (key _) (wacs-clear-saved key))
           wacs--saved-workspaces))

(defun wacs--kill-buffer-hook ()
  "Hook to clear saved associated workspaces when a buffer is killed."
  (wacs-clear-saved (current-buffer)))

(add-hook 'kill-buffer-hook 'wacs--kill-buffer-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun wacs-set-up-prefix ()
  "Set up variable `wacs-prefix-map' with wacspace commands."
  (define-prefix-command 'wacs-prefix-map)
  (define-key wacs-prefix-map (kbd "C-w") 'wacspace)
  (define-key wacs-prefix-map (kbd "w") 'wacspace)
  (define-key wacs-prefix-map (kbd "C-p") 'wacspace-switch-project)
  (define-key wacs-prefix-map (kbd "p") 'wacspace-switch-project)
  (define-key wacs-prefix-map (kbd "C-s") 'wacspace-save)
  (define-key wacs-prefix-map (kbd "s") 'wacspace-save)
  (--dotimes 9
    (let* ((n (+ it 1))
           (cmd
            `(lambda ()
               (interactive)
               (wacspace ,n))))
      (define-key wacs-prefix-map (kbd (concat "C-"
                                               (number-to-string n)))
        cmd)
      (define-key wacs-prefix-map (kbd (number-to-string n)) cmd))))

;;;###autoload
(defun wacs-set-up-keys ()
  "Set up C-z as a prefix with wacspace commands."
  (wacs-set-up-prefix)
  (global-set-key (kbd "C-z") 'wacs-prefix-map))

(defwinconf 1win)

(defwinconf 3winv
  (split-window-right)
  (other-window 1)
  (split-window-below))

(defwinconf 2winh
  (split-window-below))

(defwinconf 2winv
  (split-window-right))

(defwinconf 4win
  (split-window-right)
  (split-window-below)
  (other-window 2)
  (split-window-below))

(provide 'wacspace)

;;; wacspace.el ends here
