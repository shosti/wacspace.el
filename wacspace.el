;;; wacspace.el --- The WACky WorkSPACE manager for emACS

;; Copyright © 2013 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>
;; URL: http://github.com/shosti/wacspace.el
;; Version: 0.3
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
(require 'eshell)

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
well as the window configuration. Set to t by default in graphic
display and nil if Emacs is run in a terminal.")

(defvar wacs-main-buffer nil
  "The buffer from which wacspace was called.
Should not be set directly; will be automatically bound when
wacspace is called.")

(defvar wacs-project-base-file ".git"
  "Base file name in projects.
Wacspace will assume that project base directories have this
filename in them. This variable be dynamically bound within
helper functions. When set to nil, wacspace will assume that the
current directory is the base directory.")

(defvar wacs-end-of-buffer-modes '(eshell-mode shell-mode comint-mode)
  "Modes in which to scroll to the end of buffers.
Major modes where wacspace will scroll to the end of the
buffer after restoring or setting up.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wacs-project-dir ()
  "Return the project directory of `wacs-main-buffer'."
  (let ((fname (file-name-directory
                (buffer-file-name wacs-main-buffer))))
    (expand-file-name
     (if wacs-project-base-file
         (locate-dominating-file fname
                                 wacs-project-base-file)
       (file-name-directory fname)))))

(defun wacs-project-name ()
  "Return the name of the current project."
  (-> (wacs-project-dir)
    (split-string "/" t)
    (last)
    (car)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Because they're used within autoloaded macros, wacs--config and
;;wacs--frame-fns need to be autoloaded

;;;###autoload
(defvar wacs--config nil
  "The wacspace configuration alist.
Should not be altered manually—use `defwacspace' instead.")

(defvar wacs--winconfs nil
  "The wacspace winconf alist.
Should not be altered manually—use `defwinconf' instead.")

;;;###autoload
(defvar wacs--frame-fns nil
  "The wacspace frame function alist.
Should not be altered manually—use `wacs-set-frame-fn' instead.")

(defvar wacs--saved-workspaces (make-hash-table :test 'equal)
  "The hash of saved workspaces.
Should not be altered manually—use `wacspace-save' insetad.")

(defconst wacs--numeric-confs '(:default :1 :2 :3 :4 :5 :6 :7 :8 :9)
  "The numeric prefix configurations available to `wacspace'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions and macros ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wacs--eval-aux-cond (aux-cond)
  "Evaluate AUX-COND.
If passed a symbol, evaluate the symbol as a variable. If passed
an inline lambda, funcall the lambda. If passed a (:var VAR)
pair, evaluate VAR as a variable. If passed a (:fn FN) pair,
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

(defun wacs--alist-delete (key alist)
  "Delete KEY from alist ALIST."
  (cl-delete-if (lambda (entry)
                  (equal (car entry) key))
                alist))

(defun wacs--alist-put (key val alist)
  "Push (KEY . VAL) into alist ALIST.
If KEY already exists as a key in ALIST, delete the entry."
  (wacs--alist-delete key alist)
  (push (cons key val) alist))

(defun wacs--u-prefix? (arg)
  "Test whether ARG is universal prefix argument."
  (equal arg '(4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation and Highlighting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'wacs--with-property 'lisp-indent-function 1)
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<def\\(wacspace\\|winconf\\)\\>" . 'font-lock-keyword-face)))

;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;

(defun wacs--resolve-config (config arg)
  "Resolve CONFIG with prefix ARG."
  (let ((arg-key (if arg
                     (intern (concat ":" (number-to-string arg)))
                   :default)))
    (append (--filter (not (memq (car it) wacs--numeric-confs))
                      config)
            (cdr (assq arg-key config)))))

(defun wacs--get-config (&optional arg)
  "Get the configuration with prefix ARG associated with the current buffer."
  (let* ((mode-config-list (cdr (assoc major-mode wacs--config)))
         (config
          (cl-dolist (aux-cond-pair mode-config-list)
            (unless (eq (car aux-cond-pair) :default)
              (when (wacs--eval-aux-cond (car aux-cond-pair))
                (cl-return (cdr aux-cond-pair)))))))
    (wacs--resolve-config (if config config
                            (cdr (assq :default mode-config-list)))
                          arg)))

(defun wacs--process-config (config)
  "Process CONFIG for inclusion in `wacs--config'."
  (let ((default-conf (-map 'wacs--list->dotted-pair
                            (cdr (assq :default config)))))
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
(cl-defmacro defwacspace ((mode &optional aux-cond) &body configuration)
  "Define a wacspace for a major mode and an optional auxiliary condition.
The auxiliary condition can be a variable (such as a minor mode),
an inline lambda, a (:fn FN) pair, or a (:var VAR) pair. For full
documentation of configuration options, see the README."
  (let* ((config (wacs--process-config configuration))
         (aux-cond-key (or aux-cond :default))
         (config-entry `(,aux-cond-key . ,config))
         (mode-list-pair-var (cl-gensym)))
    `(let ((,mode-list-pair-var (assq ',mode wacs--config)))
       (if ,mode-list-pair-var
           (push ',config-entry
                 (cdr ,mode-list-pair-var))
         (push (cons ',mode (list ',config-entry))
               wacs--config))
       t)))

;;;###autoload
(cl-defmacro defwinconf (conf-name &body body)
  "Define a wacspace window configuration.
This is defined as a function (e.g. a sequence of window
splitting commands). The function need not stop with the original
window active."
  `(push (cons ',conf-name
               '(lambda () ,@body))
         wacs--winconfs))

;;;###autoload
(defmacro wacs-set-frame-fn (frame fn)
  "Set the given FRAME parameter to FN.
FRAME and FN should be passed unquoted."
  `(push (cons ',frame ',fn) wacs--frame-fns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wacs--run-winconf (conf-name)
  "Run winconf with name CONF-NAME."
  (delete-other-windows)
  (-if-let (winconf (cdr (assoc conf-name wacs--winconfs)))
    (let ((main-window (selected-window)))
      (funcall winconf)
      (select-window main-window)
      main-window)
    (error "No winconf with name: %s" conf-name)))

(defun wacs--set-frame (frame)
  "Set the frame using the function set for FRAME."
  (-if-let (frame-fn (cdr (assq frame wacs--frame-fns)))
    (funcall frame-fn)
    (message "No frame fn specified for frame alignment %s" frame)))

(cl-defmacro wacs--with-property ((prop) &body body)
  "Helper macro for using properties within configurations."
  (let ((prop-keyword (intern (concat ":" (symbol-name prop)))))
    `(let ((,prop (cdr (assoc ,prop-keyword config))))
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
           (-when-let (buffer-conf (cdr (assq win-key config)))
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
  "Given CONFIG, set up the workspace."
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
  (message "wacspace configured")
  (wacspace-save arg))

;;;###autoload
(defun wacspace (&optional arg)
  "Set up your Emacs workspace.
If there is a saved configuration with numeric prefix ARG,
restore that. Otherwise, set up your workspace based on your
wacspace configuration. If called with universal prefix
arg (C-u), force reconfiguration even if there is a saved
workspace."
  (interactive "P")
  (when (wacs--u-prefix? arg)
    (wacs-clear-saved (current-buffer))
    (setq arg nil))
  (unless (wacspace-restore arg)
    (-if-let* ((wacs-main-buffer (current-buffer))
               (config (wacs--get-config arg))
               (wacs-project-base-file
                (or (cdr (assoc :base-file config))
                    wacs-project-base-file
                    (file-name-nondirectory (buffer-file-name)))))
      (wacs--set-up-workspace arg config)
      (message
       "No wacspace configuration available for the current mode."))))

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
    (message "wacspace saved")))

;;;###autoload
(defun wacspace-restore (&optional arg)
  "Restore a window configuration saved with prefix key ARG.
Usually, you should call wacspace directly instead of this
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
          (message "wacspace restored")
          t)))))

(defun wacs-clear-saved (&optional buffer)
  "Clear saved workspaces associated with BUFFER.
BUFFER can be a string or a buffer object. If called
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
