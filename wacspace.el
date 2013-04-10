;;; wacspace.el --- The WACky WorkSPACE manager for emACS

;; Copyright © 2013 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>
;; URL: http://github.com/shosti/wacspace.el
;; Version: 0.1
;; Created: 26 March 2013
;; Keywords: workspace
;; Package-Requires: ((dash "1.1.0") (cl-lib "0.2"))

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

;; Private configuration

(defvar wacs--config nil)
(defvar wacs--winconfs nil)
(defvar wacs--frame-fns nil)
(defvar wacs--saved-workspaces (make-hash-table :test 'equal))
(defconst wacs--numeric-confs '(:default :1 :2 :3 :4 :5 :6 :7 :8 :9))

;; Helper functions and macros

(cl-defmacro wacs--when-let ((var value) &body body)
  (declare (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))

(cl-defmacro wacs--if-let ((var value) &body body)
  (declare (indent 1))
  `(let ((,var ,value))
     (if ,var ,@body)))

(defun wacs--eval-aux-cond (aux-cond)
  (cond ((listp aux-cond)
         (funcall aux-cond))
        ((boundp aux-cond)
         (symbol-value aux-cond))))

(defun wacs--switch-to-window-with-buffer (buffer)
  (-each-while (window-list)
               (lambda (_) (not (equal (window-buffer) buffer)))
               (lambda (_) (other-window 1))))

(defun wacs--list->dotted-pair (list)
  (let ((snd (cadr list)))
    (if (and (listp snd) (= (length snd) 2))
        (cons (car list) (wacs--list->dotted-pair snd))
      (cons (car list) (cadr list)))))

;; Indentation fixes

(put 'with-property 'lisp-indent-function 1)

;; Configuration

(defun wacs--resolve-config (config arg)
  (let ((arg-key (if arg
                     (intern (concat ":" (number-to-string arg)))
                   :default)))
    (append (--filter (not (memq (car it) wacs--numeric-confs))
                      config)
            (cdr (assq arg-key config)))))

(defun wacs--get-config (&optional arg)
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
  "Define a wacspace for a major mode and an optional auxiliary
condition. The auxiliary condition can either be a variable (such
as a minor mode) or an inline lambda. For full documentation of
configuration options, see the README."
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
  "Define a wacspace window configuration. This is defined as a
function (e.g. a sequence of window splitting commands). The
function need not stop with the original window active."
  `(push (cons ',conf-name
               '(lambda () ,@body))
         wacs--winconfs))

;;;###autoload
(defmacro wacs-set-frame-fn (frame fn)
  "Set the given frame parameter to the given function. Both
parameters should be passed unquoted."
  `(push (cons ',frame ',fn) wacs--frame-fns))

;; Interactive functions

(defun wacs--run-winconf (conf-name)
  (delete-other-windows)
  (wacs--if-let (winconf (cdr (assoc conf-name wacs--winconfs)))
    (let ((main-window (selected-window)))
      (funcall winconf)
      (select-window main-window)
      main-window)
    (error "No winconf with name: %s" conf-name)))

(defun wacs--set-frame (frame)
  (wacs--if-let (frame-fn (cdr (assq frame wacs--frame-fns)))
    (funcall frame-fn)
    (message "No frame fn specified for frame alignment %s" frame)))

(cl-defmacro wacs--with-property ((prop) &body body)
  (let ((prop-keyword (intern (concat ":" (symbol-name prop)))))
    `(let ((,prop (cdr (assoc ,prop-keyword config))))
       (when ,prop
         ,@body))))

(defun wacs--set-up-windows (config main-buffer main-window)
  (-each (-take (length (window-list))
                '(:main :aux1 :aux2 :aux3 :aux4 :aux5))
         (lambda (win-key)
           (wacs--when-let (buffer-conf (cdr (assq win-key config)))
             (select-window main-window)
             (other-window (string-to-number
                            (substring (symbol-name win-key) -1)))
             (cl-case (car buffer-conf)
               (:buffer (switch-to-buffer
                         (if (eq (cdr buffer-conf) :main)
                             main-buffer
                           (cdr buffer-conf))))
               (:cmd (funcall (cdr buffer-conf)))))))
  (select-window main-window)
  (wacs--switch-to-window-with-buffer main-buffer))

;;;###autoload
(defun wacspace (&optional arg)
  "Set up your Emacs workspace. First, wacspace will try to
restore a window configuration saved with prefix ARG. If that
doesn't work, wacspace will set up your workspace based on your
configuration."
  (interactive "P")
  (unless (wacspace-restore arg)
    (let ((config (wacs--get-config arg))
          (main-buffer (current-buffer)))
      (if config
          (progn
            (wacs--with-property (before)
                                 (save-window-excursion
                                   (funcall before)))
            (wacs--with-property (frame)
                                 (wacs--set-frame frame))
            (let ((main-window
                   (wacs--with-property (winconf)
                                        (wacs--run-winconf winconf))))
              (wacs--set-up-windows config main-buffer main-window))
            (wacs--with-property (after)
                                 (save-window-excursion
                                   (funcall after)))
            (wacspace-save arg))
        (message
         "No wacspace configuration available for the current mode.")))))

;;;###autoload
(defun wacspace-save (&optional arg)
  "Save the current window configuration with given ARG as a
prefix key. When wacspace is invoked in the future in any of the
current buffers with given prefix key, the current workspace will
be restored."
  (interactive "P")
  (let* ((config-symbol-alist
          (gethash (current-buffer)
                   wacs--saved-workspaces))
         (workspace-symbol (cl-gensym))
         (frame (cdr (assq :frame (wacs--get-config arg))))
         (new-config-alist
          (push (cons (or arg :default) (cons workspace-symbol frame))
                config-symbol-alist)))
    (--each (window-list)
      (puthash (window-buffer it)
               new-config-alist
               wacs--saved-workspaces))
    (window-configuration-to-register workspace-symbol)))

;;;###autoload
(defun wacspace-restore (&optional arg)
  "Restore a window configuration saved with prefix key ARG.
Usually, you should call wacspace directly instead of this
function unless you want to skip the possibility of
configuration."
  (let ((buffer (current-buffer))
        (pos (point)))
    (ignore-errors
      (let* ((conf (cdr
                    (assoc (or arg :default)
                           (gethash buffer
                                    wacs--saved-workspaces))))
             (register-sym (car conf))
             (frame (cdr conf)))
        (when register-sym
          (jump-to-register register-sym)
          (wacs--switch-to-window-with-buffer buffer)
          (goto-char pos)
          (wacs--set-frame frame)
          t)))))

;; Standard configuration

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
