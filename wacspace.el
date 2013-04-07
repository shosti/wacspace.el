;;; wacspace.el --- The WACky WorkSPACE manager for emACS -*- lexical-binding: t -*-

;; Copyright © 2013 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>
;; URL: http://github.com/shosti/wacspace.el
;; Version: 0.0
;; Created: 26 March 2013
;; Keywords: workspace
;; Package-Requires: ((dash "1.1.0"))

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

(defun wacs--select-main-window ()
  (select-window (window-at 1 1))) ;rather hackey--better way?

;; Indentation fixes

(put 'with-property 'lisp-indent-function 1)

;; Configuration

(defun wacs--get-config (&optional arg)
  (defun resolve-config (config)
    (let ((arg-key (if arg
                       (intern (concat ":" (number-to-string arg)))
                     :default)))
      (append (--filter (not (memq (car it) wacs--numeric-confs))
                        config)
              (cdr (assq arg-key config)))))

  (let* ((mode-config-list (cdr (assoc major-mode wacs--config)))
         (config
          (cl-dolist (aux-cond-pair mode-config-list)
            (unless (eq (car aux-cond-pair) :default)
              (when (wacs--eval-aux-cond (car aux-cond-pair))
                (cl-return (cdr aux-cond-pair)))))))
    (resolve-config (if config config
                      (cdr (assq :default mode-config-list))))))

;;;###autoload
(cl-defmacro defwacspace ((mode &optional aux-cond) &body configuration)
  "Define a wacspace for a major mode and an optional auxiliary
condition. The auxiliary condition can either be a variable (such
as a minor mode) or an inline lambda. For full documentation of
configuration options, see the README."
  (defun list->dotted-pair (list)
    (let ((snd (cadr list)))
      (if (and (listp snd) (= (length snd) 2))
          (cons (car list) (list->dotted-pair snd))
        (cons (car list) (cadr list)))))

  (defun process-config (config)
    (let ((default-conf (-map 'list->dotted-pair
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
                              (-map 'list->dotted-pair (cdr entry)))
                        default-conf))
               (t (list->dotted-pair entry))))
       config)))

  (let* ((config (process-config configuration))
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
paramaters should be passed unquoted."
  `(push (cons ',frame ',fn) wacs--frame-fns))

;; Interactive functions

(defun wacs--run-winconf (conf-name)
  (delete-other-windows)
  (funcall (cdr (assoc conf-name wacs--winconfs)))
  (wacs--select-main-window))

(defun wacs--set-frame (frame)
  (wacs--when-let (frame-fn (cdr (assq frame wacs--frame-fns)))
    (funcall frame-fn)))

;;;###autoload
(defun wacspace (&optional arg)
  "Set up your Emacs workspace based on your wacspace configuration."
  (interactive "P")

  (cl-defmacro with-property ((prop) &body body)
    (let ((prop-keyword (intern (concat ":" (symbol-name prop)))))
      `(let ((,prop (cdr (assoc ,prop-keyword config))))
         (when ,prop
           ,@body
           (select-window main-window)))))

  (defun set-up-windows (config main-buffer)
    (-each (-take (length (window-list))
                  '(:main :aux1 :aux2 :aux3 :aux4 :aux5))
           (lambda (win-key)
             (wacs--when-let (buffer-conf (cdr (assq win-key config)))
               (wacs--select-main-window)
               (other-window (string-to-number
                              (substring (symbol-name win-key) -1)))
               (cl-case (car buffer-conf)
                 (:buffer (switch-to-buffer
                           (if (eq (cdr buffer-conf) :main)
                               main-buffer
                             (cdr buffer-conf))))
                 (:cmd (funcall (cdr buffer-conf)))))))
    (--each (window-list)
      (when (equal (window-buffer it)
                   main-buffer)
        (select-window it))))

  (let ((config (wacs--get-config arg))
        (main-window (selected-window))
        (main-buffer (current-buffer)))
    (unless nil
      (unless config
        (message
         "No wacspace configuration available for the current mode."))
      (with-property (frame)
        (wacs--set-frame frame))
      (with-property (before)
        (funcall before))
      (with-property (winconf)
        (wacs--run-winconf winconf))
      (set-up-windows config main-buffer)
      (with-property (after)
        (funcall after))
      ;(wacspace-save arg)
      )))

(defun wacspace-save (&optional arg)
  (interactive "P")
  (let* ((config-symbol-alist
          (gethash (current-buffer)
                   wacs--saved-workspaces))
         (workspace-symbol
          (intern
           (concat ":"
                   (apply
                    'concat
                    (-interpose "-"
                                (--map (buffer-name
                                        (window-buffer it))
                                       (window-list))))
                   (when arg (concat "-" (number-to-string arg))))))
         (new-config-alist
          (push (cons (or arg :default) workspace-symbol)
                config-symbol-alist)))
    (--each (window-list)
      (puthash (window-buffer it)
               new-config-alist
               wacs--saved-workspaces))
    (window-configuration-to-register workspace-symbol)))

(defun wacspace-restore (&optional arg)
  (wacs--when-let (register-sym
                   (cdr
                    (assoc (or arg :default)
                           (gethash (current-buffer)
                                    wacs--saved-workspaces))))
    (save-excursion
      (jump-to-register register-sym))
    t))

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
