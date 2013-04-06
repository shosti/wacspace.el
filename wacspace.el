;;; wacspace.el --- emACS WorkSPACE -*- lexical-binding: t -*-

;; Copyright © 2013 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>
;; Version: 0.1
;; Created: 26 March 2013
;; Keywords: workspace

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides context-aware workspace management for Emacs.

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

;; Public configuration options

(defvar wacs-fill-screen-fn 'display-fill-screen
  "Function to fill screen.")
(defvar wacs-left-screen-fn nil
  "Function to fill the left half of the screen.")
(defvar wacs-right-screen-fn nil
  "Function to fill the right half of the screen.")
(defvar wacs-top-screen-fn nil
  "Function to fill the top of the screen.")
(defvar wacs-bottom-screen-fn nil
  "Function to fill the bottom of the screen.")

;; Private configuration

(defvar wacs--config nil)
(defvar wacs--winconfs nil)
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

;; Indentation fix
(put 'with-property 'lisp-indent-function 1)

(defun wacs--eval-aux-cond (aux-cond)
  (cond ((listp aux-cond)
         (funcall aux-cond))
        ((boundp aux-cond)
         (eval aux-cond))))

;; Configuration

(defun wacs--get-config (&optional arg)
  (defun resolve-config (config)
    (let ((arg-key (if arg
                       (intern (concat ":" arg))
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

(cl-defmacro defwacspace ((mode &optional aux-cond) &body configuration)
  "Define a wacspace for a major mode and an optional auxiliary
condition. The auxiliary condition can either be a variable (such
as a minor mode) or a lambda. For full documentation of
configuration options, see the README."
  (defun list->dotted-pair (list)
    (cons (car list) (cadr list)))

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

(cl-defmacro defwinconf ((conf-name) &body body)
  `(push (cons ',conf-name
               '(lambda () ,@body))
         wacs--winconfs))

;; Interactive functions

(defun wacs--run-winconf (conf-name)
  (delete-other-windows)
  (funcall (cdr (assoc conf-name wacs--winconfs))))

(defmacro wacs--set-frame (frame)
  (let ((frame-fn
         (intern (concat "wacs-" (symbol-name frame) "-screen-fn"))))
    `(funcall ,frame-fn)))

(defun wacspace (&optional arg)
  (interactive "P")

  (cl-defmacro with-property ((prop) &body body)
    (let ((prop-keyword (intern (concat ":" (symbol-name prop)))))
      `(let ((,prop (cdr (assoc ,prop-keyword config))))
         (when ,prop
           ,@body
           (select-window main-window)))))

  (let ((config (wacs--get-config arg))
        (main-window (car (window-list))))
    (unless config
      (message "No wacspace configuration available for the current mode."))
    (with-property (frame)
      (wacs--set-frame frame))
    (with-property (winconf)
      (wacs--run-winconf winconf))))

;; Standard configuration

(defwinconf (3winv)
  (split-window-right)
  (other-window 1)
  (split-window-below))

(provide 'wacspace)

;;; wacspace.el ends here
