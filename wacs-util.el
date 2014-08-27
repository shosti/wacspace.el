;;; wacs-util.el --- wacspace utility functions and macros -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions and macros for wacspace.el.

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

(require 'dash)
(require 'cl-lib)

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
  (select-window (get-buffer-window buffer)))

(defconst wacs--boring-buffers
  '("Minibuf"
    "Echo Area"
    "code-conver[a-z]+-work"
    "\*Messages\*"
    "\*Backtrace\*")
  "Buffers where you don't care about the point (minibuffers and such).")

(defun wacs--interesting-buffers ()
  "Return buffers that are not in the boring list."
  (let ((-compare-fn (lambda (s r) (string-match r s))))
    (-filter (lambda (b)
               (not (-contains? wacs--boring-buffers
                                (buffer-name b))))
             (buffer-list))))

(defun wacs--buffer-point (buffer)
  "Get the point for BUFFER."
  (with-current-buffer buffer
    (point)))

(defun wacs--set-buffer-point (buffer position)
  "Set the point in BUFFER to POSITION."
  (-if-let (buffer-window
            (car (-filter (lambda (w)
                            (equal (window-buffer w) buffer))
                          (window-list))))
      (set-window-point buffer-window position)
    (with-current-buffer buffer
      (goto-char position))))

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

(defun wacs--to-cons (thing)
  "Make sure THING is a cons cell."
  (if (consp thing) thing (list thing)))

(provide 'wacs-util)

;;; wacs-util.el ends here
