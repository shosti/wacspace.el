;;; wacs-helper.el --- Helper methods for wacspace -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Useful helper functions for setting up wacspace configurations

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

(require 'wacs-util)
(require 'wacs-configuration)
(require 'dash)

;;;###autoload
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

;;;###autoload
(defun wacs-eshell ()
  "Open an eshell in the main project directory."
  (let ((default-directory (wacs-project-dir))
        (eshell-buffer-name (concat "*eshell*<"
                                    (wacs-project-name)
                                    ">")))
    (eshell)))

;;;###autoload
(defun wacs-shell ()
  "Open a new shell in the main project directory."
  (let ((default-directory (wacs-project-dir)))
    (shell (concat "*shell*<" (wacs-project-name) ">"))))

(provide 'wacs-helper)

;;; wacs-helper.el ends here
