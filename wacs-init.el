;;; wacs-init.el --- wacspace initialization -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initialization and defaults for wacspace.el.

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

(require 'wacs-configuration)
(require 'dash)

(defvar wacs-prefix-map
  "The prefix keymap for wacspace commands.

Bound to C-z by default.")

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

(provide 'wacs-init)

;;; wacs-init.el ends here
