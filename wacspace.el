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

(defgroup wacspace nil
  "The WACky WorkSPACE manager for emACS"
  :prefix "wacs-"
  :group 'environment)

(defcustom wacs-regexp-buffer-switching t
  "Use regexp matching for buffer switching in `wacspace'.

When set to t, :buffer option will use a regexp match if a
buffer does not exist with the exact match."
  :group 'wacspace
  :type 'boolean)

(defcustom wacs-save-frame (display-graphic-p)
  "Save frame with `wacspace'.

When set to t, wacspace will save the frame configuration as
well as the window configuration.  Set to t by default in graphic
display and nil if Emacs is run in a terminal."
  :group 'wacspace
  :type 'boolean)

(defcustom wacs-project-base-file ".git"
  "Default base file name in projects.

Wacspace will assume that project base directories have this
filename in them.  This variable be dynamically bound within
helper functions.  When set to nil, wacspace will assume that the
current directory is the base directory."
  :group 'wacspace
  :type 'string)

(defcustom wacs-end-of-buffer-modes '(eshell-mode shell-mode comint-mode)
  "Modes in which to scroll to the end of buffers.

Major modes where wacspace will scroll to the end of the
buffer after restoring or setting up."
  :group 'wacspace
  :type 'sexp)

(require 'wacs-util)
(require 'wacs-configuration)
(require 'wacs-interactive)
(require 'wacs-helper)
(require 'wacs-init)

(provide 'wacspace)

;;; wacspace.el ends here
