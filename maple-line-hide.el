;;; maple-line-hide.el --- hide modeline configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/honmaple/emacs-maple-line

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; hide modeline configurations.
;;

;;; Code:
(require 'face-remap)

(defgroup maple-line-hide nil
  "Hide modeline mode."
  :group 'maple)

(defcustom maple-line-hide-underline t
  "Whether hide mode-line but show underline."
  :group 'maple-line-hide
  :type 'boolean)

(defcustom maple-line-hide-underline-below nil
  "Whether show below window's underline."
  :group 'maple-line-hide
  :type 'boolean)

(defcustom maple-line-hide-underline-color t
  "Hide mode-line but show underline color."
  :group 'maple-line-hide
  :type 'string)

(defcustom maple-line-hide-ignore '("^magit")
  "Exclude some buffer don't hide mode-line."
  :group 'maple-line-hide
  :type 'list)

(defvar-local maple-line-hide-alist nil)
(defvar-local maple-line-hide-format nil)

(defun maple-line-hide-ignore-p()
  "Ignore some buffer or major mode."
  (or (member major-mode maple-line-hide-ignore)
      (member (buffer-name) maple-line-hide-ignore)
      (catch 'ignored
        (dolist (regex maple-line-hide-ignore)
          (when (and (stringp regex) (string-match regex (buffer-name)))
            (throw 'ignored t))))))

(defun maple-line-hide-ignore-underline-p(&optional window)
  "Ignore current WINDOW's underline."
  (not (when maple-line-hide-underline (if maple-line-hide-underline-below t (window-in-direction 'below window)))))

;;;###autoload
(define-minor-mode maple-line-hide-mode
  "maple line mode"
  :group      'maple-line
  (if maple-line-hide-mode
      (let* ((face (list :box nil :height 0.1 :underline maple-line-hide-underline-color)))
        (setq maple-line-hide-alist (list (face-remap-add-relative 'mode-line face)
                                          (face-remap-add-relative 'mode-line-inactive face)))
        (setq maple-line-hide-format mode-line-format
              mode-line-format (unless (maple-line-hide-ignore-underline-p) " "))
        (add-hook 'window-configuration-change-hook 'maple-line-hide-set-window-underline))
    (setq mode-line-format maple-line-hide-format
          maple-line-hide-format nil)
    (dolist (face-remap maple-line-hide-alist)
      (face-remap-remove-relative face-remap))
    (setq maple-line-hide-alist nil)
    (remove-hook 'window-configuration-change-hook 'maple-line-hide-set-window-underline)))

(defun maple-line-hide-set-window-underline()
  "Ignore some buffer or major mode."
  (dolist (window (window-list))
    (with-selected-window window
      (when maple-line-hide-mode
        (setq mode-line-format (unless (maple-line-hide-ignore-underline-p) " "))))))

(define-globalized-minor-mode global-maple-line-hide-mode maple-line-hide-mode
  (lambda() (unless (maple-line-hide-ignore-p) (maple-line-hide-mode 1))))

(provide 'maple-line-hide)
;;; maple-line-hide.el ends here
