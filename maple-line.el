;;; maple-line.el ---  maple line configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
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
;; maple line configuration.
;;

;;; Code:
(require 'subr-x)
(require 'maple-line-hide)

(defvar maple-line-name "*maple-line*")
(defvar maple-line-format nil)

(defgroup maple-line nil
  "Display maple line in window side."
  :group 'maple)

(defcustom maple-line-display-action '(maple-line-display-buffer)
  "Display buffer action."
  :type 'list
  :group 'maple-line)

(defcustom maple-line-display-alist '((side . bottom)
                                      (slot . 0)
                                      (window-height . 1))
  "Used by `display-buffer-in-side-window`."
  :type 'alist
  :group 'maple-line)

(defmacro maple-line-with-window (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(when-let ((window (maple-line-window)))
     (with-selected-window window ,@body)))

(defmacro maple-line-with-buffer (&rest body)
  "Execute the forms in BODY with buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer (get-buffer-create maple-line-name)))
     (with-current-buffer buffer ,@body)))

(defun maple-line-buffer-configure ()
  "Buffer configuration."
  (maple-line-with-buffer
    (display-buffer maple-line-name maple-line-display-action)
    (setq truncate-lines nil)))

(defun maple-line-window-configure ()
  "Window configuration."
  (maple-line-with-window
    (let ((window (selected-window)))
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-other-window t)
      (set-window-scroll-bars window 0 nil 0 nil)

      (set-window-fringes window 0 1)
      (set-window-margins window 0 0)
      (setq window-size-fixed 'height))))

(defun maple-line-display-buffer (buffer _alist)
  "Display BUFFER _ALIST."
  (display-buffer-in-side-window buffer maple-line-display-alist))

(defun maple-line-window ()
  "Whether show maple line."
  (get-buffer-window maple-line-name t))

(defun maple-line-refresh ()
  "Refresh maple line."
  (save-excursion
    (read-only-mode -1)
    (erase-buffer)
    (insert (format-mode-line maple-line-format))
    (read-only-mode 1)))

(defun maple-line-mode-off ()
  "Hide maple line."
  (interactive)
  (when-let ((window (maple-line-window)))
    (delete-window window))
  (global-maple-line-hide-mode -1)
  (remove-hook 'post-command-hook 'maple-line-update))

(defun maple-line-mode-on ()
  "Show maple line."
  (interactive)
  (setq maple-line-format mode-line-format)
  (global-maple-line-hide-mode 1)
  (maple-line-with-buffer
    (maple-line-buffer-configure)
    (maple-line-refresh)
    (maple-line-window-configure))
  (add-hook 'post-command-hook 'maple-line-update))

(defun maple-line-update (&rest _args)
  "Update maple line."
  (when (maple-line-window)
    (maple-line-with-buffer
      (maple-line-refresh)
      (maple-line-window-configure))))

;;;###autoload
(define-minor-mode maple-line-mode
  "maple line mode"
  :group      'maple-line
  :global     t
  (if maple-line-mode (maple-line-mode-on) (maple-line-mode-off)))

(add-to-list 'maple-line-hide-ignore maple-line-name)

(provide 'maple-line)
;;; maple-line.el ends here
