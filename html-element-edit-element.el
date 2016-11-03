;;; html-element-edit-elements.el

;; Copyright 2016 Julian T. Knabenschuh

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;; Code:

(require 'web-mode)

;; General
(defun html-element-edit-elements-end-inside ()
  (interactive)
  (web-mode-element-end)
  (backward-char))

;; Insert
(defun html-element-edit-elements-direct-before-insert (content)
  (interactive "sContent: ")
  (save-excursion
    (web-mode-tag-beginning)
    (insert content)))

(defun html-element-edit-elements-before-insert (content)
  (interactive "sContent: ")
  (save-excursion
    (web-mode-element-beginning)
    (web-mode-tag-previous)
    (html-element-edit-elements-direct-after-insert content)))

(defun html-element-edit-elements-direct-after-insert (content)
  (interactive "sContent: ")
  (save-excursion
    (web-mode-tag-end)
    (insert content)))

(defun html-element-edit-elements-after-insert (content)
  (interactive "sContent: ")
  (save-excursion
    (html-element-edit-elements-end-inside)
    (web-mode-tag-next)
    (html-element-edit-elements-direct-before-insert content)))

(provide 'html-element-edit-elements)
;;; html-element-edit-elements.el ends here
