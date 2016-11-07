;;; html-element-edit-attributes.el

;; Copyright 2016 Julian T. Knabenschuh

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

(require 'web-mode)

(defun html-element-edit-attributes-end-inside ()
  (interactive)
  (web-mode-attribute-end)
  (backward-char))

(defun html-element-edit-attributes-transpose-backward ()
  (interactive)
  (save-excursion
    (web-mode-attribute-beginning)
    (web-mode-attribute-previous)
    (web-mode-attribute-transpose)))

(provide 'html-element-edit-attributes)
;;; html-element-edit-attributes.el ends here
