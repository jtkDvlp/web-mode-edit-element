;;; html-element-edit-utils.el

;; Copyright 2016 Julian T. Knabenschuh

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

(defun html-element-edit-utils-x-position (fx)
  (save-excursion
    (funcall fx)
    (point)))

(defun html-element-edit-utils-fnil (val f)
  (if val val
    (funcall f)))

(defun html-element-edit-utils-kill-region (&optional begin end)
  (let ((begin (html-element-edit-utils-fnil begin 'region-beginning))
        (end (html-element-edit-utils-fnil end 'region-end)))
    (let ((content (buffer-substring begin end)))
      (delete-region begin end)
      content)))

(provide 'html-element-edit-utils)
;;; html-element-edit-utils.el ends here
