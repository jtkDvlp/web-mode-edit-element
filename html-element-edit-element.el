;;; html-element-edit-elements.el

;; Copyright 2016 Julian T. Knabenschuh

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

(require 'web-mode)
(require 'html-element-edit-utils)

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

;; Sibling
(defun html-element-edit-elements-sibling-previous-position ()
  (html-element-edit-utils-x-position
   'web-mode-element-sibling-previous))

(defun html-element-edit-elements-sibling-previous-p ()
  (let ((parent-position
         (html-element-edit-utils-fnil
          (save-excursion
            (web-mode-element-beginning)
            (web-mode-element-parent-position))
          'point))

        (tag-prev-position
         (html-element-edit-utils-x-position
          (lambda ()
            (web-mode-element-beginning)
            (web-mode-tag-previous)
            (web-mode-element-beginning)))))

    (not (= parent-position tag-prev-position))))

(defun html-element-edit-elements-sibling-next-position ()
  (html-element-edit-utils-x-position
   'web-mode-element-sibling-next))

(defun html-element-edit-elements-sibling-next-p ()
  (let ((parent-position
         (html-element-edit-utils-fnil
          (save-excursion
            (web-mode-element-beginning)
            (web-mode-element-parent-position))
          'point))

        (tag-next-position
         (html-element-edit-utils-x-position
          (lambda ()
            (html-element-edit-elements-end-inside)
            (web-mode-tag-next)
            (web-mode-element-beginning)))))

    (not (= parent-position tag-next-position))))

;; Parent
(defun html-element-edit-elements-parent-p ()
  (save-excursion
    (web-mode-element-beginning)
    (and (web-mode-element-parent-position)
         (not (= (web-mode-element-parent-position)
                 (web-mode-element-beginning-position))))))

(defun html-element-edit-elements-root-p ()
  (not (html-element-edit-elements-parent-p)))

;; Child
(defun html-element-edit-elements-child-p ()
  (let ((end-tag-position
         (html-element-edit-utils-x-position
          (lambda ()
            (html-element-edit-elements-end-inside)
            (web-mode-tag-beginning))))

        (child-position
         (save-excursion
           (web-mode-element-beginning)
           (web-mode-tag-next-position))))

    (not (= end-tag-position child-position))))

(defun html-element-edit-elements-child-last ()
  (interactive)
  (if (html-element-edit-elements-child-p)
      (progn
        (html-element-edit-elements-end-inside)
        (web-mode-tag-beginning)
        (web-mode-tag-previous))
    (progn
      (web-mode-element-beginning)
      (web-mode-tag-end))))

(defun html-element-edit-elements-child-first ()
  (interactive)
  (if (html-element-edit-elements-child-p)
      (progn
        (web-mode-element-beginning)
        (web-mode-tag-next))
    (progn
      (web-mode-element-beginning)
      (web-mode-tag-end))))

;; Edit
(defun html-element-edit-elements-transpose-backward ()
  (interactive)
  (when (html-element-edit-elements-sibling-previous-p)
    (save-excursion
      (web-mode-element-sibling-previous)
      (web-mode-element-transpose))))

(defun html-element-edit-elements-expand-p ()
  (html-element-edit-elements-sibling-next-p))

(defun html-element-edit-elements-expand ()
  (interactive)
  (when (html-element-edit-elements-expand-p)
    (let ((content
           (concat
            (string-trim-left
             (save-excursion
               (web-mode-element-end)
               (set-mark (point))
               (web-mode-tag-next)
               (web-mode-element-end)
               (html-element-edit-utils-kill-region)))
            "\n")))
      (save-excursion
        (html-element-edit-elements-end-inside)
        (web-mode-tag-beginning)
        (insert content)))))

(defun html-element-edit-elements-expand-over-border ()
  (interactive)
  (save-excursion
    (while (and (html-element-edit-elements-parent-p)
                (not (html-element-edit-elements-expand-p)))
      (web-mode-element-parent))
    (html-element-edit-elements-expand)))

(defun html-element-edit-elements-contract-p ()
  (html-element-edit-elements-child-p))

(defun html-element-edit-elements-contract ()
  (interactive)
  (when (html-element-edit-elements-contract-p)
    (let ((content
           (save-excursion
             (html-element-edit-elements-child-last)
             (web-mode-element-beginning)
             (web-mode-tag-previous)
             (web-mode-tag-end)
             (set-mark (point))
             (web-mode-tag-next)
             (web-mode-element-end)
             (html-element-edit-utils-kill-region))))

      (save-excursion
        (web-mode-element-end)
        (html-element-edit-elements-direct-after-insert content)))))

(defun html-element-edit-elements-contract-over-border ()
  (interactive)
  (save-excursion
    (while (and (html-element-edit-elements-parent-p)
                (not (html-element-edit-elements-contract-p)))
      (web-mode-element-parent))
    (html-element-edit-elements-contract)))

(defun html-element-edit-elements-dissolve (&optional ARGS)
  (interactive "p")
  (when (html-element-edit-elements-parent-p)
    (save-excursion
      (web-mode-element-beginning)
      (web-mode-element-parent)
      (web-mode-element-vanish ARGS))))

(provide 'html-element-edit-elements)
;;; html-element-edit-elements.el ends here
