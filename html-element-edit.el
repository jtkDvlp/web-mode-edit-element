;;; html-element-edit.el --- Helper-functions for attribute- and element-handling

;; Copyright 2016 Julian T. Knabenschuh

;; Version: 0.0
;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; Homepage: https://github.com/jtkDvlp/html-element-edit
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs
;; Keywords: languages convenience
;; Package-Requires: ((web-mode "14"))

;;; Commentary:

;; HTML-Element Edit is a smart enhancement for the package web-mode inspired by the packages ParEdit and Paxedit.

;; It provides a few helper-functions for attribute- and element-handling based on the functions given by web-mode. Further more it provides functions for slurping, barfing, dissolving, raising ... elements inspired by ParEdit and Paxedit. Last but not least this package includes a minor mode to provide a keymap with default bindings using commands of web-mode and this package.

;; To use this package, add the following lines somewhere in you init file:
;; (require 'html-element-edit)
;; (add-hook 'web-mode-hook 'html-element-edit-minor-mode)

;; See keymap in the main file or online https://github.com/jtkDvlp/html-element-edit

;;; Code:

(require 'web-mode)
(require 'html-element-edit-attributes)
(require 'html-element-edit-elements)

(defvar html-element-edit-minor-mode-map
  (make-keymap)
  "html-element-edit-minor-mode keymap")

(define-minor-mode html-element-edit-minor-mode
  "Minor mode to provide key-bindings for html-element-edit functions"
  nil " html-elemenet-edit" 'html-element-edit-minor-mode-map)

(let ((bindings
       '(;; General
         ("C-(" web-mode-element-wrap)
         ("M-(" web-mode-element-rename)

         ;; Elements
         ("C-<left>" web-mode-element-previous)
         ("C-<right>" web-mode-element-next)

         ("M-<left>" html-element-edit-elements-contract-over-border)
         ("M-<right>" html-element-edit-elements-expand-over-border)

         ("C-M-<left>" html-element-edit-elements-transpose-backward)
         ("C-M-<right>" web-mode-element-transpose)

         ("C-<up>" web-mode-element-beginning)
         ("C-<down>" web-mode-tag-match)

         ("C-S-<up>" web-mode-element-parent)
         ("C-S-<down>" web-mode-element-next)

         ("M-<up>" html-element-edit-elements-dissolve)
         ("M-<down>" html-element-edit-elements-raise)

         ("C-M-<up>" web-mode-element-content-select)
         ("C-M-<down>" web-mode-element-vanish)

         ("C-k" web-mode-element-kill)
         ("C-S-k" html-element-edit-elements-kill-siblings)
         ("M-k" html-element-edit-elements-kill-siblings-previous)
         ("M-S-k" html-element-edit-elements-kill-siblings-next)

         ;; Attributes
         ("C-S-<left>" web-mode-attribute-previous)
         ("C-S-<right>" web-mode-attribute-next)

         ("C-M-S-<left>" html-element-edit-attributes-transpose-backward)
         ("C-M-S-<right>" web-mode-attribute-transpose)

         ("C-M-S-<up>" web-mode-attribute-beginning)
         ("C-M-S-<down>" html-element-edit-attributes-end-inside)

         ("C-M-S-k" web-mode-attribute-kill))))
  (dolist (binding bindings)
    (define-key html-element-edit-minor-mode-map
      (kbd (car binding))
      (car (cdr binding))))

  (provide 'html-element-edit))
;;; html-element-edit.el ends here
