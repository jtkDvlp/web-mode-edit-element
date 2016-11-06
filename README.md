# HTML-Element Edit [![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://img.shields.io/badge/License-GPL%20v2-blue.svg)
HTML-Element Edit is a smart enhancement for the Emacs-Package ["web-mode"](https://github.com/fxbois/web-mode) inspired by the packages [ParEdit](https://www.emacswiki.org/emacs/ParEdit) and [Paxedit](https://github.com/promethial/paxedit).

It provides a few helper-functions for attribute- and element-handling based on the functions given by ["web-mode"](https://github.com/fxbois/web-mode). Futher more it provides functions for slurping, barfing, dissolving, raise ... inspired by [ParEdit](https://www.emacswiki.org/emacs/ParEdit) and [Paxedit](https://github.com/promethial/paxedit). Last but not least this package includes a minor mode to provide an keymap with default bindings.

## Getting started
### Get it
- Manually download it and [set up your load path](http://www.emacswiki.org/emacs/InstallingPackages).

### Usage
```
(require 'html-element-edit)
(add-hook 'web-mode-hook 'html-element-edit-minor-mode)
```

### Keymap
Shortcut | Command
--- | ---
General |
C-( | web-mode-element-wrap
M-( | web-mode-element-rename
Elements |
C-<left> | web-mode-element-previous
C-<right> | web-mode-element-next
M-<left> | html-element-edit-elements-contract-over-border
M-<right> | html-element-edit-elements-expand-over-border
C-M-<left> | html-element-edit-elements-transpose-backward
C-M-<right> | web-mode-element-transpose
C-<up> | web-mode-element-beginning
C-<down> | web-mode-tag-match
C-S-<up> | web-mode-element-parent
C-S-<down> | web-mode-element-next
M-<up> | html-element-edit-elements-dissolve
M-<down> | html-element-edit-elements-raise
C-M-<up> | web-mode-element-content-select
C-M-<down> | web-mode-element-vanish
C-k | web-mode-element-kill
C-S-k | html-element-edit-elements-kill-siblings
M-k | html-element-edit-elements-kill-siblings-previous
M-S-k | html-element-edit-elements-kill-siblings-next
Attributes |
C-S-<left> | web-mode-attribute-previous
C-S-<right> | web-mode-attribute-next
C-M-S-<left> | html-element-edit-attributes-transpose-backward
C-M-S-<right> | web-mode-attribute-transpose
C-M-S-<up> | web-mode-attribute-beginning
C-M-S-<down> | html-element-edit-attributes-end-inside
C-M-S-k | web-mode-attribute-kill

## Appendix
I´d be thankful to receive patches, comments and constructive criticism.

Hope the package is useful :-)
