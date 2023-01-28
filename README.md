[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://img.shields.io/badge/License-GPL%20v2-blue.svg) [![MELPA](http://melpa.org/packages/web-mode-edit-element-badge.svg)](http://melpa.org/#/web-mode-edit-element) [![MELPA Stable](http://stable.melpa.org/packages/web-mode-edit-element-badge.svg)](http://stable.melpa.org/#/web-mode-edit-element)
[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/donate?hosted_button_id=2PDXQMHX56T6U)

# web-mode-edit-element
"web-mode-edit-element" is a smart enhancement for the Emacs-Package [web-mode](https://github.com/fxbois/web-mode) inspired by the packages [ParEdit](https://www.emacswiki.org/emacs/ParEdit) and [Paxedit](https://github.com/promethial/paxedit).


It provides a few helper-functions for attribute- and element-handling based on the functions given by [web-mode](https://github.com/fxbois/web-mode). Further more it provides functions for slurping, barfing, dissolving, raising ... elements inspired by [ParEdit](https://www.emacswiki.org/emacs/ParEdit) and [Paxedit](https://github.com/promethial/paxedit). Last but not least this package includes a minor mode to provide a keymap with default bindings using commands of [web-mode](https://github.com/fxbois/web-mode) and this package.

## Getting started
### Get it
- Via [![MELPA](http://melpa.org/packages/web-mode-edit-element-badge.svg)](http://melpa.org/#/web-mode-edit-element) or [![MELPA Stable](http://stable.melpa.org/packages/web-mode-edit-element-badge.svg)](http://stable.melpa.org/#/web-mode-edit-element)
- Manually download it and [set up your load path](http://www.emacswiki.org/emacs/InstallingPackages).

### Usage
```clojure
(require 'web-mode-edit-element)
(add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode)
```

### Keymap
Shortcut | Command
--- | ---
**General** |
`C-(` | web-mode-element-wrap
`M-(` | web-mode-element-rename
`C-M-SPC` | web-mode-element-content-select
**Elements** |
`C-<left>` | web-mode-element-previous
`C-<right>` | web-mode-element-next
`M-<left>` | web-mode-edit-element-elements-contract-over-border
`M-<right>` | web-mode-edit-element-elements-expand-over-border
`C-M-<left>` | web-mode-edit-element-elements-transpose-backward
`C-M-<right>` | web-mode-element-transpose
`C-<up>` | web-mode-element-beginning
`C-<down>` | web-mode-tag-match
`C-S-<up>` | web-mode-element-parent
`C-S-<down>` | web-mode-element-next
`M-<up>` | web-mode-edit-element-elements-dissolve
`M-<down>` | web-mode-edit-element-elements-raise
`C-M-<up>` | web-mode-element-vanish
`C-M-<down>` | web-mode-edit-element-elements-sibling-next-or-parent
`C-k` | web-mode-element-kill
`C-S-k` | web-mode-edit-element-elements-kill-siblings
`M-k` | web-mode-edit-element-elements-kill-siblings-previous
`M-K` | web-mode-edit-element-elements-kill-siblings-next
**Attributes** |
`C-S-<left>` | web-mode-attribute-previous
`C-S-<right>` | web-mode-attribute-next
`C-M-S-<left>` | web-mode-edit-element-attributes-transpose-backward
`C-M-S-<right>` | web-mode-attribute-transpose
`C-M-S-<up>` | web-mode-attribute-beginning
`C-M-S-<down>` | web-mode-edit-element-attributes-end-inside
`C-M-K` | web-mode-attribute-kill

## Appendix
I´d be thankful to receive patches, comments and constructive criticism.

Hope the package is useful :-)
