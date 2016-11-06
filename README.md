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
C-&ltleft&gt; | web-mode-element-previous
C-&ltright&gt; | web-mode-element-next
M-&ltleft&gt; | html-element-edit-elements-contract-over-border
M-&ltright&gt; | html-element-edit-elements-expand-over-border
C-M-&ltleft&gt; | html-element-edit-elements-transpose-backward
C-M-&ltright&gt; | web-mode-element-transpose
C-&ltup&gt; | web-mode-element-beginning
C-&ltdown&gt; | web-mode-tag-match
C-S-&ltup&gt; | web-mode-element-parent
C-S-&ltdown&gt; | web-mode-element-next
M-&ltup&gt; | html-element-edit-elements-dissolve
M-&ltdown&gt; | html-element-edit-elements-raise
C-M-&ltup&gt; | web-mode-element-content-select
C-M-&ltdown&gt; | web-mode-element-vanish
C-k | web-mode-element-kill
C-S-k | html-element-edit-elements-kill-siblings
M-k | html-element-edit-elements-kill-siblings-previous
M-S-k | html-element-edit-elements-kill-siblings-next
Attributes |
C-S-&ltleft&gt; | web-mode-attribute-previous
C-S-&ltright&gt; | web-mode-attribute-next
C-M-S-&ltleft&gt; | html-element-edit-attributes-transpose-backward
C-M-S-&ltright&gt; | web-mode-attribute-transpose
C-M-S-&ltup&gt; | web-mode-attribute-beginning
C-M-S-&ltdown&gt; | html-element-edit-attributes-end-inside
C-M-S-k | web-mode-attribute-kill

## Appendix
I´d be thankful to receive patches, comments and constructive criticism.

Hope the package is useful :-)
