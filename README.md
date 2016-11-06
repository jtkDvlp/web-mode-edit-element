# HTML-Element Edit [![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://img.shields.io/badge/License-GPL%20v2-blue.svg)
HTML-Element Edit is a smart enhancement for the Emacs-Package [web-mode](https://github.com/fxbois/web-mode) inspired by the packages [ParEdit](https://www.emacswiki.org/emacs/ParEdit) and [Paxedit](https://github.com/promethial/paxedit).

It provides a few helper-functions for attribute- and element-handling based on the functions given by [web-mode](https://github.com/fxbois/web-mode). Further more it provides functions for slurping, barfing, dissolving, raising ... elements inspired by [ParEdit](https://www.emacswiki.org/emacs/ParEdit) and [Paxedit](https://github.com/promethial/paxedit). Last but not least this package includes a minor mode to provide a keymap with default bindings using commands of [web-mode](https://github.com/fxbois/web-mode) and this package.

## Getting started
### Get it
- Manually download it and [set up your load path](http://www.emacswiki.org/emacs/InstallingPackages).

### Usage
```clojure
(require 'html-element-edit)
(add-hook 'web-mode-hook 'html-element-edit-minor-mode)
```

### Keymap
Shortcut | Command
--- | ---
**General** |
C-( | web-mode-element-wrap
M-( | web-mode-element-rename
**Elements** |
C-&lt;left&gt; | web-mode-element-previous
C-&lt;right&gt; | web-mode-element-next
M-&lt;left&gt; | html-element-edit-elements-contract-over-border
M-&lt;right&gt; | html-element-edit-elements-expand-over-border
C-M-&lt;left&gt; | html-element-edit-elements-transpose-backward
C-M-&lt;right&gt; | web-mode-element-transpose
C-&lt;up&gt; | web-mode-element-beginning
C-&lt;down&gt; | web-mode-tag-match
C-S-&lt;up&gt; | web-mode-element-parent
C-S-&lt;down&gt; | web-mode-element-next
M-&lt;up&gt; | html-element-edit-elements-dissolve
M-&lt;down&gt; | html-element-edit-elements-raise
C-M-&lt;up&gt; | web-mode-element-content-select
C-M-&lt;down&gt; | web-mode-element-vanish
C-k | web-mode-element-kill
C-S-k | html-element-edit-elements-kill-siblings
M-k | html-element-edit-elements-kill-siblings-previous
M-S-k | html-element-edit-elements-kill-siblings-next
**Attributes** |
C-S-&lt;left&gt; | web-mode-attribute-previous
C-S-&lt;right&gt; | web-mode-attribute-next
C-M-S-&lt;left&gt; | html-element-edit-attributes-transpose-backward
C-M-S-&lt;right&gt; | web-mode-attribute-transpose
C-M-S-&lt;up&gt; | web-mode-attribute-beginning
C-M-S-&lt;down&gt; | html-element-edit-attributes-end-inside
C-M-S-k | web-mode-attribute-kill

## Appendix
I´d be thankful to receive patches, comments and constructive criticism.

Hope the package is useful :-)
