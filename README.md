# emacs-config

This are my Emacs configuration files. Below you can find the list of the packages used.

## Installation

```
cd ~
mv .emacs.d .emacs.d.bak
git clone https://github.com/daviderestivo/emacs-config.git .emacs.d
cd .emacs.d
git submodule init
git submodule update
git submodule foreach --recursive git checkout master
```

## Included packages

The following is the list of the included packages. Packages dependencies are
not listed.

* atom-one-dark-theme [[link](https://github.com/jonathanchu/atom-one-dark-theme)]
* auto-package-update [[link](https://github.com/rranelli/auto-package-update.el)]
* cisco-router-mode [[link](https://www.emacswiki.org/emacs/download/cisco-router-mode.el)]
* company-mode [[link](https://github.com/company-mode/company-mode)]
* diff-hl [[link](https://github.com/dgutov/diff-hl)]
* elpy [[link](https://elpy.readthedocs.io)]
* exec-path-from-shell [link](https://github.com/purcell/exec-path-from-shell)]
* helm [[link](https://github.com/emacs-helm/helm)]
* helm-descbinds [[link](https://github.com/emacs-helm/helm-descbinds)]
* helm-projectile [[link](https://github.com/bbatsov/helm-projectile)]
* jinja2-mode [[link](https://github.com/paradoxxxzero/jinja2-mode)]
* magit [[link](https://magit.vc)]
* markdown-mode [[link](http://jblevins.org/projects/markdown-mode)]
* ob-ipython [[link](https://github.com/gregsexton/ob-ipython)]
* org-download [[link](https://github.com/abo-abo/org-download)]
* org-plus-contrib [[link](http://orgmode.org)]
* projectile [[link](https://github.com/bbatsov/projectile)]
* psession [[link](https://github.com/thierryvolpiatto/psession)]
* py-autopep8 [[link](https://github.com/paetzke/py-autopep8.el)]
* rainbow-delimiters [[link](https://www.emacswiki.org/emacs/RainbowDelimiters)]
* smart-mode-line [[link](https://github.com/Malabarba/smart-mode-line)]
* transpose-frame [[link](https://www.emacswiki.org/emacs/TransposeFrame)]
* undo-tree [[link](https://github.com/emacsmirror/undo-tree)]
* use-package [[link](https://github.com/jwiegley/use-package)]
* yaml-mode [[Link](https://github.com/yoshiki/yaml-mode)]
* yasnippet [[Link](https://github.com/joaotavora/yasnippet)]
* wheatgrass-theme [[Link](https://github.com/jwiegley/emacs-release/blob/master/etc/themes/wheatgrass-theme.el)]

## Key bindings

| Key | Function |
| :--- | :--- |
| < f2 > | magit-status |
| < f7 > | Create new empty buffer |
| < f8 > | ispell-word |
| M-< f8 > | flyspell-check-next-highlighted-word |
| < f9 > | Toggle sroll bar  |
| C-c C-c | copy-line |
| C-c left  | Winner mode - Undo windows changes  |
| C-c right  | Winner mode - Redo windows changes  |
| C-m | YAML mode - New line and indent |
| C-x r b | helm-filtered-bookmarks |
| C-x C-r | helm-recentf |


Feel free to drop me an email in case of questions.
