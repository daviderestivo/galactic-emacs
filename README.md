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

* all-the-icons [[Link](https://github.com/domtronn/all-the-icons.el)]
* atom-one-dark-theme [[Link](https://github.com/jonathanchu/atom-one-dark-theme)]
* auto-package-update [[Link](https://github.com/rranelli/auto-package-update.el)]
* cisco-router-mode [[Link](https://www.emacswiki.org/emacs/download/cisco-router-mode.el)]
* command-log-mode [[Link](https://github.com/lewang/command-log-mode)]
* company-mode [[Link](https://github.com/company-mode/company-mode)]
* diff-hl [[Link](https://github.com/dgutov/diff-hl)]
* dockerfile-mode  [[Link](https://github.com/spotify/dockerfile-mode)]
* elpy [[Link](https://elpy.readthedocs.io)]
* exec-path-from-shell [Link](https://github.com/purcell/exec-path-from-shell)]
* helm [[Link](https://github.com/emacs-helm/helm)]
* helm-ag [[Link](https://github.com/syohex/emacs-helm-ag)]
* helm-descbinds [[Link](https://github.com/emacs-helm/helm-descbinds)]
* helm-projectile [[Link](https://github.com/bbatsov/helm-projectile)]
* jinja2-mode [[Link](https://github.com/paradoxxxzero/jinja2-mode)]
* magit [[Link](https://magit.vc)]
* markdown-mode [[Link](http://jblevins.org/projects/markdown-mode)]
* nlinum-hl [[Link](https://github.com/hlissner/emacs-nlinum-hl)]
* ob-ipython [[Link](https://github.com/gregsexton/ob-ipython)]
* org-beautify-theme [[Link](https://github.com/jonnay/org-beautify-theme)]
* org-bullets [[Link](https://github.com/sabof/org-bullets)]
* org-download [[Link](https://github.com/abo-abo/org-download)]
* org-plus-contrib [[Link](http://orgmode.org)]
* projectile [[Link](https://github.com/bbatsov/projectile)]
* psession [[Link](https://github.com/thierryvolpiatto/psession)]
* py-autopep8 [[Link](https://github.com/paetzke/py-autopep8.el)]
* rainbow-delimiters [[Link](https://www.emacswiki.org/emacs/RainbowDelimiters)]
* shell-pop-el    [[link](https://github.com/kyagi/shell-pop-el)]
* smart-mode-line [[Link](https://github.com/Malabarba/smart-mode-line)]
* transpose-frame [[Link](https://www.emacswiki.org/emacs/TransposeFrame)]
* undo-tree [[Link](https://github.com/emacsmirror/undo-tree)]
* use-package [[Link](https://github.com/jwiegley/use-package)]
* volatile-highlights [[Link](https://github.com/k-talo/volatile-highlights.el)]
* wheatgrass-theme [[Link](https://github.com/jwiegley/emacs-release/blob/master/etc/themes/wheatgrass-theme.el)]
* which-key [[Link](https://github.com/justbur/emacs-which-key)]
* wttrin [[Link](https://github.com/bcbcarl/emacs-wttrin)]
* yaml-mode [[Link](https://github.com/yoshiki/yaml-mode)]
* yang-mode [[Link](https://github.com/mbj4668/yang-mode)]
* yasnippet [[Link](https://github.com/joaotavora/yasnippet)]

## Key bindings

| Key | Function |
| :--- | :--- |
| < f2 > | magit-status |
| < f5 > | magit-list-repositories |
| < f6 > | org-directory-search-ag |
| < f7 > | Create new empty buffer |
| < f8 > | ispell-word |
| M-< f8 > | flyspell-check-next-highlighted-word |
| < f9 > | Toggle sroll bar  |
| C-= | copy-line |
| C-c left  | Winner mode - Undo windows changes  |
| C-c right  | Winner mode - Redo windows changes  |
| C-m | YAML mode - New line and indent |
| C-x r b | helm-filtered-bookmarks |
| C-x C-r | helm-recentf |
| M-s | helm-do-ag-this-file |
| C-u M-s | helm-do-ag-buffers |
| C-c C-l |  helm-eshell-history |
| C-c C-; |  helm-eshell-prompts |
| C-+ | drestivo/insert-date |
| C-x t | shell-pop-universal-key |


Feel free to drop me an email in case of questions.
