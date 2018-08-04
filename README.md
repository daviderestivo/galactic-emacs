# emacs-config

This are my Emacs configuration files. Below you can find the list of the packages used.

![Emacs frame with natural title bar](https://raw.githubusercontent.com/daviderestivo/emacs-config/master/emacs_with_natural_title_bar.png)

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
* auto-sudoedit [[Link](https://github.com/ncaq/auto-sudoedit)]
* avy [[Link](https://github.com/abo-abo/avy)]
* cider [[Link](https://github.com/clojure-emacs/cider)]
* cisco-router-mode [[Link](https://www.emacswiki.org/emacs/download/cisco-router-mode.el)]
* command-log-mode [[Link](https://github.com/lewang/command-log-mode)]
* company-mode [[Link](https://github.com/company-mode/company-mode)]
* diff-hl [[Link](https://github.com/dgutov/diff-hl)]
* dockerfile-mode [[Link](https://github.com/spotify/dockerfile-mode)]
* elisp-bug-hunter [[Link](https://github.com/Malabarba/elisp-bug-hunter)]
* elpy [[Link](https://elpy.readthedocs.io)]
* emacs-dashboard [[Link](https://github.com/rakanalh/emacs-dashboard)]
* esh-autosuggest [[Link](https://github.com/dieggsy/esh-autosuggest)]
* exec-path-from-shell [[Link](https://github.com/purcell/exec-path-from-shell)]
* eyebrowse [[Link](https://github.com/wasamasa/eyebrowse)]
* helm [[Link](https://github.com/emacs-helm/helm)]
* helm-ag [[Link](https://github.com/syohex/emacs-helm-ag)]
* helm-descbinds [[Link](https://github.com/emacs-helm/helm-descbinds)]
* helm-github-stars [[Link](https://github.com/Sliim/helm-github-stars)]
* helm-projectile [[Link](https://github.com/bbatsov/helm-projectile)]
* ibuffer-sidebar [[Link](https://github.com/jojojames/ibuffer-sidebar)]
* imenu-list [[Link](https://github.com/bmag/imenu-list)]
* jinja2-mode [[Link](https://github.com/paradoxxxzero/jinja2-mode)]
* magit [[Link](https://magit.vc)]
* magit-org-todos [[Link](https://github.com/danielma/magit-org-todos.el)]
* markdown-mode [[Link](http://jblevins.org/projects/markdown-mode)]
* multiple-cursors [[Link](https://github.com/magnars/multiple-cursors.el)]
* ob-ipython [[Link](https://github.com/gregsexton/ob-ipython)]
* org-beautify-theme [[Link](https://github.com/jonnay/org-beautify-theme)]
* org-bullets [[Link](https://github.com/sabof/org-bullets)]
* org-download [[Link](https://github.com/abo-abo/org-download)]
* org-plus-contrib [[Link](http://orgmode.org)]
* projectile [[Link](https://github.com/bbatsov/projectile)]
* psession [[Link](https://github.com/thierryvolpiatto/psession)]
* py-autopep8 [[Link](https://github.com/paetzke/py-autopep8.el)]
* rainbow-delimiters [[Link](https://www.emacswiki.org/emacs/RainbowDelimiters)]
* shell-pop-el [[link](https://github.com/kyagi/shell-pop-el)]
* smart-mode-line [[Link](https://github.com/Malabarba/smart-mode-line)]
* sr-speedbar [[Link](https://github.com/emacsorphanage/sr-speedbar)]
* transpose-frame [[Link](https://www.emacswiki.org/emacs/TransposeFrame)]
* undo-tree [[Link](https://github.com/emacsmirror/undo-tree)]
* use-package [[Link](https://github.com/jwiegley/use-package)]
* volatile-highlights [[Link](https://github.com/k-talo/volatile-highlights.el)]
* which-key [[Link](https://github.com/justbur/emacs-which-key)]
* wttrin [[Link](https://github.com/bcbcarl/emacs-wttrin)]
* yaml-mode [[Link](https://github.com/yoshiki/yaml-mode)]
* yang-mode [[Link](https://github.com/mbj4668/yang-mode)]
* yasnippet [[Link](https://github.com/joaotavora/yasnippet)]

## Key bindings

| Key           | Function                                 |
| :---          | :---                                     |
| < f2 >        | magit-status                             |
| < f5 >        | magit-list-repositories                  |
| < f6 >        | org-directory-search-ag                  |
| < f7 >        | Create new empty buffer                  |
| < f8 >        | ispell-word                              |
| M-< f8 >      | flyspell-check-next-highlighted-word     |
| < f9 >        | Toggle sroll bar                         |
| < f12 >       | imenu-list-smart-toggle                  |
| M-<f12>       | sr-speedbar-toggle                       |
| C-< f12 >     | ibuffer-sidebar-toggle-sidebar           |
| C-c m         | avy-got-char                             |
| C-=           | copy-line                                |
| C-c left      | Winner mode - Undo windows changes       |
| C-c right     | Winner mode - Redo windows changes       |
| C-m           | YAML mode - New line and indent          |
| C-x r b       | helm-filtered-bookmarks                  |
| C-x C-r       | helm-recentf                             |
| M-s           | helm-do-ag-this-file                     |
| C-u M-s       | helm-do-ag-buffers                       |
| C-c C-l       | helm-eshell-history                      |
| C-c C-;       | helm-eshell-prompts                      |
| C-+           | drestivo/insert-date                     |
| C-x t         | shell-pop-universal-key                  |
| C-c j         | drestivo/org-show-current-heading-tidily |
| C-S-c C-S-c   | mc/edit-lines                            |
| C->           | mc/mark-next-like-this                   |
| C-<           | mc/mark-previous-like-this               |
| M-C->         | mc/mark-next-like-this-symbol            |
| M-C-<         | mc/mark-previous-like-this-symbol        |
| C-c C-<       | mc/mark-all-like-this                    |
| C-c C->       | mc/mark-all-like-this-symbol             |
| C-c C-n       | mc/insert-numbers                        |
| C-c C-r       | mc/reverse-regions                       |
| C-c C-s       | mc/sort-regions                          |
| C-S-<mouse-1> | mc/add-cursor-on-click                   |
| C-'           | mc-hide-unmatched-lines-mode             |
| M-o           | other-window                             |


Feel free to drop me an email in case of questions.
