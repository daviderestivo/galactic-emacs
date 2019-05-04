# emacs-config

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This repository contains my Emacs configuration. You can clone it as
it comes or simply use whatever part you might need/like. The look and
feel of the Emacs frame is based on the atom-one-dark-theme with some
minor changes.

## Emacs Scratch Buffer
![Emacs scracth buffer](https://raw.githubusercontent.com/daviderestivo/emacs-config/master/screenshots/emacs_scratch_buffer.png)

## Emacs Dashboard
![Emacs dashboard](https://raw.githubusercontent.com/daviderestivo/emacs-config/master/screenshots/emacs_dashboard.png)


## Emacs sidebars
![Emacs sidebars](https://raw.githubusercontent.com/daviderestivo/emacs-config/master/screenshots/emacs_sidebars.png)

## Installation
### Emacs Installation

This configuration is mainly tested on the HEAD version of Emacs
(currently 27.x) running on macOS. If your're using brew, as a package
manager on macOS, please install Emacs with the below command:

``` bash
brew tap daviderestivo/emacs-head
brew install emacs-head --HEAD --with-cocoa --with-no-frame-refocus --with-imagemagick --with-jansson
```

or if you prefer Emacs 26.x:

``` bash
brew tap daviderestivo/emacs-head
brew install emacs-head --with-cocoa --with-no-frame-refocus --with-imagemagick --with-multicolor-fonts
```

### Repository Clone

Please run the below commands to backup your current Emacs config and clone this repo:

``` bash
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

### Narrowing
* helm [[Link](https://github.com/emacs-helm/helm)]
* helm-ag [[Link](https://github.com/syohex/emacs-helm-ag)]
* helm-descbinds [[Link](https://github.com/emacs-helm/helm-descbinds)]
* helm-github-stars [[Link](https://github.com/Sliim/helm-github-stars)]
* helm-projectile [[Link](https://github.com/bbatsov/helm-projectile)]

### Networking
* cisco-router-mode [[Link](https://www.emacswiki.org/emacs/download/cisco-router-mode.el)]

### Programming
* cider [[Link](https://github.com/clojure-emacs/cider)]
* company-lsp [[Link](https://github.com/tigersoldier/company-lsp)]
* company-mode [[Link](https://github.com/company-mode/company-mode)]
* dap-mode [[Link](https://github.com/yyoncho/dap-mode)]
* dap-java [[Link](https://github.com/yyoncho/dap-mode)]
* diff-hl [[Link](https://github.com/dgutov/diff-hl)]
* dockerfile-mode [[Link](https://github.com/spotify/dockerfile-mode)]
* elisp-bug-hunter [[Link](https://github.com/Malabarba/elisp-bug-hunter)]
* elpy [[Link](https://elpy.readthedocs.io)]
* geiser [[Link](https://gitlab.com/jaor/geiser)]
* lsp-mode [[Link](https://github.com/emacs-lsp/lsp-mode)]
* lsp-java [[Link](https://github.com/emacs-lsp/lsp-java)]
* lsp-java-treemacs [[Link](https://github.com/emacs-lsp/lsp-java)]
* lsp-ui [[Link](https://github.com/emacs-lsp/lsp-ui)]
* jinja2-mode [[Link](https://github.com/paradoxxxzero/jinja2-mode)]
* json-mode   [[Link](https://github.com/joshwnj/json-mode)]
* magit [[Link](https://magit.vc)]
* magit-org-todos [[Link](https://github.com/danielma/magit-org-todos.el)]
* markdown-mode [[Link](http://jblevins.org/projects/markdown-mode)]
* projectile [[Link](https://github.com/bbatsov/projectile)]
* py-autopep8 [[Link](https://github.com/paetzke/py-autopep8.el)]
* smartparens  [[Link](https://github.com/Fuco1/smartparens)]
* treemacs-projectile [[Link](https://github.com/Alexander-Miller/treemacs)]
* yaml-mode [[Link](https://github.com/yoshiki/yaml-mode)]
* yang-mode [[Link](https://github.com/mbj4668/yang-mode)]
* yasnippet [[Link](https://github.com/joaotavora/yasnippet)]

### Org
* ob-ipython [[Link](https://github.com/gregsexton/ob-ipython)]
* org-beautify-theme [[Link](https://github.com/jonnay/org-beautify-theme)]
* org-bullets [[Link](https://github.com/sabof/org-bullets)]
* org-download [[Link](https://github.com/abo-abo/org-download)]
* idle-org-agenda [[Link](https://github.com/enisozgen/idle-org-agenda)]
* org-mind-map [[Link](https://github.com/theodorewiles/org-mind-map)
* org-plus-contrib [[Link](http://orgmode.org)]

### Visual
* all-the-icons [[Link](https://github.com/domtronn/all-the-icons.el)]
* atom-one-dark-theme [[Link](https://github.com/jonathanchu/atom-one-dark-theme)]
* auto-highlight-symbol [[Link](https://github.com/gennad/auto-highlight-symbol)]
* beacon [[Link](https://github.com/Malabarba/beacon)]
* emacs-dashboard [[Link](https://github.com/rakanalh/emacs-dashboard)]
* eyebrowse [[Link](https://github.com/wasamasa/eyebrowse)]
* goto-line-preview [[Link](https://github.com/jcs090218/goto-line-preview)]
* highlight-indent-guides [[Link](https://github.com/DarthFennec/highlight-indent-guides)]
* ibuffer-sidebar [[Link](https://github.com/jojojames/ibuffer-sidebar)]
* imenu-list [[Link](https://github.com/bmag/imenu-list)]
* rainbow-delimiters [[Link](https://www.emacswiki.org/emacs/RainbowDelimiters)]
* smart-mode-line [[Link](https://github.com/Malabarba/smart-mode-line)]
* smart-mode-line-atom-one-dark-theme [[Link](https://github.com/daviderestivo/smart-mode-line-atom-one-dark-theme)]
* transpose-frame [[Link](https://www.emacswiki.org/emacs/TransposeFrame)]
* treemacs-icons-dired [[Link](https://github.com/Alexander-Miller/treemacs)]
* volatile-highlights [[Link](https://github.com/k-talo/volatile-highlights.el)]

### Various
* auto-package-update [[Link](https://github.com/rranelli/auto-package-update.el)]
* auto-sudoedit [[Link](https://github.com/ncaq/auto-sudoedit)]
* avy [[Link](https://github.com/abo-abo/avy)]
* command-log-mode [[Link](https://github.com/lewang/command-log-mode)]
* esh-autosuggest [[Link](https://github.com/dieggsy/esh-autosuggest)]
* exec-path-from-shell [[Link](https://github.com/purcell/exec-path-from-shell)]
* load-bash-alias [[Link](https://github.com/daviderestivo/load-bash-alias)]
* move-text [[Link](https://github.com/emacsfodder/move-text)]
* multiple-cursors [[Link](https://github.com/magnars/multiple-cursors.el)]
* open-junk-file [[Link](https://github.com/rubikitch/open-junk-file)]
* psession [[Link](https://github.com/thierryvolpiatto/psession)]
* shell-pop-el [[Link](https://github.com/kyagi/shell-pop-el)]
* string-inflection [[Link](https://github.com/akicho8/string-inflection)]
* treemacs [[Link](https://github.com/Alexander-Miller/treemacs)]
* undo-tree [[Link](https://github.com/emacsmirror/undo-tree)]
* use-package [[Link](https://github.com/jwiegley/use-package)]
* which-key [[Link](https://github.com/justbur/emacs-which-key)]
* wttrin [[Link](https://github.com/bcbcarl/emacs-wttrin)]

## Key bindings

### General

| Key           | Function                                 |
| :---          | :---                                     |
| \<f2\>        | magit-status                             |
| \<f5\>        | magit-list-repositories                  |
| \<f6\>        | org-directory-search-ag                  |
| \<f7\>        | create new empty buffer (org-mode)       |
| \<f8\>        | ispell-word                              |
| M-\<f8\>      | flyspell-goto-next-error                 |
| \<f9\>        | toggle sroll bar                         |
| \<f12\>       | imenu-list-smart-toggle                  |
| M-\<f12\>     | treemacs                                 |
| C-\<f12\>     | ibuffer-sidebar-toggle-sidebar           |
| s-/           | avy-goto-char                            |
| C-=           | copy-line                                |
| C-c \<left\>  | Winner mode - Undo windows changes       |
| C-c \<right\> | Winner mode - Redo windows changes       |
| C-m           | YAML mode - New line and indent          |
| C-c p         | projectile keymap                        |
| M-+           | drestivo-insert-date                     |
| C-x t         | shell-pop-universal-key                  |
| C-c j         | drestivo-org-show-current-heading-tidily |
| M-o           | other-window                             |
| M-up          | move-text-up                             |
| M-down        | move-text-down                           |

### Helm

| Key     | Function                |
| :---    | :---                    |
| C-x r b | helm-filtered-bookmarks |
| C-x C-r | helm-recentf            |
| M-s     | helm-do-ag-this-file    |
| C-u M-s | helm-do-ag-buffers      |
| C-c C-; | helm-eshell-prompts     |
| C-c C-l | helm-eshell-history     |

### Multiple Cursors

| Key             | Function                          |
| :---            | :---                              |
| C-S-c C-S-c     | mc/edit-lines                     |
| C->             | mc/mark-next-like-this            |
| C-<             | mc/mark-previous-like-this        |
| M-C->           | mc/mark-next-like-this-symbol     |
| M-C-<           | mc/mark-previous-like-this-symbol |
| C-c C-<         | mc/mark-all-like-this             |
| C-c C->         | mc/mark-all-like-this-symbol      |
| C-c C-n         | mc/insert-numbers                 |
| C-c C-r         | mc/reverse-regions                |
| C-c C-s         | mc/sort-regions                   |
| C-S-\<mouse-1\> | mc/add-cursor-on-click            |
| C-'             | mc-hide-unmatched-lines-mode      |

### Org-mode

| Key    | Function                                 |
| :---   | :---                                     |
| C-c e  | org-encrypt-entry                        |
| C-c d  | org-decrypt-entry                        |
| C-c l  | org-store-link                           |
| C-c a  | org-agenda                               |
| C-c c  | org-capture                              |
| C-c b  | org-iswitch                              |
| C-c j  | drestivo-org-show-current-heading-tidily |
| <F6>   | drestivo-org-directory-search-ag         |
| M-<F6> | helm-do-ag                               |

### Smartparens

| Key             | Function               |
| :---            | :---                   |
| C-M-f           | sp-forward-sexp        |
| C-M-b           | sp-backward-sexp       |
| C-M-u           | sp-up-sexp             |
| C-M-d           | sp-down-sexp           |
| C-M-a           | sp-beginning-of-sexp   |
| C-M-e           | sp-end-of-sexp         |
| C-M-n           | sp-next-sexp           |
| C-M-p           | sp-previous-sexp       |
| C-M-t           | sp-transpose-sexp      |
| C-M-k           | sp-kill-sexp           |
| C-M-w           | sp-copy-sexp           |
| M-\<backspace\> | sp-unwrap-sexp         |
| C-s-\<right\>   | sp-forward-slurp-sexp  |
| C-s-\<left\>    | sp-forward-barf-sexp   |
| C-M-\<left\>    | sp-backward-slurp-sexp |
| C-M-\<right\>   | sp-backward-barf-sexp  |
| M-(             | sp-wrap-round          |
| M-[             | sp-wrap-square         |
| M-{             | sp-wrap-curly          |


Feel free to drop me an email in case of questions.
