# Key bindings

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Key bindings](#key-bindings)
    - [General](#general)
    - [Helm](#helm)
    - [Multiple Cursors](#multiple-cursors)
    - [Org-mode](#org-mode)
    - [Smartparens](#smartparens)

<!-- markdown-toc end -->


## General

| Key           | Function                                       |
| :---          | :---                                           |
| \<f2\>        | magit-status                                   |
| \<f5\>        | magit-list-repositories                        |
| \<f7\>        | create new empty buffer (org-mode)             |
| \<f8\>        | ispell-word                                    |
| M-\<f8\>      | flyspell-goto-next-error                       |
| \<f9\>        | toggle sroll bar                               |
| \<f12\>       | imenu-list-smart-toggle                        |
| M-\<f12\>     | treemacs                                       |
| C-\<f12\>     | ibuffer-sidebar-toggle-sidebar                 |
| s-/           | avy-goto-char                                  |
| C-=           | copy-line                                      |
| C-c \<left\>  | Winner mode - Undo windows changes             |
| C-c \<right\> | Winner mode - Redo windows changes             |
| C-c r p       | parrot-rotate-prev-word-at-point               |
| C-c r n       | parrot-rotate-next-word-at-point               |
| C-m           | YAML mode - New line and indent                |
| C-c p         | projectile keymap                              |
| M-+           | galactic-emacs-insert-date                     |
| C-c s         | shell-pop-eshell                               |
| C-c j         | galactic-emacs-org-show-current-heading-tidily |
| M-o           | other-window                                   |
| M-up          | move-text-up                                   |
| M-down        | move-text-down                                 |

## Helm

| Key      | Function                |
| :---     | :---                    |
| C-x r b  | helm-filtered-bookmarks |
| C-x C-r  | helm-recentf            |
| M-\<f6\> | helm-do-ag              |
| M-s      | helm-do-ag-this-file    |
| C-u M-s  | helm-do-ag-buffers      |
| C-c C-;  | helm-eshell-prompts     |
| C-c C-l  | helm-eshell-history     |

## Multiple Cursors

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

## Org-mode

| Key    | Function                                       |
| :---   | :---                                           |
| C-c e  | org-encrypt-entry                              |
| C-c d  | org-decrypt-entry                              |
| C-c a  | org-agenda                                     |
| C-c c  | org-capture                                    |
| C-c i  | org-insert-heading                             |
| C-c j  | galactic-emacs-org-show-current-heading-tidily |
| \<f6\> | galactic-emacs-org-directory-search-ag         |

## Smartparens

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
| C-M-\<right\>   | sp-forward-slurp-sexp  |
| C-M-\<left\>    | sp-forward-barf-sexp   |
| C-S-M-\<left\>  | sp-backward-slurp-sexp |
| C-S-M-\<right\> | sp-backward-barf-sexp  |
| M-(             | sp-wrap-round          |
| M-[             | sp-wrap-square         |
| M-{             | sp-wrap-curly          |

