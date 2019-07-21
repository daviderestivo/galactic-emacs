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

## Helm

| Key     | Function                |
| :---    | :---                    |
| C-x r b | helm-filtered-bookmarks |
| C-x C-r | helm-recentf            |
| M-s     | helm-do-ag-this-file    |
| C-u M-s | helm-do-ag-buffers      |
| C-c C-; | helm-eshell-prompts     |
| C-c C-l | helm-eshell-history     |

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
| C-s-\<right\>   | sp-forward-slurp-sexp  |
| C-s-\<left\>    | sp-forward-barf-sexp   |
| C-M-\<left\>    | sp-backward-slurp-sexp |
| C-M-\<right\>   | sp-backward-barf-sexp  |
| M-(             | sp-wrap-round          |
| M-[             | sp-wrap-square         |
| M-{             | sp-wrap-curly          |
