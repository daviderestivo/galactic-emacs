# Key bindings

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Key bindings](#key-bindings)
    - [Eshell Mode](#eshell-mode)
    - [Helm Mode](#helm-mode)
    - [Magit Mode](#magit-mode)
    - [Multiple Cursors Mode](#multiple-cursors-mode)
    - [Org Mode](#org-mode)
    - [Smartparens Mode](#smartparens-mode)
    - [Winner Mode](#winner-mode)
    - [Variuos](#variuos)

<!-- markdown-toc end -->


## Eshell Mode

| Key     | Function                             |
|:--------|:-------------------------------------|
| C-c s p | shell-pop-eshell                     |
| C-c s n | create new eshell interactive buffer |
| C-l     | eshell/clear                         |
| C-c C-l | helm-eshell-history                  |
| C-c C-; | helm-eshell-prompts                  |

## Helm Mode

| Key       | Function                |
|:----------|:------------------------|
| M-x       | helm-M-x                |
| M-y       | helm-show-kill-ring     |
| C-o       | helm-next-source        |
| C-x b     | helm-mini               |
| C-x r b   | helm-filtered-bookmarks |
| C-x C-f   | helm-find-files         |
| C-x C-r   | helm-recentf            |
| C-c h x   | helm-register           |
| C-c C-SPC | helm-mark-ring          |
| C-c h SPC | helm-all-mark-rings     |
| M-\<f6\>  | helm-do-ag              |
| M-s       | helm-do-ag-this-file    |
| C-u M-s   | helm-do-ag-buffers      |
| C-c C-;   | helm-eshell-prompts     |
| C-c C-l   | helm-eshell-history     |

## Magit Mode

| Key    | Function                |
|:-------|:------------------------|
| \<f2\> | magit-status            |
| \<f5\> | magit-list-repositories |

## Multiple Cursors Mode

| Key             | Function                          |
|:----------------|:----------------------------------|
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

## Org Mode

| Key    | Function                                       |
|:-------|:-----------------------------------------------|
| C-c a  | org-agenda                                     |
| C-c b  | org-fold-hide-block-all                        |
| C-c c  | org-capture                                    |
| C-c d  | org-decrypt-entry                              |
| C-c e  | org-encrypt-entry                              |
| C-c i  | org-insert-heading                             |
| C-c j  | galactic-emacs-org-show-current-heading-tidily |
| \<f6\> | galactic-emacs-org-directory-search-ag         |
| \<f7\> | create new empty buffer (org-mode)             |

## Smartparens Mode

| Key             | Function               |
|:----------------|:-----------------------|
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

## Winner Mode

| Key           | Function                           |
|:--------------|:-----------------------------------|
| C-c \<left\>  | winner mode - Undo windows changes |
| C-c \<right\> | winner mode - Redo windows changes |

## Variuos

| Key            | Function                                       |
|:---------------|:-----------------------------------------------|
| \<f8\>         | ispell-word                                    |
| M-\<f8\>       | flyspell-goto-next-error                       |
| \<f9\>         | toggle sroll bar                               |
| \<f12\>        | imenu-list-smart-toggle                        |
| M-\<f12\>      | treemacs                                       |
| C-\<f12\>      | ibuffer-sidebar-toggle-sidebar                 |
| s-/            | avy-goto-char                                  |
| C-=            | copy-line                                      |
| C-c q          | gptel                                          |
| C-c r p        | parrot-rotate-prev-word-at-point               |
| C-c r n        | parrot-rotate-next-word-at-point               |
| C-m            | YAML mode - New line and indent                |
| C-c p          | projectile keymap                              |
| M-+            | galactic-emacs-insert-date                     |
| M-o            | other-window                                   |
| M-up           | move-text-up                                   |
| M-down         | move-text-down                                 |
| F (dired-mode) | dired-create-empty-file (Require GNU/Emacs 27) |
