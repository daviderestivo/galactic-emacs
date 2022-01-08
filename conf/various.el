;;; various.el --- Galactic Emacs various packages -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2022 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/various.el
;; Version: 12.0.0
;; Keywords: emacs config dotemacs


;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; Galactic Emacs: various packages that do not fall into any other category.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


;; auto-package-update
(use-package auto-package-update
  :ensure t
  :defer t
  :config
  ;; Automatically delete old packages
  (setq auto-package-update-delete-old-versions t))

;; When find-file and dired-mode try to access a non writable file
;; auto-sudoedit re-opens the file automatically using sudo in TRAMP
(use-package auto-sudoedit
  :ensure t
  :diminish auto-sudoedit-mode
  :config
  (auto-sudoedit-mode 1))

;; A GNU Emacs package for jumping to visible text using a char-based
;; decision tree
(use-package avy
  :ensure t
  :config
  ;; Full path before target, leaving all original text
  (setq avy-styles-alist '((avy-goto-char . pre)))
  ;; When nil, the searches does not ignore case
  (setq avy-case-fold-search nil)
  :bind
  ("s-/" . avy-goto-char))

;; Display the keys you typed in a special buffer: *command-log*
(use-package command-log-mode
  :ensure t
  :defer t)

;; Dired
(unless (version<= emacs-version "27")
  (use-package dired
    :bind
    (:map dired-mode-map
          ("F"  . dired-create-empty-file))))

;; Emacs IRC configuration
(use-package erc
  :config
  ;; Auto identify
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-autojoin-timing 'ident)
  (setq erc-user-full-name  user-full-name)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode ((,user-login-name . ,freenode-login-password)))))
  ;; Enable spell checking
  (erc-spelling-mode 1)
  ;; Enable UTF-8 support
  (setq erc-server-coding-system '(utf-8 . utf-8))
  ;; Enable mIRC-style color commands
  (setq erc-interpret-mirc-color t)
  ;; Open query buffers in the current window
  (setq erc-query-display 'buffer)
  ;; Disable keybindings track
  (setq erc-track-enable-keybindings nil)
  ;; Logging
  (require 'erc-log)
  (erc-log-mode 1)
  (setq erc-log-channels-directory
        (concat user-emacs-directory ".erc/logs/"))
  (if (not (file-exists-p erc-log-channels-directory))
      (mkdir erc-log-channels-directory t))
  (setq erc-save-buffer-on-part nil
        erc-save-queries-on-quit nil
        erc-log-write-after-send t
        erc-log-write-after-insert t)
  ;; Desktop notifications
  (require 'erc-desktop-notifications)
  (erc-notifications-enable)
  ;; Use Notification Center on macOS
  (when (string= system-type "darwin")
    (advice-add 'erc-notifications-notify :after #'galactic-emacs-erc-ns-notify))
  ;; Autoaway setup
  (setq erc-auto-discard-away t)
  (setq erc-autoaway-idle-seconds 600)
  (setq erc-autoaway-use-emacs-idle t))

;; Browse the Emacsmirror package database
(use-package epkg
  :ensure t
  :defer t)

;; Literate Calc Mode
(use-package literate-calc-mode
  :diminish literate-calc-minor-mode
  :ensure t
  :defer t)

;; MoveText allows you to move the current line using M-up / M-down
;; (or any other bindings you choose) if a region is marked, it will
;; move the region instead.
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; Multiple cursors support
(use-package multiple-cursors
  :ensure t
  :defer t
  :init
  (require 'mc-hide-unmatched-lines-mode)
  :bind
  ;; When you have an active region that spans multiple lines
  ;; mc/edit-lines will add a cursor to each line:
  ("C-S-c C-S-c"   . mc/edit-lines)
  ("C->"           . mc/mark-next-like-this)
  ("C-<"           . mc/mark-previous-like-this)
  ("M-C->"         . mc/mark-next-like-this-symbol)
  ("M-C-<"         . mc/mark-previous-like-this-symbol)
  ("C-c C-<"       . mc/mark-all-like-this)
  ("C-c C->"       . mc/mark-all-like-this-symbol)
  ("C-c C-n"       . mc/insert-numbers)
  ("C-c C-r"       . mc/reverse-regions)
  ("C-c C-s"       . mc/sort-regions)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  (:map mc/keymap
        ("<return>" . nil)))

;; Open a junk (memo) file to try-and-error
(use-package open-junk-file
  :ensure t
  :defer t
  :config
  (setq open-junk-file-format "~/iCloud/emacs/junk/%Y/%m/%d-%H%M%S."))

;; A package to rotate text and party with parrots at the same time
(use-package parrot
  :diminish parrot-mode
  :ensure t
  :init
  (setq parrot-minimum-window-width most-positive-fixnum)
  :bind
  ("C-c r p" . parrot-rotate-prev-word-at-point)
  ("C-c r n" . parrot-rotate-next-word-at-point)
  :config
  (parrot-mode))

;; A very simple alternative to more involved session management
;; solutions
(use-package savehist
  :config
  (setq savehist-additional-variables
        '(buffer-name-history
          extended-command-history
          file-name-history
          kill-ring
          regexp-search-ring
          search-ring))
  (setq
   savehist-autosave-interval 60
   savehist-file (expand-file-name "savehist/history" user-emacs-directory))
  (savehist-mode t))

;; underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of
;; names
(use-package string-inflection
  :ensure t
  :defer t)

;; This mode adds Swiss holidays for the GNU/Emacs calendar
(use-package swiss-holidays
  :ensure t
  :config
  (setq holiday-other-holidays
	(append swiss-holidays swiss-holidays-labour-day)))

;; Synosaurus is a thesaurus fontend for Emacs with pluggable backends
(use-package synosaurus
  :ensure t
  :ensure-system-package (wn . "brew install wordnet || sudo apt-get install wordnet")
  :diminish synosaurus-mode
  :init
  (synosaurus-mode)
  :config
  (setq synosaurus-backend 'synosaurus-backend-wordnet))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  ;; Increase global undo limits to avoid undo-tree history being
  ;; truncated.
  ;;
  ;; See: https://github.com/syl20bnr/spacemacs/issues/12110
  ;; https://www.reddit.com/r/emacs/comments/bx82j3/somehow_my_undo_history_is_truncated_what_did_i/
  ;;
  ;; Default in GNU/Emacs is 80000
  (setq undo-limit 800000)
  ;; Default in GNU/Emacs is 120000
  (setq undo-strong-limit 12000000)
  ;; Default in GNU/Emacs is 12000000
  (setq undo-outer-limit 120000000)
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-diff 1)
  (setq undo-tree-visualizer-timestamps 1))

;; xwidget
(use-package xwidget
  :config
  (when (featurep 'xwidget)
    (setq xwidget-webkit-enable-plugins t)))

;; which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; wttrin - Weather application
(use-package wttrin
  :ensure t
  :defer t
  :commands (wttrin)
  :init
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
  (setq wttrin-default-cities '("Aarau"
                                "Bern"
                                "Zurich")))


;;; various.el ends here
