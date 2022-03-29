;;; programming.el --- Galactic Emacs programming packages -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2022 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/programming.el
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

;; Galactic Emacs selection of packages for programming.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


;; Automatically debug and bisect your init file
(use-package bug-hunter
  :ensure t
  :defer t)

;; cider - Clojure Interactive Development Environment
(use-package cider
  :ensure t
  :defer t
  :ensure-system-package (lein . leiningen)
  :config
  (setq cider-allow-jack-in-without-project t))

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; A convenient frontend to GHCi
(use-package dante
  :ensure t
  :defer t
  :after haskell-mode
  :commands 'dante-mode
  :hook
  (haskell-mode . flycheck-mode)
  (haskell-mode . dante-mode))

;; Debug Adapter Protocol for Emacs
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; DAP Adapter for java
(use-package dap-java
  :after (lsp-java))

;; A Dockerfile mode for Emacs
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

;; EIN - Emacs IPython Notebook
(use-package ein
  :ensure t
  :config
  (setq ein:output-area-inlined-images t))

;; ElDoc
(use-package eldoc
  :defer t
  :diminish eldoc-mode)

;; elpy
;;
;; Please install the following packages before running elpy:
;;
;; Python2: pip2 install jedi flake8 autopep8
;; Python3: pip3 install jedi flake8 autopep8
;;
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :diminish elpy-mode
  :hook
  (python-mode . elpy-mode)
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

;; Geiser is a generic Emacs/Scheme interaction mode, featuring an
;; enhanced REPL
(use-package geiser
  :ensure t
  :defer t)

;; Haskell mode
(use-package haskell-mode
  :ensure t
  :defer t)

;; Emacs client/library for the Language Server Protocol
(use-package lsp-mode
  :ensure t
  :defer t)

;; Provides integration between lsp-mode and treemacs
(use-package lsp-treemacs
  :ensure t
  :after (treemacs))

;; Emacs Java IDE using Eclipse JDT Language Server
(use-package lsp-java
  :ensure t
  :after lsp-mode lsp-treemacs
  :hook
  (java-mode . lsp))

;; Higher level UI modules of lsp-mode, like flycheck support and code
;; lenses.
(use-package lsp-ui
  :ensure t
  :defer t)

;; jinja2-mode
(use-package jinja2-mode
  :ensure t
  :defer t
  :hook
  (jinja2-mode . (lambda ()
                   (setq show-trailing-whitespace t)
                   (flyspell-prog-mode)
                   (superword-mode 1)))
  :mode
  (("\\.j2\\'" . jinja2-mode)))

;; A reformat tool for JSON (required by json-mode)
(use-package json-reformat
  :ensure t
  :defer t)

;; Get the path to a JSON element in Emacs (required by json-mode)
(use-package json-snatcher
  :ensure t
  :defer t)

;; Major mode for editing JSON files
(use-package json-mode
  :ensure t
  :defer t
  :requires (json-reformat json-snatcher))

;; macrostep: interactive macro-expander
(use-package macrostep
  :defer t
  :ensure t
  :bind
  ("\C-ce" . macrostep-expand))

;; magit
(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-repository-directories
        (list '("~/.emacs.d" . 1 )
              '("~/.emacs.d/elpa" . 1 )
              '("~/.dotfiles" . 1 )
              '("~/org" . 1 )))
  (if (boundp 'galactic-emacs-magit-custom-repository-directories)
      (setq magit-repository-directories
            (append magit-repository-directories
                    galactic-emacs-magit-custom-repository-directories)))
  :config
  ;; Expand "unpushed to upstream or recent" magit section
  (push (cons [unpushed status] 'show) magit-section-initial-visibility-alist)
  ;; Displaying commit date and time in magit status
  ;; (setq magit-status-margin
  ;;    '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  :bind
  ("<f2>" . magit-status)
  ("<f5>" . magit-list-repositories))

;; magit-org-todos - Get todo.org into your magit status.
(use-package magit-org-todos
  :ensure t
  :config
  (magit-org-todos-autoinsert))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t
  :ensure-system-package (markdown . "brew install markdown || sudo apt-get install markdown")
  :hook
  (markdown-mode . (lambda ()
                     (setq show-trailing-whitespace t)
                     (flyspell-prog-mode)
                     (superword-mode 1)))
  :config
  (set-face-attribute 'markdown-code-face nil :background "#282C34")
  (set-face-attribute 'markdown-code-face nil :foreground "#ABB2BF"))

;; markdown-toc
(use-package markdown-toc
  :ensure t
  :defer t)

;; Octave-mode
(use-package octave
  :config
  ;; Enable octave-mode for .m files
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

;; A linter for the metadata in Emacs Lisp files which are intended to
;; be packages
(use-package package-lint
  :ensure t
  :defer t)

;; projectile
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; py-autopep8
(use-package py-autopep8
  :after elpy-mode
  :ensure t
  :defer t
  :hook
  ;; Configure elpy autopep8 support
  (elpy-mode . py-autopep8-enable-on-save))

;; re-builder
(use-package re-builder
  :defer t
  :config
  ;; Set re-builder default syntax to 'string
  (setq reb-re-syntax 'string))

;; ruby-mode
(use-package ruby-mode
  :defer t
  :config
  (add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'make-it-local))))

;; Sylvester the Cat's Common Lisp IDE
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

;; Minor mode for Emacs that deals with parens pairs and tries to be
;; smart about it.
(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  :hook
  (emacs-lisp-mode       . smartparens-mode)
  (lisp-interaction-mode . smartparens-mode)
  (clojure-mode          . smartparens-mode)
  (cider-repl-mode       . smartparens-mode)
  (sly-mrepl-mode        . smartparens-mode)
  :bind (
         :map smartparens-mode-map
         ("C-M-f"         . sp-forward-sexp)
         ("C-M-b"         . sp-backward-sexp)
         ("C-M-u"         . sp-up-sexp)
         ("C-M-d"         . sp-down-sexp)
         ("C-M-a"         . sp-beginning-of-sexp)
         ("C-M-e"         . sp-end-of-sexp)
         ("C-M-n"         . sp-next-sexp)
         ("C-M-p"         . sp-previous-sexp)
         ("C-M-t"         . sp-transpose-sexp)
         ("C-M-k"         . sp-kill-sexp)
         ("C-M-w"         . sp-copy-sexp)
         ("M-<backspace>" . sp-unwrap-sexp)
         ("C-M-<right>"   . sp-forward-slurp-sexp)
         ("C-M-<left>"    . sp-forward-barf-sexp)
         ("C-S-M-<left>"  . sp-backward-slurp-sexp)
         ("C-S-M-<right>" . sp-backward-barf-sexp)
         ("M-("           . sp-wrap-round)
         ("M-["           . sp-wrap-square)
         ("M-{"           . sp-wrap-curly)))

;; A regexp/replace command for Emacs with interactive visual feedback
(use-package visual-regexp
  :ensure t
  :defer t)

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :defer t
  :hook
  (yaml-mode . (lambda ()
                 (define-key yaml-mode-map "\C-m" 'newline-and-indent)
                 (setq show-trailing-whitespace t)
                 (flyspell-prog-mode)
                 (superword-mode 1)))
  :mode "\\.\\(yml\\|knd\\'\\)")

;; YANG mode
(use-package yang-mode
  :ensure t
  :defer t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"           ;; AndreaCrotti/yasnippet-snippets
          "~/.emacs.d/snippets-addons"))  ;; Personal snippets
  (yas-reload-all))


;;; programming.el ends here
