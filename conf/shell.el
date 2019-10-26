;;; shell.el --- Galactic Emacs shell configuration

;;
;; Copyright (C) 2016-2019 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/shell.el
;; Version: 11.0.0
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

;; A Emacs shell configuration.


;; Eshell
(use-package eshell
  :hook
  (eshell-exit . delete-window)
  (eshell-mode . (lambda ()
                   ;; (setq eshell-destroy-buffer-when-process-dies t)
                   ;; Programs that need special displays
                   (setq helm-eshell-fuzzy-match t)
                   (eshell-cmpl-initialize)
                   ;; Date: Sat Sep  8 08:33:37 CEST 2018 - Comment it out because it's buggy in Emacs 27
                   ;;(define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                   (add-to-list 'eshell-visual-subcommands '("git" "diff" "help" "log" "show"))
                   (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)
                   (define-key eshell-mode-map (kbd "C-c C-;")  'helm-eshell-prompts)))

  :config
  ;; Eshell prompt customization
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'galactic-emacs-eshell-prompt))

;; Fish-like autosuggestions in eshell
(use-package esh-autosuggest
  :ensure t
  :defer t
  :hook
  (eshell-mode . esh-autosuggest-mode))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  ;; http://stackoverflow.com/questions/35286203/exec-path-from-shell-message-when-starting-emacs
  (when (string= system-type "darwin")
    (exec-path-from-shell-initialize)))

;; Convert bash aliases into eshell ones
(use-package load-bash-alias
  :ensure t
  :defer t
  :config
  (setq load-bash-alias-bashrc-file "~/.bashrc")
  (setq load-bash-alias-exclude-aliases-regexp "^alias magit\\|^alias oc"))

;; Pop-up a shell
(use-package shell-pop
  :ensure t
  :init
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))
        shell-pop-term-shell "eshell"
        shell-pop-universal-key (kbd "C-x t")
        shell-pop-window-size 50
        shell-pop-full-span nil
        shell-pop-window-position "bottom"))


;;; shell.el ends here
