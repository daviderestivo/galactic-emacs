;;; narrowing.el --- Galactic Emacs narrowing configuration -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2020 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/narrowing.el
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

;; A narrowing configuration based on Helm.


;; helm - Helm is an incremental completion and selection narrowing
;; framework for Emacs.
(use-package helm
  :ensure t
  :hook
  (helm-minibuffer-set-up . galactic-emacs-helm-hide-minibuffer-maybe)
  :diminish helm-mode
  :commands helm-mode
  :config
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (helm-mode 1)
  ;; Increase max length of buffer names (default 20) to the longest
  ;; buffer-name length found.
  (setq helm-buffer-max-length nil)
  ;; Enable fuzzy matching
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-M-x-always-save-history t)
  ;;--------------------------------------------------------------------------;;
  ;;       Work with Spotlight on macOS instead of the regular locate         ;;
  ;;--------------------------------------------------------------------------;;
  (if (string= system-type "darwin")
      (progn
        (setq galactic-emacs-helm-locate-spotlight-command "mdfind -name -onlyin ~ %s %s")
        (setq galactic-emacs-helm-locate-exclude-dirs "~/Library")
        (setq galactic-emacs-helm-locate-exclude-command " | egrep -v ")
        (setq helm-locate-command
              (concat galactic-emacs-helm-locate-spotlight-command
                      galactic-emacs-helm-locate-exclude-command
                      galactic-emacs-helm-locate-exclude-dirs))
        (setq helm-locate-fuzzy-match nil))
    (setq helm-locate-fuzzy-match t))
  ;;--------------------------------------------------------------------------;;
  ;;     END Work with Spotlight on macOS instead of the regular locate       ;;
  ;;--------------------------------------------------------------------------;;
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-autoresize-mode t)
  ;; The below makes helm behaving nicely together with treemacs
  (setq helm-split-window-inside-p t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-follow-mode-persistent t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  ;; Replace the default helm grep command with ag.
  ;; Requires "The Silver Searcher" (ag) to be installed.
  ;; On macOS use: 'brew install the_silver_searcher'
  (when (executable-find "ag")
    ;; For helm to recognize correctly the matches we need to enable
    ;; line numbers and columns in its output, something the
    ;; --vimgrep option does.
    (setq helm-grep-default-command         "ag -i --vimgrep --nogroup --nocolor -z %p %f"
          helm-grep-default-recurse-command "ag -i --vimgrep --nogroup --nocolor -z %p %f"))
  :bind
  ;; bind keys because of this commit:
  ;; https://github.com/emacs-helm/helm/commit/1de1701c73b15a86e99ab1c5c53bd0e8659d8ede
  ("M-x"       . helm-M-x)
  ("M-y"       . helm-show-kill-ring)
  ("C-x b"     . helm-mini)
  ("C-x r b"   . helm-filtered-bookmarks)
  ("C-x C-f"   . helm-find-files)
  ("C-x C-r"   . helm-recentf)
  ("C-c h x"   . helm-register)
  ("C-c h SPC" . helm-all-mark-rings))

;; helm-ag
;; Requires "The Silver Searcher" (ag) to be installed.
;; On macOS use: 'brew install the_silver_searcher'
(use-package helm-ag
  :ensure t
  :ensure-system-package (ag . "brew install the_silver_searcher || sudo apt-get install silversearcher-ag")
  :config
  ;; Use .agignore file at project root
  (setq helm-ag-use-agignore t)
  ;; Enable  approximate string matching (fuzzy matching)
  (setq helm-ag-fuzzy-match t)
  ;; :bind together with lambdas is unsupported in use-package
  (global-set-key (kbd "M-s") '(lambda (P)
                                 (interactive "P")
                                 (if (eq P nil)
                                     (helm-do-ag-this-file)
                                   (helm-do-ag-buffers))))
  :bind
  ("M-<f6>" . helm-do-ag))

;; helm-descbinds
(use-package helm-descbinds
  :ensure t
  :defer t
  :config
  (helm-descbinds-mode))

;; Provides capabilities to fetch your starred repositories from
;; github and select one for browsing
(use-package helm-github-stars
  :ensure t
  :defer t
  :config
  (setq helm-github-stars-username galactic-emacs-github-username)
  (setq helm-github-stars-refetch-time 0.5))

;; Integration between helm and treemacs icons
(use-package helm-icons
  :ensure t
  :config
  (helm-icons-enable))

;; helm-projectile
(use-package helm-projectile
  :ensure t
  :defer t
  :config
  (helm-projectile-on))


;;; narrowing.el ends here
