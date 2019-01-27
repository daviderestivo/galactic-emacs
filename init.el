;;; init.el --- An Emacs init file

;;
;; Copyright (C) 2016-2019 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/emacs-config
;; Version: 6.0.0
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

;; This is my Emacs config file. Below you can find the list of the
;; packages used:
;;
;;  `Narrowing'
;; - helm                                [https://github.com/emacs-helm/helm]
;; - helm-ag                             [https://github.com/syohex/emacs-helm-ag]
;; - helm-descbinds                      [https://github.com/emacs-helm/helm-descbinds]
;; - helm-github-stars                   [https://github.com/Sliim/helm-github-stars]
;; - helm-projectile                     [https://github.com/bbatsov/helm-projectile]
;;
;;  `Networking'
;; - cisco-router-mode                   [https://www.emacswiki.org/emacs/download/cisco-router-mode.el]
;;
;;  `Programming'
;; - cider                               [https://github.com/clojure-emacs/cider]
;; - company-lsp                         [https://github.com/tigersoldier/company-lsp]
;; - company-mode                        [https://github.com/company-mode/company-mode]
;; - dap-mode                            [https://github.com/yyoncho/dap-mode]
;; - dap-java                            [https://github.com/yyoncho/dap-mode]
;; - diff-hl                             [https://github.com/dgutov/diff-hl]
;; - dockerfile-mode                     [https://github.com/spotify/dockerfile-mode]
;; - elisp-bug-hunter                    [https://github.com/Malabarba/elisp-bug-hunter]
;; - elpy                                [https://elpy.readthedocs.io]
;; - geiser                              [https://gitlab.com/jaor/geiser]
;; - lsp-mode                            [https://github.com/emacs-lsp/lsp-mode]
;; - lsp-java                            [https://github.com/emacs-lsp/lsp-java]
;; - lsp-java-treemacs                   [https://github.com/emacs-lsp/lsp-java]
;; - lsp-ui                              [https://github.com/emacs-lsp/lsp-ui]
;; - jinja2-mode                         [https://github.com/paradoxxxzero/jinja2-mode]
;; - json-mode                           [https://github.com/joshwnj/json-mode]
;; - magit                               [https://magit.vc]
;; - magit-org-todos                     [https://github.com/danielma/magit-org-todos.el]
;; - markdown-mode                       [http://jblevins.org/projects/markdown-mode]
;; - projectile                          [https://github.com/bbatsov/projectile]
;; - py-autopep8                         [https://github.com/paetzke/py-autopep8.el]
;; - smartparens                         [https://github.com/Fuco1/smartparens]
;; - treemacs-projectile                 [https://github.com/Alexander-Miller/treemacs]
;; - yaml-mode                           [https://github.com/yoshiki/yaml-mode]
;; - yang-mode                           [https://github.com/mbj4668/yang-mode]
;; - yasnippet                           [https://github.com/joaotavora/yasnippet]
;;
;;  `Org'
;; - ob-ipython                          [https://github.com/gregsexton/ob-ipython]
;; - org-beautify-theme                  [https://github.com/jonnay/org-beautify-theme]
;; - org-bullets                         [https://github.com/sabof/org-bullets]
;; - org-download                        [https://github.com/abo-abo/org-download]
;; - idle-org-agenda                     [https://github.com/enisozgen/idle-org-agenda]
;; - org-mind-map                        [https://github.com/theodorewiles/org-mind-ma]
;; - org-plus-contrib                    [http://orgmode.org]
;;
;;  `Visual'
;; - all-the-icons                       [https://github.com/domtronn/all-the-icons.el]
;; - atom-one-dark-theme                 [https://github.com/jonathanchu/atom-one-dark-theme]
;; - emacs-dashboard                     [https://github.com/rakanalh/emacs-dashboard]
;; - eyebrowse                           [https://github.com/wasamasa/eyebrowse]
;; - ibuffer-sidebar                     [https://github.com/jojojames/ibuffer-sidebar]
;; - imenu-list                          [https://github.com/bmag/imenu-list]
;; - rainbow-delimiters                  [https://www.emacswiki.org/emacs/RainbowDelimiters]
;; - smart-mode-line                     [https://github.com/Malabarba/smart-mode-line]
;; - smart-mode-line-atom-one-dark-theme [https://github.com/daviderestivo/smart-mode-line-atom-one-dark-theme]
;; - transpose-frame                     [https://www.emacswiki.org/emacs/TransposeFrame]
;; - treemacs-icons-dired                [https://github.com/Alexander-Miller/treemacs]
;; - volatile-highlights                 [https://github.com/k-talo/volatile-highlights.el]
;;
;;  `Various'
;; - auto-package-update                 [https://github.com/rranelli/auto-package-update.el]
;; - auto-sudoedit                       [https://github.com/ncaq/auto-sudoedit]
;; - avy                                 [https://github.com/abo-abo/avy]
;; - command-log-mode                    [https://github.com/lewang/command-log-mode]
;; - esh-autosuggest                     [https://github.com/dieggsy/esh-autosuggest]
;; - exec-path-from-shell                [https://github.com/purcell/exec-path-from-shell]
;; - load-bash-alias                     [https://github.com/daviderestivo/load-bash-alias]
;; - multiple-cursors                    [https://github.com/magnars/multiple-cursors.el]
;; - psession                            [https://github.com/thierryvolpiatto/psession]
;; - shell-pop-el                        [https://github.com/kyagi/shell-pop-el]
;; - treemacs                            [https://github.com/Alexander-Miller/treemacs]
;; - undo-tree                           [https://github.com/emacsmirror/undo-tree]
;; - use-package                         [https://github.com/jwiegley/use-package]
;; - which-key                           [https://github.com/justbur/emacs-which-key]
;; - wttrin                              [https://github.com/bcbcarl/emacs-wttrin]
;;
;; Feel free to drop me an email in case of questions.


;;; Improve Emacs startup time

;;
;; PLEASE DO NOT ADD ANY CODE `BEFORE' THIS SECTION
;;
;; Avoid garbage collection during startup. The GC eats up quite a bit
;; of time, easily doubling the startup time. The trick is to turn up
;; the memory threshold (500 MB should be sufficient) in order to
;; prevent it from running during startup.
(setq gc-cons-threshold (* 500 1024 1024)
      gc-cons-percentage 0.6)

;; After Emacs startup has been completed, set `gc-cons-threshold' to
;; 16 MB and reset `gc-cons-percentage' to its original value.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Log Emacs startup time in *Messages*
(add-hook 'emacs-startup-hook
          (lambda ()
            (message (format "Emacs startup time: %s" (emacs-init-time)))))


;;; General configuration section

;; Configure Emacs package manager. Not required anymore on Emacs 27
(if (version< emacs-version "27")
    (package-initialize))
(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

;; Change the below priorities if you prefer melpa-stable packages.
;; Higher is better.
(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package use-package-ensure-system-package
  :ensure t)
(setq package-enable-at-startup nil)

;; Bootstrap `diminish'
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))
(eval-when-compile
  (require 'diminish))

(require 'bind-key)

;; Tell Emacs where is your personal elisp lib directory
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Tell Emacs where is your personal theme directory
(add-to-list 'custom-theme-load-path (expand-file-name "themes"
                                                       user-emacs-directory))

;; Save custom variables to custom.el
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Load helper functions
(load-file (expand-file-name "conf/helper-functions.el"
                             user-emacs-directory))

;; Load auto-backup settings
(load-file (expand-file-name "conf/backup.el"
                             user-emacs-directory))

;; Load visual settings
(load-file (expand-file-name "conf/visual.el"
                             user-emacs-directory))

;; Load various settings
(load-file (expand-file-name "conf/various.el"
                             user-emacs-directory))

;; Load packages
(load-file (expand-file-name "conf/packages.el"
                             user-emacs-directory))

;; Load flyspell settings
(load-file (expand-file-name "conf/flyspell.el"
                             user-emacs-directory))


;;; init.el ends here
