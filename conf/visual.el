;;; visual.el --- Galactic Emacs visual packages and settings -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2022 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/visual.el
;; Version: 12.0.0
;; Keywords: emacs config


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

;; Galactic Emacs visual packages and settings.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


;; Inhibit startup screen, splash screen and startup message
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-message t)

;; Enable visual line fringe and empty line indicator
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil
              indicate-empty-lines t
              indent-tabs-mode nil)

;; Emacs resizes the (GUI) frame when your newly set font is larger
;; (or smaller) than the system default. This seems to add 0.4-1s to
;; startup.
(setq frame-inhibit-implied-resize t)

;; Enable pixel-scroll-precision-mode on Emacs 29 and above
(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode))

;; Line and column numbers
(if (version< emacs-version "26.1")
    (global-linum-mode)
  (global-display-line-numbers-mode t))
(column-number-mode t)

;; Blinking cursor
(blink-cursor-mode t)

;; Change cursor type to vertical bar
(setq-default cursor-type 'box)

;; Use visual bell instead of audio
(setq visible-bell 1)

;; Enable visual-line-mode globally
(global-visual-line-mode t)
(diminish 'visual-line-mode)

;; Set Emacs frame size and transparency
(setq galactic-emacs-frame-height 60)
(setq galactic-emacs-frame-width 130)
(setq galactic-emacs-frame-alpha '(96 96))

;; Set default font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-14"))

;; The latter does more work than the former, under the hood.
;; (set-face-attribute 'default nil
;;                     :family "DejaVu Sans Mono"
;;                     :height 140)

;; Show trailing white-spaces
;; Type M-x delete-trailing-whitespace to delete all trailing
;; white-space. This command deletes all extra spaces at the
;; end of each line in the buffer, and all empty lines at the
;; end of the buffer.
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                python-mode-hook
                js-mode-hook))
  (add-hook mode
            (lambda ()
              (setq show-trailing-whitespace t))))

;; Turn on highlighting current line
(global-hl-line-mode 1)

;; Disable the toolbar and the scroll-bar. Press F9 to enable the
;; scroll-bar
(if (or (display-graphic-p) (daemonp))
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (global-set-key (kbd "<f9>") #'toggle-scroll-bar)))

;; Enable show-paren-mode. paren-mode allows one to see
;; matching pairs of parentheses and other characters.
(show-paren-mode 1)
(setq show-paren-delay 0.5)

;; When prettify-symbols-mode and font-locking are enabled,
;; symbols are prettified (displayed as composed characters)
;; according to the rules in `prettify-symbols-alist'
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (progn
              (setq prettify-symbols-unprettify-at-point 'right-edge)
              (prettify-symbols-mode))))

;; Unset the frame title and remove the icon
(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)

;; Avoid gaps between windows when tiling, unless the currently used
;; typeface is exactly aligned with the effective display area.
;; Link: https://github.com/d12frosted/homebrew-emacs-plus/issues/130
(setq frame-resize-pixelwise t)

;; Sort apropos results by relevancy
(setq apropos-sort-by-scores t)

;; Datetime format
(setq display-time-day-and-date t
      display-time-24hr-format t)

;; Set tab width to 4
(setq tab-width 4)

;; By default, Emacs thinks a sentence is a full-stop followed by 2
;; spaces. Let’s make it full-stop and 1 space.
(setq sentence-end-double-space nil)

;; Emacs has the built-in DocView mode which lets you view PDFs. The
;; below setting allows continue scrolling
(setq doc-view-continuous t)

;; Enable winner mode
;; Winner Mode is a global minor mode. When activated, it allows
;; you to “undo” (and “redo”) changes in the window configuration
;; with the key commands ‘C-c left’ and ‘C-c right’.
(winner-mode t)

;; Insert right brackets when left one is typed
(electric-pair-mode 1)

;; Set initial *scratch* buffer message and set major mode to
;; lisp-interaction
(setq initial-scratch-message (with-temp-buffer
                                (insert-file-contents
                                 (expand-file-name "scratch-ascii-art.txt"
                                                   user-emacs-directory))
                                (buffer-string)))
(with-current-buffer
    (get-buffer "*scratch*")
  (lisp-interaction-mode))

;; Customize Emacs calendar to start a week on Monday and to show the week number
(setq calendar-week-start-day 1)
(copy-face 'default 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :foreground "light green")
(setq calendar-intermonth-text
      '(propertize
        (format "%2d" (car
                       (calendar-iso-from-absolute
                        (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))
(setq calendar-intermonth-header (propertize "Wk"))

;; All the icons
(when (window-system) ; Available only in GUI mode
  (progn
    ;; The fonts installation runs only the first time all-the-icons is
    ;; downloaded.
    (setq galactic-emacs-all-the-icons-first-run t)
    ;; Check if this is the first run
    (when (car (file-expand-wildcards
                (concat user-emacs-directory "elpa/all-the-icons-*")))
      (setq galactic-emacs-all-the-icons-first-run nil))
    ;; If this is the first run we download all-the-icons package and
    ;; the related fonts
    (when galactic-emacs-all-the-icons-first-run
      (use-package all-the-icons
        :ensure t
        :init
        (all-the-icons-install-fonts t))))
  (use-package all-the-icons
    :ensure t))

;; atom-one-dark-theme
(use-package atom-one-dark-theme
  :ensure t
  :init
  (galactic-emacs-setup-frame-appearance)
  (add-hook 'after-make-frame-functions 'galactic-emacs-setup-frame-appearance 'append)
  :config
  ;; The below theme is used both for the case of Emacs running GUI
  ;; mode
  (when (display-graphic-p)
    (load-theme 'atom-one-dark)))

;; Automatic highlighting current symbol minor mode
(use-package auto-highlight-symbol
  :ensure t
  :diminish auto-highlight-symbol-mode
  :config
  (global-auto-highlight-symbol-mode t)
  (setq ahs-idle-interval 0.5))

;; Never lose your cursor again
(use-package beacon
  :ensure t
  ;; beacon is no more working on Emacs < 27
  :if (not (version< emacs-version "27"))
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;; An extensible Emacs startup screen showing you what’s most important
(use-package dashboard
  :ensure t
  :diminish page-break-lines-mode
  :config
  (dashboard-setup-startup-hook)
  ;; Configure initial-buffer-choice to show the dashboard in frames
  ;; created with `emacsclient -c'
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; Set the banner logo text
  (setq dashboard-banner-logo-title "Welcome to Galactic Emacs")
  ;; Set banner footer text
  (setq dashboard-set-footer t)
  (setq dashboard-footer  "\"To succeed, planning alone is insufficient. One must improvise as well.\" - I. Asimov, Foundation")
  ;; Set an alternate Emacs logo
  (setq dashboard-startup-banner (expand-file-name "logos/galactic-emacs-logo.png"
                                                   user-emacs-directory))
  ;; Customize banner font
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  ;; Show info about the packages loaded and the init time
  (setq dashboard-set-init-info t)
  ;; Add icons to the widget headings and their items
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; Customize the buttons of the navigator bar
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(((,(when (display-graphic-p)
               (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
            "Homepage"
            "Visit project Homepage"
            (lambda (&rest _) (browse-url "https://github.com/daviderestivo/galactic-emacs")))
           (,(when (display-graphic-p)
               (all-the-icons-octicon "repo-pull" :height 1.1 :v-adjust 0.0))
            "Update Galactic Emacs"
            "Update Galactic Emacs"
            (lambda (&rest _) (galactic-emacs-update-config)))
           (,(when (display-graphic-p)
               (all-the-icons-faicon "archive" :height 1.1 :v-adjust 0.0))
            "Update Installed Packages"
            "Update Installed Packages"
            (lambda (&rest _) (galactic-emacs-update-packages)))))))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :config
  ;; Customize active/inactive mode-line colors
  (set-face-attribute 'mode-line nil
                      :background "#2C323C"
                      :box '(:line-width 3 :color "#2C323C"))
  (set-face-attribute 'mode-line-inactive nil
                      :background "#282C34"
                      :box '(:line-width 3 :color "#282C34"))
  (display-battery-mode)
  (doom-modeline-mode 1))

;; diff-hl
(use-package diff-hl
  :ensure t
  :hook
  ;; Highlight changed files in the fringe of dired
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :diminish diff-hl-mode
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (display-graphic-p)
                      (global-diff-hl-mode)
                    (progn
                      (setq diff-hl-side 'right)
                      (global-diff-hl-mode)
                      (diff-hl-margin-mode)))))
    ;; Emacs not running in daemon mode
    (if (display-graphic-p)
        (global-diff-hl-mode)
      (progn
        (setq diff-hl-side 'right)
        (global-diff-hl-mode)
        (diff-hl-margin-mode)))))

;; Temporarily disabling font-lock and switching to a barebones
;; mode-line, until you stop scrolling (at which point it re-enables).
(use-package fast-scroll
  :ensure t
  :diminish fast-scroll-mode
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

(use-package ediff
  :config
  ;; Split horizontally and avoid floating ediff window
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function (quote ediff-setup-windows-plain)))

;; Global minor mode for Emacs that allows you to manage your window
;; configurations in a simple manner, just like tiling window managers.
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode)
  ;; Display the *scratch* buffer for every newly created workspace
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-mode-line-right-delimiter "] "))

;; gnutls customization
;;
;; Please look at: https://blogs.fsfe.org/jens.lechtenboerger/2014/03/23/certificate-pinning-for-gnu-emacs/
(use-package gnutls
  :defer t
  :ensure-system-package (gnutls-cli . "brew install gnutls || sudo apt-get install gnutls-bin")
  :config
  (setq tls-program '("gnutls-cli -p %p %h")
        imap-ssl-program '("gnutls-cli -p %p %s")
        smtpmail-stream-type 'starttls))

;; Preview line when executing goto-line command
(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

;; highlight-indentation-mode
(use-package highlight-indentation
  :ensure t
  :diminish highlight-indentation-mode
  :config
  (set-face-attribute 'highlight-indentation-face nil
                      :background "gray18")
  (set-face-attribute 'highlight-indentation-current-column-face nil
                      :background "gray18"))

;; This minor mode highlights indentation levels via font-lock
(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'column)
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-odd-face-perc 2)
  (setq highlight-indent-guides-auto-even-face-perc 1)
  (setq highlight-indent-guides-auto-character-face-perc 4))

;; ibuffer-sidebar
(if (not (version< emacs-version "25.1"))
    (use-package ibuffer-sidebar
      :ensure t
      :defer t
      :hook
      (ibuffer-mode . galactic-emacs-disable-number-and-visual-line)
      :config
      (setq ibuffer-sidebar-use-custom-font nil)
      :bind
      ("C-<f12>" . ibuffer-sidebar-toggle-sidebar)))

;; imenu-list
(use-package imenu-list
  :ensure t
  :defer t
  :hook
  (imenu-list-major-mode . galactic-emacs-disable-number-and-visual-line)
  :config
  (setq imenu-list-position 'right
        imenu-list-auto-resize t)
  :bind
  ("<f12>" . imenu-list-smart-toggle))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode       . rainbow-delimiters-mode)
  (cider-repl-mode . rainbow-delimiters-mode))

;; transpose-frame
;; https://www.emacswiki.org/emacs/TransposeFrame
(use-package transpose-frame)

;; A tree layout file explorer for Emacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :hook
  ;; Allow treemacs window to be resized and disable line numbers
  (treemacs-mode .
                 (lambda () (progn
                              (treemacs-toggle-fixed-width)
                              (if (version< emacs-version "26.1")
                                  (linum-mode)
                                (display-line-numbers-mode))
                              (setq display-line-numbers nil))))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-asc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("M-<f12>"   . treemacs)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)))

;; Allows you to use treemacs icons in dired buffers with
;; treemacs-icons-dired-mode
(use-package treemacs-icons-dired
  :ensure t
  :config (treemacs-icons-dired-mode))

;; A small utility package to fill the small gaps left by using
;; filewatch-mode and git-mode in conjunction with magit.
(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; Allows to quickly add your projectile projects to the treemacs
;; workspace by calling `treemacs-projectile'
(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; whitespace - Highlight lines that exceed 80 chars length
(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (set-face-attribute 'whitespace-line nil :background "gray20" :foreground "dark gray")
  ;; whitespace-mode is not compatible with magit. Disabling it on
  ;; magit-mode.
  (defun galactic-emacs-prevent-whitespace-mode-for-magit ()
    (not (derived-mode-p 'magit-mode)))
  (add-function :before-while whitespace-enable-predicate 'galactic-emacs-prevent-whitespace-mode-for-magit)
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-global-modes '(not org-mode lisp-interaction-mode))
  (global-whitespace-mode t))


;;; visual.el ends here
