;;; init.el - An Emacs init file

;;
;; Copyright (C) 2016-2017 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/emacs-config
;; Version: 4.0.0
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

;; This is my emacs config file. Below you can find the list of the
;; packages used:
;;
;; - atom-one-dark-theme  [https://github.com/jonathanchu/atom-one-dark-theme]
;; - auto-package-update  [https://github.com/rranelli/auto-package-update.el]
;; - cisco-router-mode    [https://www.emacswiki.org/emacs/download/cisco-router-mode.el]
;; - company-mode         [https://github.com/company-mode/company-mode]
;; - elpy                 [https://elpy.readthedocs.io]
;; - exec-path-from-shell [https://github.com/purcell/exec-path-from-shell]
;; - helm                 [https://github.com/emacs-helm/helm]
;; - jinja2-mode          [https://github.com/paradoxxxzero/jinja2-mode]
;; - magit                [https://magit.vc]
;; - markdown-mode        [http://jblevins.org/projects/markdown-mode/]
;; - org                  [http://orgmode.org]
;; - projectile           [https://github.com/bbatsov/projectile]
;; - py-autopep8          [https://github.com/paetzke/py-autopep8.el]
;; - rainbow-delimiters   [https://www.emacswiki.org/emacs/RainbowDelimiters]
;; - smart-mode-line      [https://github.com/Malabarba/smart-mode-line]
;; - transpose-frame      [https://www.emacswiki.org/emacs/TransposeFrame]
;; - use-package          [https://github.com/jwiegley/use-package]
;; - yaml-mode            [https://github.com/yoshiki/yaml-mode]
;; - wheatgrass-theme     [https://github.com/jwiegley/emacs-release/blob/master/etc/themes/wheatgrass-theme.el]
;;
;; Feel free to drop me an email in case of questions.


;;; General configuration section

;; Configure Emacs package manager
(package-initialize)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

;; Change the below priorities if you prefer melpa-stable packages. Higher is better.
(setq package-archive-priorities
      '(("org" . 4)
        ("melpa" .  3)
        ("mepla-stable" . 2)
        ("gnu" . 1)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Set LANG and LC_* variables
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;; Sort apropos results by relevancy
(setq apropos-sort-by-scores t)

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

;; Show trailing white-spaces
;; Type M-x delete-trailing-whitespace to delete all trailing
;; white-space. This command deletes all extra spaces at the
;; end of each line in the buffer, and all empty lines at the
;; end of the buffer.
(setq-default show-trailing-whitespace t)

;; Datetime format
(setq display-time-day-and-date t
      display-time-24hr-format t)

;; Set tab width to 4
(setq tab-width 4)

;; Use visual bell instead of audio
(setq visible-bell 1)

;; Set the initial major mode to text-mode
(setq initial-major-mode (quote text-mode))

;; Make *scratch* buffer blank.
(setq initial-scratch-message nil)

;; Disable the toolbar and the scroll-bar. Press F9 to enable the scroll-bar
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-set-key (kbd "<f9>") 'toggle-scroll-bar)

;; Enable show-paren-mode. paren-mode allows one to see
;; matching pairs of parentheses and other characters.
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Enable winner mode
;; Winner Mode is a global minor mode. When activated, it allows
;; you to “undo” (and “redo”) changes in the window configuration
;; with the key commands ‘C-c left’ and ‘C-c right’.
(winner-mode t)

;; Change default dir to ~
(cd "~")

;; Tell Emacs where is your personal elisp lib directory
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Save custom variables to custom.el
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))
(load custom-file 'noerror)

;; Set Emacs frame size and center it on the screen
(defvar frame-height 60)
(defvar frame-width 130)
(add-to-list 'default-frame-alist
             `(height . ,frame-height))
(add-to-list 'default-frame-alist
             `(width . ,frame-width))
(defvar frame-pixel-height
  (* frame-height (frame-char-height)))
(defvar frame-pixel-width
  (* frame-width (frame-char-width)))
(setq initial-frame-alist
;; Avoid the issue of having emacs on the middle of two displays.
      `((left . ,(/ (-
                     (round (* (display-pixel-height) 1.777))
                       frame-pixel-width) 2))
	(top .  ,(/ (-
                     ;; Remove 100px to take into account the MAC dock
                     (- (display-pixel-height) 100)  frame-pixel-height)
                    2))))

;; Backup files settings
(setq backup-directory-alist `(("." . "~/.saves"))) ;; Backup directory
(setq backup-by-copying t)  ;; Backup the file by copying it
(setq delete-old-versions t ;; delete excess backup files silently
      kept-new-versions 6   ;; oldest versions to keep when a new
			    ;; numbered backup is made (default: 2)
      kept-old-versions 2   ;; newest versions to keep when a new
			    ;; numbered backup is made (default: 2)
      version-control t     ;; version numbers for backup files
      )

;; Enable line and column numbering
(global-linum-mode t)
(setq linum-format "%d ")
(column-number-mode 1)

;; Enable word wrap
(global-visual-line-mode t)

;; Insert right brackets when left one is typed
(electric-pair-mode 1)

;; Keep a list of recently opened files
(recentf-mode 1)

;; Spell checking configuration
(setq ispell-program-name "aspell")
;; Enable flyspell for text files and enable superword mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   ;; Enable superword mode, useful for “snake_case”.
                   (superword-mode 1)
                   )))
;; Enable flyspell for code and enable superword mode
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook
                R-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode)
               ;; Enable superword mode, useful for “snake_case”.
               (superword-mode 1)
               )))
;; Add some of the ispell shortcuts:
;; - press <f8> to check a word
;; - press M-<f8> to check the next one
(global-set-key (kbd "<f8>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
;; In Mac OS X the right mouse button does not seem to trigger
;; [mouse-2], so you cannot right click a word to get a suggestion.
;; This can be fixed with the below:
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3]
       #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3]
       #'undefined)))

;; Ediff settings
;; Split horizontally and avoid floating ediff window
(setq ediff-split-window-function 'split-window-horizontally)
;; Customize ediff background colors
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-background
             ediff-even-diff-face-A "SlateGray4")
            (set-face-background
             ediff-even-diff-face-B "SlateGray4")
            (set-face-background
             ediff-even-diff-face-C "SlateGray4")
            (set-face-background
             ediff-odd-diff-face-A "SlateGray4")
            (set-face-background
             ediff-odd-diff-face-B "SlateGray4")
            (set-face-background
             ediff-odd-diff-face-C "SlateGray4")))

;; ansi-term settings
;; This tells term (which is used by ansi-term) to kill the buffer
;; after the terminal is exited.
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)
;; Always use bash
(defvar my-term-shell "/usr/local/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Sets the term to use UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)
;; URLs that show up in my terminal (via man pages, help, info,
;; errors, etc) to be clickable.
(defun my-term-hook () (goto-address-mode))
(add-hook 'term-mode-hook 'my-term-hook)

;; Enable octave-mode for .m files
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Highlight lines that exceed 80 chars length
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)

;; Setup bookmark
(setq bookmark-save-flag 1) ;; every time bookmark is changed,
                            ;; automatically save it
;; Open bookmark list at startup
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;; Automatically auto-refresh a buffer if the file has changed on disk
(global-auto-revert-mode t)

;; Enable smooth scrolling
;; Scroll one line at a time (less "jumpy" than defaults)
;; (1 ((shift) . 5)) makes scroll by 1 line at a time by default
;; and 5 lines at a time when 'Shift' key is held.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed 'nil)
;; No need to set 'mouse-wheel-follow-mouse' in Emacs 24.5
;; because it is enabled by default.
;; (setq mouse-wheel-follow-mouse 't)
;; Keyboard scroll one line at a time
(setq scroll-step 1)

;; copy-line key binding
(global-set-key "\C-c\C-c" 'copy-line)

;; Make isearch treat space dash underscore newline as same
(setq search-whitespace-regexp "[-_ \n]")


;;; Helper functions

;; Redefine battery-pmset because of https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-09/msg00952.html
(defun battery-pmset-with-fix ()
  "Get battery status information using `pmset'.

The following %-sequences are provided:
%L Power source (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%h Remaining time in hours
%m Remaining time in minutes
%t Remaining time in the form `h:min'"
  (let (power-source load-percentage battery-status battery-status-symbol
	remaining-time hours minutes)
    (with-temp-buffer
      (ignore-errors (call-process "pmset" nil t nil "-g" "ps"))
      (goto-char (point-min))
      (when (re-search-forward "\\(?:Currentl?y\\|Now\\) drawing from '\\(AC\\|Battery\\) Power'" nil t)
	(setq power-source (match-string 1))
	(when (re-search-forward "^ -InternalBattery-0\\([ \t]+\(id\=[0-9]+\)\\)*[ \t]+" nil t)
	  (when (looking-at "\\([0-9]\\{1,3\\}\\)%")
	    (setq load-percentage (match-string 1))
	    (goto-char (match-end 0))
	    (cond ((looking-at "; charging")
		   (setq battery-status "charging"
			 battery-status-symbol "+"))
		  ((< (string-to-number load-percentage) battery-load-critical)
		   (setq battery-status "critical"
			 battery-status-symbol "!"))
		  ((< (string-to-number load-percentage) battery-load-low)
		   (setq battery-status "low"
			 battery-status-symbol "-"))
		  (t
		   (setq battery-status "high"
			 battery-status-symbol "")))
	    (when (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\) remaining"  nil t)
	      (setq remaining-time (match-string 1))
	      (let ((h (string-to-number (match-string 2)))
		    (m (string-to-number (match-string 3))))
		(setq hours (number-to-string (+ h (if (< m 30) 0 1)))
		      minutes (number-to-string (+ (* h 60) m)))))))))
    (list (cons ?L (or power-source "N/A"))
	  (cons ?p (or load-percentage "N/A"))
	  (cons ?B (or battery-status "N/A"))
	  (cons ?b (or battery-status-symbol ""))
	  (cons ?h (or hours "N/A"))
	  (cons ?m (or minutes "N/A"))
	  (cons ?t (or remaining-time "N/A")))))

;; Create a new buffer without prompting for the name. Bound to F7
(defun new-empty-buffer ()
  "Create a new empty buffer. New buffer will be named “untitled” or
“untitled<2>”, “untitled<3>”, ..."
  (interactive)
  (let ((new-buf (generate-new-buffer "untitled")))
    (switch-to-buffer new-buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))
(global-set-key (kbd "<f7>") 'new-empty-buffer)

;; Open a new terminal window below the current one
(defun tb ()
  "Add terminal on the bottom"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (ansi-term "/usr/local/bin/bash"))

;; copy-line - Source https://www.emacswiki.org/emacs/CopyingWholeLines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
    Ease of use features:
     - Move to start of next line.
     - Appends the copy on sequential calls.
     - Use newline as last char even on the last line of the buffer.
     - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))


;;; Packages configuration section

;; cisco-router-mode
;; https://github.com/emacsmirror/cisco-router-mode/blob/master/cisco-router-mode.el
(load-library "cisco-router-mode")

;; transpose-frame
;; https://www.emacswiki.org/emacs/TransposeFrame
(load-library "transpose-frame")

;; atom-one-dark-theme (GUI mode)
(use-package atom-one-dark-theme
  :if window-system
  :ensure t)

;; wheatgrass-theme (Terminal mode)
(use-package wheatgrass-theme
  :if (eq window-system nil))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  ;; http://stackoverflow.com/questions/35286203/exec-path-from-shell-message-when-starting-emacs
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; magit
(use-package magit
  :ensure t
  :config
  :bind
  ("<f2>" . magit-status))

;; ORG
(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/home-various.org"
                               "~/org/work-project-BNS.org"
                               "~/org/work-project-HE.org"
                               "~/org/work-project-ME.org"
                               "~/org/work-various.org"))
  ;; Equivalent of "#+STARTUP: showeverything " on all ORG files
  (setq org-startup-folded nil)
  ;; org-mobile configuration: File where mobile captured notes will be stored
  (setq org-mobile-inbox-for-pull "~/org/mobileorg.org")
  ;; org-mobile configuration: Dropbox root directory
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  :bind
  ("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda))

;; ORG Babel: Main section
(use-package ob
  :config
  ;; Make org mode allow eval elisp, python and ruby
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ruby . t)))
  ;; Stop Emacs asking for confirmation when evaluating a code block
  (setq org-confirm-babel-evaluate nil)
  ;; Turn on syntax highlight
  (setq org-src-fontify-natively t)
  ;; Set python3 as default python interpreter
  (setq org-babel-python-command "python3"))

;; ORG Babel: Ipython section
(use-package ob-ipython
  :ensure t
  :config
  (setq ob-ipython-command "ipython3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)))
  ;; Display images inline in the same buffer
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :config
  ;; Automatically delete old packages
  (setq auto-package-update-delete-old-versions t))

;; smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup)
  (display-time-mode)
  ;; The below is a temporary fix for Emacs <= 25.2.1
  (when (memq window-system '(mac ns))
    (setq battery-status-function 'battery-pmset-with-fix
          battery-echo-area-format "Power %L, battery %B (%p%% charged, remaining time %t)"
          battery-mode-line-format " [ %b%p%% ] ")
    (display-battery-mode)))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; jinja2-mode
(use-package jinja2-mode
  :ensure t
  :config
   (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))
   ;; "C-c c" jinja2-close-tag
   ;; "C-c t" jinja2-insert-tag
   ;; "C-c v" jinja2-insert-var
   ;; "C-c #" jinja2-insert-comment
   )

;; helm
(use-package helm
  :ensure t
  :defer 1
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  :bind
  ;; bind keys because of this commit:
  ;; https://github.com/emacs-helm/helm/commit/1de1701c73b15a86e99ab1c5c53bd0e8659d8ede
  ("M-x"     . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files))

;; company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; projectile
(use-package projectile
  :ensure t
  :config
  (add-hook 'python-mode-hook 'projectile-mode))

;; elpy
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (elpy-use-ipython "ipython3")
  ;; Fix IPython5 s new prompt behavior (https://github.com/jorgenschaefer/elpy/issues/992)
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  ;; Configure py-autopep8
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; highlight-indentation-mode
(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "gray18")
  (set-face-background 'highlight-indentation-current-column-face "gray18"))

;; markdown-mode
(use-package markdown-mode
  :ensure t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"           ;; AndreaCrotti/yasnippet-snippets
          "~/.emacs.d/snippets-addons"    ;; Personal snippets
          ))
  (yas-reload-all))

;;; init.el ends here
