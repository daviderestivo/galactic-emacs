;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;
;; Add melpa, melpa-stable and marmalade package archives
;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

;;
;; Set Environment variables
;;
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;;
;; Sort apropos results by relevancy
;;
(setq apropos-sort-by-scores t)

;;
;; exec-path-from-shell
;;
;; http://stackoverflow.com/questions/35286203/exec-path-from-shell-message-when-starting-emacs
(setq exec-path-from-shell-check-startup-files nil)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;
;; Inhibit startup screen, splash screen and startup message
;;
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-message t)

;;
;; Enable visual line fringe and empty line indicator
;;
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil
              indicate-empty-lines t
              indent-tabs-mode nil)

;;
;; Show trailing whitespaces
;;
;; Type M-x delete-trailing-whitespace to delete all trailing whitespace.
;; This command deletes all extra spaces at the end of each line in the buffer,
;; and all empty lines at the end of the buffer
;;
(setq-default show-trailing-whitespace t)

;;
;; Datetime format
;;
(setq display-time-day-and-date t
      display-time-24hr-format t)

;;
;; Set tab width to 4
;;
(setq tab-width 4)

;;
;; Use visual bell instead of audio
;;
(setq visible-bell 1)

;;
;; Set the initial major mode to text-mode
;;
(setq initial-major-mode (quote text-mode))

;;
;; Make *scratch* buffer blank.
;;
(setq initial-scratch-message nil)

;;
;; Disable the Toolbar
;;
(tool-bar-mode -1)

;;
;; Disable the Scrollbar
;;
(toggle-scroll-bar -1)

;;
;; Enable show-paren-mode. paren-mode allows one to see
;; matching pairs of parentheses and other characters.
;;
(show-paren-mode 1)
(setq show-paren-delay 0)

;;
;; Enable winner mode
;;
;; Winner Mode is a global minor mode. When activated, it allows
;; you to “undo” (and “redo”) changes in the window configuration
;; with the key commands ‘C-c left’ and ‘C-c right’.
;;
(winner-mode t)

;;
;; Change default dir to ~
;;
(cd "~")

;;
;; Tell emacs where is your personal elisp lib dir
;;
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;
;; Set default font to Menlo 14pt
;;
(custom-set-faces
 '(default ((t (:height 140 :family "Menlo"))))
 )

;;
;; Add cisco-router-mode
;;
(load-library "cisco-router-mode")

;;
;; Save custom variables to custom.el
;;
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))
(load custom-file 'noerror)

;;
;; Set Emacs frame size and center it on the screen
;;
(defvar frame-height 50)
(defvar frame-width 120)
(add-to-list 'default-frame-alist `(height . ,frame-height))
(add-to-list 'default-frame-alist `(width . ,frame-width))

(defvar frame-pixel-height (* frame-height (frame-char-height)))
(defvar frame-pixel-width  (* frame-width (frame-char-width)))

(setq initial-frame-alist
;; Avoid the issue of having emacs on the middle of two displays.
      `((left . ,(/ (-
                     (round (* (display-pixel-height) 1.777))
                       frame-pixel-width) 2))
	(top .  ,(/ (-
                     ;; Remove 100px to take into account the MAC dock
                     (- (display-pixel-height) 100)  frame-pixel-height)
                    2))))

;;
;; Set Emacs startup position
;;
;; (setq initial-frame-alist '((left . 400) (top . 200)))

;;
;; Backup files settings
;;
;; Backup directory
(setq backup-directory-alist `(("." . "~/.saves")))
;; Backup the file by copying it
(setq backup-by-copying t)
(setq delete-old-versions t ; delete excess backup files silently
      kept-new-versions 6   ; oldest versions to keep when a new
			    ; numbered backup is made (default: 2)
      kept-old-versions 2   ; newest versions to keep when a new
			    ; numbered backup is made (default: 2)
      version-control t     ; version numbers for backup files
      )

;;
;; Enable line and column numbering
;;
(global-linum-mode t)
(setq linum-format "%d ")
(column-number-mode 1)

;;
;; Enable word wrap
;;
(global-visual-line-mode t)

;;
;; Insert right brackets when left one is typed
;;
(electric-pair-mode 1)

;;
;; Keep a list of recently opened files
;;
(recentf-mode 1)

;;
;; Spell checking config
;;
(setq ispell-program-name "aspell")
;;
;; Enable flyspell for text files and enable superword mode
;;
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   ;; Enable superword mode, useful for “snake_case”.
                   (superword-mode 1)
                   )))
;;
;; Enable flyspell for code and enable superword mode
;;
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
;;
;; Add some of the ispell shortcuts:
;; - press <f8> to check a word
;; - press M-<f8> to check the next one
;;
(global-set-key (kbd "<f8>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
;;
;; In Mac OS X the right mouse button does not seem to trigger
;; [mouse-2], so you cannot right click a word to get a suggestion.
;; This can be fixed with the below:
;;
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3]
       #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3]
       #'undefined)))

;;
;; Ediff settings: split horizontally and avoid floating ediff window
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ansi-term settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This tells term (which is used by ansi-term) to kill the buffer
;; after the terminal is exited.
;;
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)
;;
;; Always use bash
;;
(defvar my-term-shell "/usr/local/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
;;
;; Sets the term to use UTF-8
;;
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)
;;
;; URLs that show up in my terminal (via man pages, help, info,
;; errors, etc) to be clickable.
;;
(defun my-term-hook () (goto-address-mode))
(add-hook 'term-mode-hook 'my-term-hook)
;;
;; Open Shell in another window
;;
(defun tb ()
  "Add terminal on the bottom"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (ansi-term "/usr/local/bin/bash"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END ansi-term settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Sun Apr  2 01:13:21 CEST 2017: Disabled because of company-mode
;;
;; Settings for hippie-expand
;;
;; (global-set-key (kbd "C-<tab>") 'hippie-expand)
;; (setq hippie-expand-try-functions-list
;;        '(try-complete-lisp-symbol
;;          try-complete-lisp-symbol-partially
;;          try-expand-dabbrev
;;          try-expand-dabbrev-from-kill
;;          try-expand-dabbrev-all-buffers
;;          try-expand-line
;;          try-complete-file-name-partially
;;          try-complete-file-name))

;;
;; Create a new buffer without prompting for the name. Bound to F7
;;
(defun new-empty-buffer ()
  "Create a new empty buffer. New buffer will be named “untitled” or
“untitled<2>”, “untitled<3>”, ..."
  (interactive)
  (let ((new-buf (generate-new-buffer "untitled")))
    (switch-to-buffer new-buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))
(global-set-key (kbd "<f7>") 'new-empty-buffer)

;;
;; Add magit-status key binding
;;
(global-set-key (kbd "<f2>") 'magit-status)

;;
;; ORG config section
;;
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/org/home-various.org"
                             "~/org/work-project-BNS.org"
                             "~/org/work-project-HE.org"
                             "~/org/work-project-ME.org"
                             "~/org/work-various.org"))

;;
;; Rainbow delimiters mode config section
;;
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;
;; See custom.el for the configuration
;;

;;
;; auto-package-update - Automatically delete old packages
;;
(setq auto-package-update-delete-old-versions t)

;;
;; Enable smooth scrolling
;;
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

;;
;; Enable smart-mode-line
;;
(display-time-mode)
(display-battery-mode)
(setq sml/theme 'respectful)
(sml/setup)

;;
;; Setup transpose-frame
;;
(require 'transpose-frame)

;;
;; Setup YAML mode
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;
;; Setup Jinja2 mode
;;
(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))
;; "C-c c" jinja2-close-tag
;; "C-c t" jinja2-insert-tag
;; "C-c v" jinja2-insert-var
;; "C-c #" jinja2-insert-comment

;;
;; Setup helm
;;
(require 'helm-config)
;; bind keys because of this commit:
;; https://github.com/emacs-helm/helm/commit/1de1701c73b15a86e99ab1c5c53bd0e8659d8ede
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;;
;; Setup company-mode
;;
(global-company-mode)
(setq company-idle-delay 0.2)
(setq company-selection-wrap-around t)
(define-key company-active-map [tab] 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;;
;; Enable projectile mode
;;
(require 'projectile)
(add-hook 'python-mode-hook 'projectile-mode)

;;
;; Automatically auto-refresh a buffer if the file has changed on disk
;;
(global-auto-revert-mode t)

;;
;; Highlight lines that exceed 80 chars length
;;
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

;;
;; Setup bookmark
;;
(setq bookmark-save-flag 1) ;; every time bookmark is changed,
                            ;; automatically save it
;; Open bookmark list at startup
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;;
;; Elpy configuration
;;
(package-initialize)
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(elpy-use-ipython "ipython3")
;; Fix IPython5 s new prompt behavior (https://github.com/jorgenschaefer/elpy/issues/992)
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

;;
;; Configure py-autopep8
;;
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;
;; highlight-indentation-mode configuration
;;
(set-face-background 'highlight-indentation-face "gray18")
(set-face-background 'highlight-indentation-current-column-face "gray18")
