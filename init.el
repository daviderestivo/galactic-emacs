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
;; Set default font
;;
(set-default-font "Menlo 14")

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
      `((left . ,(/ (- (round (* (display-pixel-height) 1.777))
                       frame-pixel-width) 2))
	(top .  ,(/ (- (display-pixel-height)  frame-pixel-height)
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
;; Enable flyspell for text files 
;;
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
;;
;; Enable flyspell for code 
;;
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook
                R-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Jedi - Python auto-completion package for Emacs - ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sun Apr  2 00:56:58 CEST 2017 - Disabled below section because jedi
;; has been replaced with company-jedi.
;;
;; (require 'jedi)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;;
;; Set Jedi to use python3 instead of the default python2
;;
(setq jedi:environment-root "python3")
(setq jedi:server-command
      '("~/.emacs.d/.python-environments/python3/bin/jediepcserver"))
;;
;; In order to install a python2 venv for Jedi do the following:
;;
;; 1. Create a new buffer and enter python-mode
;;
;; 2. Evaluate the following: 
;;
;;    (setq jedi:environment-root "python2")
;;    (setq jedi:environment-virtualenv
;;          (append python-environment-virtualenv
;;                  '("--python" "/usr/local/bin/python")))
;;
;; 3. Install the Jedi server using:
;;
;;    M-x jedi:install-server
;; 
;; IMPORTANT: running jedi:install-server on an already installed
;; venv will update all of the additional packages
;; (pip install --upgrade).
;;

;;
;; The default Jedy python venv is python3 based. See above.
;; The below two functions permit to switch a buffer between
;; python2 and python3 if needed.
;;
(defun jedi:use-python2 ()
  (interactive)
  ;; Switch a buffer to python2
  (jedi:stop-server)
  (make-local-variable 'jedi:server-command)
  (setq jedi:server-command
	'("~/.emacs.d/.python-environments/python2/bin/jediepcserver"))
  (make-local-variable 'jedi:environment-root)
  (setq  jedi:environment-root "python2")
  )

(defun jedi:use-python3 ()
  (interactive)
  (jedi:stop-server)
  ;; Switch a buffer to python3
  (make-local-variable 'jedi:server-command)
  (setq jedi:server-command
	'("~/.emacs.d/.python-environments/python3/bin/jediepcserver"))
  (make-local-variable 'jedi:environment-root)
  (setq  jedi:environment-root "python3")  
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END Jedi - Python auto-completion package for Emacs - ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;
;; company-jedi setup
;;
(defun company-jedi:python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'company-jedi:python-mode-hook)

;;
;; Emacs IPython Notebook config section
;; 
(require 'ein)
;; Enable autocomplete
(setq ein:use-auto-complete t)

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
;; bind keys because of this commit: https://github.com/emacs-helm/helm/commit/1de1701c73b15a86e99ab1c5c53bd0e8659d8ede
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
;; Setup fill-column-indicator
;;
(setq fci-rule-width 1)
(setq fci-rule-color "#3E4451")
;; enable fci-mode as a global minor mode
(define-globalized-minor-mode global-fci-mode
  fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
