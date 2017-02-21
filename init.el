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
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

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
;; Inhibit welcome buffer
;;
(setq inhibit-startup-screen t)

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
;; Add Major modes
;; 
(load-library "cisco-router-mode")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "3d47d88c86c30150c9a993cc14c808c769dad2d4e9d0388a24fee1fbf61f0971" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(package-selected-packages
   (quote
    (smart-mode-line-powerline-theme smart-mode-line zenburn-theme markdown-preview-mode ein jedi auto-package-update rainbow-delimiters org magit ## exec-path-from-shell)))
 '(show-paren-mode t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "white"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan"))))
 '(sml/col-number ((t (:inherit sml/global :background "black" :foreground "gray70" :weight bold))))
 '(sml/filename ((t (:inherit sml/global :background "#383838" :foreground "dark gray" :weight light))))
 '(sml/line-number ((t (:inherit sml/global :background "black" :foreground "gray70" :weight bold))))
 '(sml/modes ((t (:inherit sml/global :background "#5F5F5F" :foreground "Black" :weight ultra-light))))
 '(sml/modified ((t (:foreground "dark gray"))))
 '(sml/position-percentage ((t (:inherit sml/prefix :background "#5F5F5F" :foreground "gray70" :weight normal))))
 '(sml/prefix ((t (:inherit sml/global :background "#383838" :foreground "dark gray"))))
 '(sml/read-only ((t (:inherit sml/not-modified :foreground "dark gray"))))
 '(sml/vc ((t (:inherit sml/git :background "#5F5F5F" :foreground "dark gray"))))
 '(sml/vc-edited ((t (:inherit sml/prefix :background "#5F5F5F" :foreground "burlywood1")))))

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
      `((left . ,(/ (- (round (* (display-pixel-height) 1.777))  frame-pixel-width) 2))
	(top .  ,(/ (- (display-pixel-height)  frame-pixel-height) 2))))

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
      kept-new-versions 6   ; oldest versions to keep when a new numbered backup
			    ; is made (default: 2)
      kept-old-versions 2   ; newest versions to keep when a new numbered backup
			    ; is made (default: 2)
      version-control t     ; version numbers for backup files
      )

;;
;; Enable line and column numbering
;;
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq linum-format "%d ")
(column-number-mode 1)

;;
;; whitespace-mode
;;
;; Highlight lines that exceed a certain length (80 columns)
;;
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length to 80 columns
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode 1)

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
;; In Mac OS X the right mouse button does not seem to trigger [mouse-2], so you
;; cannot right click a word to get a suggestion. This can be fixed with
;; the below:
;;
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;
;; Ediff settings: split horizontally and avoid floating ediff window
;;
(setq ediff-split-window-function 'split-window-horizontally)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ansi-term settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This tells term (which is used by ansi-term) to kill the buffer after the
;; terminal is exited. 
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
(defvar my-term-shell "/bin/bash")
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
;; URLs that show up in my terminal (via man pages, help, info, errors, etc)
;; to be clickable.
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
  (ansi-term "/bin/bash"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END ansi-term settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Code completion key bindings in Emacs
;;
(global-set-key (kbd "C-<tab>") 'hippie-expand)
;;
;; Settings for hippie-expand
;;
(setq hippie-expand-try-functions-list
       '(try-complete-lisp-symbol
         try-complete-lisp-symbol-partially
         try-expand-dabbrev
         try-expand-dabbrev-from-kill
         try-expand-dabbrev-all-buffers
         try-expand-line
         try-complete-file-name-partially
         try-complete-file-name))

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
;; rainbow-delimiters-mode setup, with decreasing bracket size
;;
;;(custom-set-faces
;;
;; '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 2.0))))
;; '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.8))))
;; '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.6))))
;; '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.4))))
;; '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.2))))
;; '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.1))))
;; '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
;; '(rainbow-delimiters-depth-8-face ((t (:foreground "black" :height 0.9))))
;; '(rainbow-delimiters-depth-8-face ((t (:foreground "white" :height 0.8))))
;; '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 0.7))))
;; )
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Jedi - Python auto-completion package for Emacs - ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
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
;;    (setq jedi:environment-root "default")
;;    (setq jedi:environment-virtualenv
;;          (append python-environment-virtualenv
;;                  '("--python" "/usr/local/bin/python")))
;;
;; 3. Install the Jedi server using:
;;
;;    M-x jedi:install-server
;; 
;; IMPORTANT: running jedi:install-server on an already installed venv will
;; update all of the additional packages (pip install --upgrade).
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
	'("~/.emacs.d/.python-environments/default/bin/jediepcserver"))
  (make-local-variable 'jedi:environment-root)
  (setq  jedi:environment-root "default")
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
(setq sml/theme 'powerline)
(sml/setup)

;;
;; Setup transpose-frame
;;
(require 'transpose-frame)
