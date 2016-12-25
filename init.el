;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;
;; Add melpa, melpa-stable and marmalade package archives
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;
;; Sort apropos results by relevancy
;;
(setq apropos-sort-by-scores t)

;;
;; exec-path-from-shell
;;
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;
;; Inhibit welcome buffer
;;
(setq inhibit-startup-screen t)

;;
;; Change default dir to ~
;;
(cd "~")

;;
;; Tell emacs where is your personal elisp lib dir
;;
(add-to-list 'load-path "~/.emacs.d/lisp/")

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
 '(custom-enabled-themes (quote (tango-dark)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(package-selected-packages (quote (magit ## exec-path-from-shell)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Set Emacs window frame size
;;
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 120))

;;
;; Set Emacs startup position
;;
;; TODO: to be improved centering it on the screen
(setq initial-frame-alist '((left . 400) (top . 200)))

;;
;; Backup files settings
;;
;; Backup directory
(setq backup-directory-alist `(("." . "~/.saves")))
;; Backup the file by copying it
(setq backup-by-copying t)
(setq delete-old-versions t ; delete excess backup files silently
  kept-new-versions 6       ; oldest versions to keep when a new numbered backup is made (default: 2)
  kept-old-versions 2       ; newest versions to keep when a new numbered backup is made (default: 2)
  version-control t         ; version numbers for backup files
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
(setq ispell-program-name "/usr/local/bin/aspell")
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


;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-term settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This tells term (which is used by ansi-term) to kill the buffer after the terminal is exited. 
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
;;  URLs that show up in my terminal (via man pages, help, info, errors, etc) to be clickable.
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
