;;; init.el - An Emacs init file

;;
;; Copyright (C) 2016-2018 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/emacs-config
;; Version: 4.3.0
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
;; - all-the-icons                     [https://github.com/domtronn/all-the-icons.el]
;; - atom-one-dark-theme               [https://github.com/jonathanchu/atom-one-dark-theme]
;; - auto-package-update               [https://github.com/rranelli/auto-package-update.el]
;; - auto-sudoedit                     [https://github.com/ncaq/auto-sudoedit]
;; - cisco-router-mode                 [https://www.emacswiki.org/emacs/download/cisco-router-mode.el]
;; - command-log-mode                  [https://github.com/lewang/command-log-mode]
;; - company-mode                      [https://github.com/company-mode/company-mode]
;; - diff-hl                           [https://github.com/dgutov/diff-hl]
;; - dockerfile-mode                   [https://github.com/spotify/dockerfile-mode]
;; - elpy                              [https://elpy.readthedocs.io]
;; - esh-autosuggest                   [https://github.com/dieggsy/esh-autosuggest]
;; - exec-path-from-shell              [https://github.com/purcell/exec-path-from-shell]
;; - helm                              [https://github.com/emacs-helm/helm]
;; - helm-ag                           [https://github.com/syohex/emacs-helm-ag]
;; - helm-descbinds                    [https://github.com/emacs-helm/helm-descbinds]
;; - helm-projectile                   [https://github.com/bbatsov/helm-projectile]
;; - jinja2-mode                       [https://github.com/paradoxxxzero/jinja2-mode]
;; - magit                             [https://magit.vc]
;; - markdown-mode                     [http://jblevins.org/projects/markdown-mode]
;; - nlinum-hl                         [https://github.com/hlissner/emacs-nlinum-hl]
;; - ob-ipython                        [https://github.com/gregsexton/ob-ipython]
;; - org-beautify-theme                [https://github.com/jonnay/org-beautify-theme]
;; - org-bullets                       [https://github.com/sabof/org-bullets]
;; - org-download                      [https://github.com/abo-abo/org-download]
;; - org-plus-contrib                  [http://orgmode.org]
;; - projectile                        [https://github.com/bbatsov/projectile]
;; - psession                          [https://github.com/thierryvolpiatto/psession]
;; - py-autopep8                       [https://github.com/paetzke/py-autopep8.el]
;; - rainbow-delimiters                [https://www.emacswiki.org/emacs/RainbowDelimiters]
;; - shell-pop-el                      [https://github.com/kyagi/shell-pop-el
;; - smart-mode-line                   [https://github.com/Malabarba/smart-mode-line]
;; - transpose-frame                   [https://www.emacswiki.org/emacs/TransposeFrame]
;; - undo-tree                         [https://github.com/emacsmirror/undo-tree]
;; - use-package                       [https://github.com/jwiegley/use-package]
;; - volatile-highlights               [https://github.com/k-talo/volatile-highlights.el]
;; - wheatgrass-theme                  [https://github.com/jwiegley/emacs-release/blob/master/etc/themes/wheatgrass-theme.el]
;; - which-key                         [https://github.com/justbur/emacs-which-key]
;; - wttrin                            [https://github.com/bcbcarl/emacs-wttrin]
;; - yaml-mode                         [https://github.com/yoshiki/yaml-mode]
;; - yang-mode                         [https://github.com/mbj4668/yang-mode]
;; - yasnippet                         [https://github.com/joaotavora/yasnippet]
;;
;; Feel free to drop me an email in case of questions.


;;; General configuration section

;; Configure Emacs package manager
(package-initialize)
(require 'package)
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

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

;; Install `cl-generic' if required
;;
;; This is a forward compatibility package, which provides (a subset
;; of) the features of the cl-generic package introduced in Emacs-25,
;; for use on previous emacsen.
(if (version< emacs-version "25")
    (progn
      (unless (package-installed-p 'cl-generic)
        (package-refresh-contents)
        (package-install 'cl-generic))
      (require 'cl-generic)))

;; Print Emacs server utpime every half an hour
(run-at-time "00:00" 1800 '(lambda () (message
                                       (concat "[" (current-time-string) "]" " GNU Emacs server uptime: "
                                               (emacs-uptime)))))

;; Personal Information
(setq drestivo/personal-file (expand-file-name "personal.el"
                                               user-emacs-directory))
(when (file-exists-p drestivo/personal-file)
  (load drestivo/personal-file))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; Set LANG and LC_* variables
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;; macOS 'ls' command does not support the "--dired" option needed by Emacs
;; Alternatively, we use Emacs's own emulation of "ls"
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

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

;; Datetime format
(setq display-time-day-and-date t
      display-time-24hr-format t)

;; Set tab width to 4
(setq tab-width 4)

;; Blinking cursor
(blink-cursor-mode t)

;; Use visual bell instead of audio
(setq visible-bell 1)

;; By default, Emacs thinks a sentence is a full-stop followed by 2
;; spaces. Let’s make it full-stop and 1 space.
(setq sentence-end-double-space nil)

;; Emacs has the built-in DocView mode which lets you view PDFs. The
;; below setting allows continue scrolling
(setq doc-view-continuous t)

;; Set the initial major mode of newly created buffers to org-mode
(setq initial-major-mode (quote org-mode))

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

;; Change default directory to ~
(cd "~")

;; Tell Emacs where is your personal elisp lib directory
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Save custom variables to custom.el
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Set Emacs frame size and center it on the screen
(defvar drestivo/frame-height 60)
(defvar drestivo/frame-width 130)
(add-to-list 'default-frame-alist
             `(height . ,drestivo/frame-height))
(add-to-list 'default-frame-alist
             `(width . ,drestivo/frame-width))
(defvar drestivo/frame-pixel-height
  (* drestivo/frame-height (frame-char-height)))
(defvar drestivo/frame-pixel-width
  (* drestivo/frame-width (frame-char-width)))
(setq initial-frame-alist
;; Avoid the issue of having Emacs on the middle of two displays.
      `((left . ,(/ (-
                     (round (* (display-pixel-height) 1.777))
                       drestivo/frame-pixel-width) 2))
	(top .  ,(/ (-
                     ;; Remove 100px to take into account the MAC dock
                     (- (display-pixel-height) 100)  drestivo/frame-pixel-height) 2))))

;;-------------------------;;
;;  Backup files settings  ;;
;;-------------------------;;
;; Auto save very often
;; Save every 20 characters typed (this is the minimum)
(setq auto-save-interval 20)
;;
;; An example to understand Emacs backup retention logic. Let's assume
;; kept-new-versions and kept-old-versions are both set to 2. Here’s a
;; simulation in Emacs Lisp of 8 saves using lists, showing what
;; backups are available after each save when N is 2.
;;
;;  (1)
;;  (2 1)
;;  (3 2 1)
;;  (4 3 2 1)
;;  (5 4 2 1)
;;  (6 5 2 1)
;;  (7 6 2 1)
;;  (8 7 2 1)
;;
(setq vc-make-backup-files t) ;; Backup version controlled files
(setq backup-by-copying t)    ;; Backup the files by copying it
(setq delete-old-versions t   ;; Delete excess backup files silently
      kept-old-versions 2     ;; Oldest versions to keep when a new
			      ;; numbered backup is made (default: 2)
      kept-new-versions 10    ;; Newest versions to keep when a new
			      ;; numbered backup is made (default: 2)
      version-control t)      ;; Version numbers for backup files

;; By default, Emacs creates a backup only when you save the first
;; time ('per-session' backup). Here we describe how to make Emacs do a
;; backup on every save ('per-save' backup), not just the first.

(defvar drestivo/backup-location (expand-file-name "~/.saves")
  "Base directory for backup files.")

(defvar drestivo/backup-file-size-limit (* 10 1024 1024)
  "Maximum size of a file (in bytes) that should be copied at each save point.

If a file is greater than this size, don't make a backup of it.
Default is 10 MB")

(defvar drestivo/backup-trash-dir (expand-file-name "~/.saves/trash")
  "Directory for unwanted backups.")

(defvar drestivo/backup-exclude-regexp nil
  "Don't back up files matching this regexp.

Files whose full name matches this regexp are backed up to
`drestivo/backup-trash-dir'. Set to nil to disable this.")

;; Default and per-save backups go here:
;; N.B. backtick and comma allow evaluation of expression
;; when forming list
(setq backup-directory-alist
      `(("" . ,(expand-file-name "per-save" drestivo/backup-location))))

;; Add trash directories if needed
(if drestivo/backup-exclude-regexp
    (add-to-list 'backup-directory-alist `(,drestivo/backup-exclude-regexp . ,drestivo/backup-trash-dir)))

(defun drestivo/backup-every-save ()
  "Backup files every time they are saved.

Files are backed up to `drestivo/backup-location' in
sub-directories \"per-session\" once per Emacs session, and
\"per-save\" every time a file is saved.

Files whose names match the REGEXP in
`drestivo/backup-exclude-regexp' are copied to
`drestivo/backup-trash-dir' instead of the normal backup
directory.

Files larger than `drestivo/backup-file-size-limit' are not
backed up."

  ;; Make a special "per session" backup at the first save of each
  ;; Emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist
           `(("." . ,(expand-file-name "per-session" drestivo/backup-location))))
          (kept-new-versions 10))
      ;; Add trash directory if needed
      (if drestivo/backup-exclude-regexp
          (add-to-list
           'backup-directory-alist
           `(,drestivo/backup-exclude-regexp . ,drestivo/backup-trash-dir)))
      ;; Is the file too large?
      (if (<= (buffer-size) drestivo/backup-file-size-limit)
          (progn
            (message "Made per session backup of %s" (buffer-name))
            (backup-buffer))
        (message "WARNING: File %s too large to backup - increase value of drestivo/backup-file-size-limit" (buffer-name)))))
  ;; Make a per-save backup on each save. The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    ;;
    ;; Is the file too large?
    ;;
    (if (<= (buffer-size) drestivo/backup-file-size-limit)
        (progn
          (message "Made per save backup of %s" (buffer-name))
          (backup-buffer))
      (message "WARNING: File %s too large to backup - increase value of drestivo/backup-file-size-limit" (buffer-name)))))

;; Add to save hook
(add-hook 'before-save-hook 'drestivo/backup-every-save)

;;-----------------------------;;
;;  END Backup files settings  ;;
;;-----------------------------;;

;; Enable word wrap
(global-visual-line-mode t)
(diminish 'visual-line-mode)

;; Insert right brackets when left one is typed
(electric-pair-mode 1)

;; Keep a list of recently opened files
(setq-default recent-save-file "~/.emacs.d/recentf")
(recentf-mode 1)

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
            '(lambda ()
               (setq show-trailing-whitespace t))))

;; Spell checking configuration
(setq ispell-program-name "aspell")
;; Enable flyspell for text files and enable superword mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (diminish 'flyspell-mode)
                   ;; Enable superword mode, useful for “snake_case”.
                   (superword-mode 1)
                   (diminish 'superword-mode)
                   )))
;; Enable flyspell for code and enable superword mode
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                python-mode-hook
                js-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode)
               (diminish 'flyspell-mode)
               ;; Enable superword mode, useful for “snake_case”.
               (superword-mode 1)
               (diminish 'superword-mode)
               )))
;; Add some of the ispell shortcuts:
;; - press <f8> to check a word
;; - press M-<f8> to check the next one
(global-set-key (kbd "<f8>") 'ispell-word)
(defun drestivo/flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'drestivo/flyspell-check-next-highlighted-word)
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

;; Enable octave-mode for .m files
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Setup bookmark
(setq bookmark-save-flag 1) ;; every time bookmark is changed,
                            ;; automatically save it
;; Load bookmarks list at startup
(require 'bookmark)
(bookmark-bmenu-list)
;; Uncomment the next line if you want Emacs to switch to the
;; bookmarks buffer at startup
;; (switch-to-buffer "*Bookmark List*")

;; Automatically auto-refresh a buffer if the file has changed on disk
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)

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
(global-set-key (kbd "C-=") 'drestivo/copy-line)
;; No newline is added when pasting
(setq drestivo/copy-line-append-newline nil)

;; Make isearch treat space dash underscore newline as same
(setq search-whitespace-regexp "[-_ \n]")

;; Type y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ask for confirmation before exiting Emacs
(if (not (daemonp))
    (setq confirm-kill-emacs 'y-or-n-p))

;; Turn on highlighting current line
(global-hl-line-mode 1)

;; Make typing delete/overwrites selected text
(delete-selection-mode 1)

;; Customize Emacs calendar to start a week on Monday and to show the week number
(setq calendar-week-start-day 1)
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :foreground "light green")
(setq calendar-intermonth-text
      '(propertize
        (format "%2d" (car
                       (calendar-iso-from-absolute
                        (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))
(setq calendar-intermonth-header (propertize "Wk"))

;; Save the cursor position for every file you opened
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))


;;; Helper functions

;; Search for a keyword on the ORG directory using ag
;; Requires "The Silver Searcher" (ag) to be installed:
;; On macOS use: 'brew install the_silver_searcher'
;; On a Debian based GNU/Linux distro use: 'apt-get install silversearcher-ag'
(defun drestivo/org-directory-search-ag ()
  "Search for a keyword in the ORG folder using ag"
  (interactive)
  (if (not (eq org-directory nil))
      (helm-do-ag org-directory)
    (message "error: org-directory not set.")))

;; Reload Emacs init file
(defun drestivo/reload-dotemacs-file ()
  "Reload your init.el file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Redefine battery-pmset because of https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-09/msg00952.html
(defun drestivo/battery-pmset ()
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
(defun drestivo/new-empty-buffer ()
  "Create a new empty buffer. New buffer will be named “untitled” or
“untitled<2>”, “untitled<3>”, ..."
  (interactive)
  (let ((new-buf (generate-new-buffer "untitled")))
    (switch-to-buffer new-buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))
(global-set-key (kbd "<f7>") 'drestivo/new-empty-buffer)

;; copy-line - Source https://www.emacswiki.org/emacs/CopyingWholeLines
(defun drestivo/copy-line (arg)
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
    (if (eq last-command 'drestivo/copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (if drestivo/copy-line-append-newline
      (kill-append "\n" nil))
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun drestivo/helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun drestivo/org-download-method (link)
  "This is an helper function for org-download.

It creates an \"./image\" folder within the same directory of the ORG file.
Images are separated inside that image folder by additional folders one per
ORG file.

More info can be found here: https://github.com/abo-abo/org-download/issues/40.
See the commit message for an example:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir (concat
              (file-name-directory (buffer-file-name))
              (format "%s/%s/%s"
                      "images"
                      (file-name-base (buffer-file-name))
                      (org-download--dir-2)))))
    (progn
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Check if directory exists otherwise creates it
      (unless (file-exists-p dir)
        (make-directory dir t))
      (message (format "Image: %s saved!" (expand-file-name filename-with-timestamp dir)))
      (expand-file-name filename-with-timestamp dir))))

(defun drestivo/customize-wheatgrass-theme ()
  "This is an helper function for customizing the wheatgrass theme"
  (custom-set-faces
   ;; Selection highlight customization
   '(region ((t (:background "gray13"))))
   ;; company-mode customization
   '(company-preview ((t (:background "black"))))
   '(company-preview-common ((t (:background "black" :foreground "#E5C07B"))))
   '(company-preview-search ((t (:inherit company-preview :background "black"))))
   '(company-scrollbar-bg ((t (:background "black"))))
   '(company-tooltip ((t (:background "black"))))
   '(company-tooltip-annotation ((t (:background "black" :foreground "#828997"))))
   '(company-tooltip-common ((t (:background "black"))))
   ;; hl-line customization
   '(hl-line ((t (:background "gray13"))))
   ;; Linum customization
   '(linum ((t (:background "black"))))
   ;; Helm customization
   '(helm-candidate-number ((t (:background "color-233" :foreground "#98C379"))))
   '(helm-ff-directory ((t (:background "black" :foreground "#56B6C2" :weight bold))))
   '(helm-ff-executable ((t (:background "black" :foreground "#98C379" :weight normal))))
   '(helm-ff-file ((t (:background "black" :foreground "#ABB2BF" :weight normal))))
   '(helm-ff-invalid-symlink ((t (:background "black" :foreground "#E06C75" :weight bold))))
   '(helm-ff-symlink ((t (:background "black" :foreground "#E5C07B" :weight bold))))
   '(helm-header ((t (:background "color-234" :foreground "#828997" :box (:line-width 6 :color "#282C34") :underline nil))))
   '(helm-separator ((t (:background "black" :foreground "#E06C75"))))
   '(helm-source-header ((t (:background "black" :foreground "#E5C07B" :box (:line-width 6 :color "brightblack") :underline nil :weight bold))))
   ;; Magit customization
   '(magit-blame-date ((t (:background "black" :foreground "#5C6370"))))
   '(magit-blame-hash ((t (:background "black" :foreground "#C678DD"))))
   '(magit-blame-heading ((t (:background "black" :foreground "#828997"))))
   '(magit-blame-name ((t (:background "black" :foreground "#E5C07B"))))
   '(magit-blame-summary ((t (:background "black" :foreground "#828997"))))
   '(magit-diff-context-highlight ((t (:background "black" :foreground "#ABB2BF"))))
   '(magit-refname ((t (:background "black" :foreground "#ABB2BF" :weight bold))))
   '(magit-refname-stash ((t (:foreground "#ABB2BF" :weight bold))))))

(defun drestivo/insert-date ()
  (interactive)
  "Insert current datetime into buffer without a newline."
  (insert (concat "Date: " (shell-command-to-string "printf %s \"$(date)\""))))
(global-set-key (kbd "C-+") 'drestivo/insert-date)

(defmacro drestivo/with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun drestivo/eshell-prompt ()
  "Customize eshell prompt.

This function requires `all-the-icons' package to be installed
(https://github.com/domtronn/all-the-icons.el)."
  (if (window-system)
      (setq drestivo/header-bg "#282C34")
    ;; The background used when Emacs runs in a terminal
    (setq drestivo/header-bg "black"))
  ;; In order to set the eshell prompt correctly we need to
  ;; distinguish between the case where we are in a local folder or
  ;; the case where we are connected to a remote server via TRAMP
  ;; (i.e.). The shell need to be temporary restored to the
  ;; default one.
  (let ((shell-file-name "/bin/sh"))
    (progn
      (if (file-remote-p default-directory)
          (progn
            (setq drestivo/user-login-name (replace-regexp-in-string "\n$" ""
                                                                     (shell-command-to-string "whoami"))
                  drestivo/system-name (replace-regexp-in-string "\n$" ""
                                                                 (shell-command-to-string "hostname"))
                  drestivo/user-uid (string-to-number (replace-regexp-in-string "\n$" ""
                                                                                (shell-command-to-string "id -u")))))
        (progn
          (setq drestivo/user-login-name (user-login-name)
                ;; Remove the domain name from the local eshell prompt
                drestivo/system-name (car (split-string (system-name) "\\."))
                drestivo/user-uid (user-uid))))
      (concat
       "┌─ "
       (if (window-system)
           (all-the-icons-faicon "folder-open-o")
         "")
       " "
       (drestivo/with-face (concat (eshell/pwd) " ") :background drestivo/header-bg)
       (if (string= (ignore-errors (vc-responsible-backend default-directory)) "Git")
           (progn
             (setq git-status (split-string (vc-git--run-command-string default-directory "status" "-s")))
             (drestivo/with-face
              (format "[%s %s %s] "
                      (if (window-system)
                          (all-the-icons-faicon "git-square")
                        "Git")
                      (if (window-system)
                          (concat (all-the-icons-octicon  "git-branch") ":" (car (vc-git-branches)))
                        (concat "branch" ":" (car (vc-git-branches))))
                      (concat
                       "status:"
                       (if (member "A" git-status)  "A" "-")   ;; Added files (not committed)
                       (if (member "M" git-status)  "M" "-")   ;; Modified files
                       (if (member "D" git-status)  "D" "-")   ;; Deleted files
                       (if (member "??" git-status) "U" "-"))) ;; Untracked files
              :background drestivo/header-bg :foreground "LightGreen")))
       (drestivo/with-face
        (concat
         "["
         (if (window-system)
             (concat (all-the-icons-material "schedule") " "))
         (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
         "]") :background drestivo/header-bg :foreground "gainsboro")
       (drestivo/with-face "\n└─> " :background drestivo/header-bg)
       (drestivo/with-face drestivo/user-login-name :foreground "LightBlue")
       "@"
       (drestivo/with-face drestivo/system-name :foreground "LightGreen")
       (if (= drestivo/user-uid 0)
           (drestivo/with-face " #" :foreground "LightRed")
         " $")
       " "))))


;;
;; ORG utility functions
;; Link: https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
;;
(defun drestivo/org-show-current-heading-tidily ()
  (interactive)
  "In an org file shows current entry, keeping other entries collapsed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))


;;; Packages configuration section

;; cisco-router-mode
;; https://github.com/emacsmirror/cisco-router-mode/blob/master/cisco-router-mode.el
(load-library "cisco-router-mode")

;; transpose-frame
;; https://www.emacswiki.org/emacs/TransposeFrame
(load-library "transpose-frame")

;; atom-one-dark-theme (GUI mode) and wheatgrass-theme (Terminal mode)
(use-package atom-one-dark-theme
  :ensure t
  :config
  ;; The below hook is needed when Emacs is started in daemon mode.
  ;; In this case it is required to add a hook called during the
  ;; creation of every new frame in order to load the configured
  ;; options like i.e. the theme.
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (window-system)
                      (progn
                        (display-graphic-p frame)
                        (load-theme 'atom-one-dark t)
                        ;; Transparent window in Emacs on macOS
                        (set-frame-parameter (selected-frame) 'alpha '(98 98))
                        ;; Always bring a newly created frame on top
                        (select-frame-set-input-focus frame)
                        (toggle-scroll-bar -1))
                    (progn
                      (load-theme 'wheatgrass t)
                      (drestivo/customize-wheatgrass-theme)))))
    ;; Emacs not running in daemon mode
    (if (window-system)
        (progn
         (load-theme 'atom-one-dark t)
         ;; Transparent window in Emacs on macOS
         (set-frame-parameter (selected-frame) 'alpha '(98 98)))
    (progn
      (load-theme 'wheatgrass t)
      (drestivo/customize-wheatgrass-theme)))))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  ;; http://stackoverflow.com/questions/35286203/exec-path-from-shell-message-when-starting-emacs
  (when (string= system-type "darwin")
    (exec-path-from-shell-initialize)))

;; magit
(use-package magit
  :defer t
  :ensure t
  :init
  (setq magit-repository-directories
          (list '("~/.emacs.d" . 1 )
                '("~/.dotfiles" . 1 )
                '("~/org" . 1 )))
  :bind
  ("<f2>" . magit-status)
  ("<f5>" . magit-list-repositories))

;; ORG
(use-package org
  :defer t
  :ensure org-plus-contrib
  :config
  (load-library "find-lisp")
  ;; ORG directories and files
  (setq org-directory "~/org/")
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq org-agenda-files
                    (append
                     (find-lisp-find-files (concat org-directory "agenda") "\.org$")
                     (find-lisp-find-files (concat org-directory "notebooks") "\.org$")
                     (find-lisp-find-files (concat org-directory "work-projects") "\.org$")
                     (list (concat org-directory "refile-beorg.org"))
                     (list (concat org-directory "refile.org"))))))
  (setq org-default-notes-file (concat org-directory "refile.org"))
  ;; Additional files to be searched in addition to the default ones
  ;; contained in the agenda folder
  (setq org-agenda-text-search-extra-files
        (append
         (find-lisp-find-files (concat org-directory "home-projects") "\.org$")
         (find-lisp-find-files (concat org-directory "work-projects") "\.org$")
         (find-lisp-find-files (concat org-directory "notebooks") "\.org$")
         (list (concat org-directory "refile-beorg.org"))
         (list (concat org-directory "refile.org"))))
  ;; Configure refiling
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-refile-allow-creating-parent-nodes t)
  ;; A timestamp or a note will be recorded when an entry has been refiled
  (setq  org-log-refile t)
  ;; Set ORG ellipsis style to a downward arrow "⤵" instead of "..."
  (setq org-ellipsis "⤵")
  ;; Save all ORG buffers every hour
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)
  ;; Add "CLOSED: [timestamp]" when a task is marked as DONE
  (setq org-log-done t)
  ;; Equivalent of "#+STARTUP: showeverything " on all ORG files
  (setq org-startup-folded nil)
  ;; Images inlined on opening an org buffer
  (setq org-startup-with-inline-images t)
  ;; Set images default width to 320. Emacs requires ImageMagick support "--with-imagemagick@6"
  (setq org-image-actual-width '(320))
  ;; Default file applications on a macOS system
  (when (string= system-type "darwin")
    (setq org-file-apps org-file-apps-defaults-macosx))
  ;; ORG default TODO keywords
  ;; The below can be customized per file using:
  ;;
  ;; #+TODO: "TODO(t)" "DOING(d)" "WAIT OTHERS(w)" "DELEGATED(g)" "REVIEW(r)" "|" "DONE(D)" "CANCELED(C)" " REVIEWED(R)")
  ;;
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(d)" "WAIT OTHERS(w)" "DELEGATED(g)" "REVIEW(r)" "|" "DONE(D)" "CANCELED(C)" " REVIEWED(R)")))
  ;; ORG mode has its own markup syntax but seeing the emphasis
  ;; markers is distracting. The below setting hides it.
  (setq org-hide-emphasis-markers t)
  ;; Wrap long lines. Don't let it disappear to the right
  (setq org-startup-truncated nil)
  ;; When in a URL pressing enter key opens it
  (setq org-return-follows-link t)
  ;; Capture templates for: TODO tasks and notes
  (setq org-capture-templates
        (quote (("t" "Todo" entry (file (lambda () (concat org-directory "refile.org")))
                 "* TODO %?\n%U\n%a\n")
                ("n" "Note" entry (file (lambda () (concat org-directory "refile.org")))
                 "* %? :NOTE:\n%U\n%a\n"))))
  ;; ORG tags shortcuts
  (setq org-tag-alist '(("HIGH" . ?h)
                        ("MEDIUM" . ?m)
                        ("LOW" . ?l)
                        ("NOTE" . ?n)
                        ("REVIEW" . ?r)))
  ;; Add some useful hooks to org-mode
  (add-hook 'org-mode-hook
            '(lambda ()
                (setq show-trailing-whitespace t)
                (flyspell-prog-mode)
                (org-indent-mode)
                (superword-mode 1)
                (if (window-system)
                    (load-theme 'org-beautify t))))

  :bind
  ("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda)
  ("\C-cc" . org-capture)
  ("\C-cb" . org-iswitchb)
  ("\C-cj" . drestivo/org-show-current-heading-tidily)
  ("<f6>"  . drestivo/org-directory-search-ag))

;; ORG Babel: Main section
(use-package ob
  :defer t
  :config
  ;; Make ORG mode allow eval elisp, python and ruby
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
  :defer t
  :ensure t
  :config
  (setq ob-ipython-command "ipython3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)))
  ;; Display images inline in the same buffer
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

;; org-download
(use-package org-download
  :ensure t
  :config
  ;; Change screen capture command only for macOS
  (when (string= system-type "darwin")
    (setq org-download-screenshot-method "screencapture -s -x %s"))
  (setq org-download-method  'drestivo/org-download-method)
  (setq org-download-heading-lvl 0)
  ;; org-download default directory
  ;; (setq-default org-download-image-dir "./images")
  (setq org-download-image-html-width '320))

;; org-bullets
(use-package org-bullets
  :ensure t
  :config
 (add-hook 'org-mode-hook
            '(lambda ()
                (org-bullets-mode 1))))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; auto-package-update
(use-package auto-package-update
  :defer t
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
  (when (string= system-type "darwin")
    (setq battery-status-function 'drestivo/battery-pmset
          battery-echo-area-format "Power %L, battery %B (%p%% charged, remaining time %t)"
          battery-mode-line-format " [ %b%p%% ] ")
    (display-battery-mode)))

;; yaml-mode
(use-package yaml-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)
               (setq show-trailing-whitespace t)
               (flyspell-prog-mode)
               (superword-mode 1))))

;; jinja2-mode
(use-package jinja2-mode
  :defer t
  :ensure t
  :config
   (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))
   ;; "C-c c" jinja2-close-tag
   ;; "C-c t" jinja2-insert-tag
   ;; "C-c v" jinja2-insert-var
   ;; "C-c #" jinja2-insert-comment
   (add-hook 'jinja2-mode-hook
             '(lambda ()
                 (setq show-trailing-whitespace t)
                 (flyspell-prog-mode)
                 (superword-mode 1))))

;; helm-config
(use-package helm-config
  :config
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c")))

;; helm - Helm is an incremental completion and selection narrowing
;; framework for Emacs.
(use-package helm
  :defer t
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  ;; Enable fuzzy matching
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  ;;--------------------------------------------------------------------------;;
  ;;       Work with Spotlight on macOS instead of the regular locate         ;;
  ;;--------------------------------------------------------------------------;;
  (if (string= system-type "darwin")
      (progn
        (setq drestivo/helm-locate-spotlight-command "mdfind -name -onlyin ~ %s %s")
        (setq drestivo/helm-locate-exclude-dirs "~/Library")
        (setq drestivo/helm-locate-exclude-command " | egrep -v ")
        (setq helm-locate-command
              (concat drestivo/helm-locate-spotlight-command
                      drestivo/helm-locate-exclude-command
                      drestivo/helm-locate-exclude-dirs))
        (setq helm-locate-fuzzy-match nil))
    (setq helm-locate-fuzzy-match t))
  ;;--------------------------------------------------------------------------;;
  ;;     END Work with Spotlight on macOS instead of the regular locate       ;;
  ;;--------------------------------------------------------------------------;;
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-autoresize-mode t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-follow-mode-persistent t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)
  (add-hook 'helm-minibuffer-set-up-hook
            'helm-hide-minibuffer-maybe)
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  ;; Replace the default helm grep command with ag.
  ;; Requires "The Silver Searcher" (ag) to be installed.
  ;; On macOS use: 'brew install the_silver_searcher'
  (when (executable-find "ag")
    ;; For helm to recognize correctly the matches we need to enable
    ;; line numbers and columns in its output, something the
    ;; --vimgrep option does.
   (setq helm-grep-default-command         "ag --vimgrep --nogroup --nocolor -z %p %f"
         helm-grep-default-recurse-command "ag --vimgrep --nogroup --nocolor -z %p %f"))
  :bind
  ;; bind keys because of this commit:
  ;; https://github.com/emacs-helm/helm/commit/1de1701c73b15a86e99ab1c5c53bd0e8659d8ede
  ("M-x"     . helm-M-x)
  ("M-y"     . helm-show-kill-ring)
  ("C-x b"   . helm-mini)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files)
  ("C-x C-r" . helm-recentf)
  ("C-c h x" . helm-register)
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
                                   (helm-do-ag-buffers)))))

;; helm-descbinds
(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

;; helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; company
(use-package company
  :diminish company-mode
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
  :diminish projectile-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'projectile-mode)
  (setq projectile-completion-system 'helm))

;; py-autopep8
(use-package py-autopep8
  :ensure t
  :config
  ;; Configure elpy autopep8 support
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; elpy
(use-package elpy
  :diminish elpy-mode
  :defer t
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (elpy-use-ipython "ipython3")
  ;; Fix IPython5 new prompt behavior (https://github.com/jorgenschaefer/elpy/issues/992)
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (add-hook 'python-mode-hook 'elpy-mode))

;; highlight-indentation-mode
(use-package highlight-indentation
  :diminish highlight-indentation-mode
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "gray18")
  (set-face-background 'highlight-indentation-current-column-face "gray18"))

;; markdown-mode
(use-package markdown-mode
  :defer t
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
            '(lambda ()
                (setq show-trailing-whitespace t)
                (flyspell-prog-mode)
                (superword-mode 1)))
  (set-face-background 'markdown-code-face "#282C34")
  (set-face-foreground 'markdown-code-face "#ABB2BF"))

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"           ;; AndreaCrotti/yasnippet-snippets
          "~/.emacs.d/snippets-addons"    ;; Personal snippets
          ))
  (yas-reload-all))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-diff 1)
  (setq undo-tree-visualizer-timestamps 1))

;; psession
(use-package psession
  :ensure t
  :config
  (setq psession-object-to-save-alist
        '(
          ;;(ioccur-history . "ioccur-history.el")
          (extended-command-history . "extended-command-history.el")
          (helm-external-command-history . "helm-external-command-history.el")
          (helm-surfraw-engines-history . "helm-(setq )urfraw-engines-history.el")
          (psession--save-buffers-alist . "psession-save-buffers-alist.el")
          (helm-ff-history . "helm-ff-history.el")
          (helm-grep-history . "helm-grep-history.el")
          (kill-ring . "kill-ring.el")
          (kill-ring-yank-pointer . "kill-ring-yank-pointer.el")
          (register-alist . "register-alist.el")
          (psession--winconf-alist . "psession-winconf-alist.el")
          ))
  (psession-mode 1))

;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; wttrin - Weather application
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
  (setq wttrin-default-cities '("Aarau"
                                "Bern"
                                "Zurich")))

;; which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

;; whitespace - Highlight lines that exceed 80 chars length
(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-global-modes '(not org-mode lisp-interaction-mode))
  (global-whitespace-mode t))

;; gnutls customization
;;
;; Please look at: https://blogs.fsfe.org/jens.lechtenboerger/2014/03/23/certificate-pinning-for-gnu-emacs/
(use-package gnutls
  :ensure t
  :ensure-system-package (gnutls-cli . "brew install gnutls || sudo apt-get install gnutls-bin")
  :config
  (setq tls-program '("gnutls-cli -p %p %h")
        imap-ssl-program '("gnutls-cli -p %p %s")
        smtpmail-stream-type 'starttls))

;; dired customization
(use-package dired
  :defer t
  :config
  (setq dired-dwim-target nil)
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode 1))))

;; ElDoc
(use-package eldoc
  :diminish eldoc-mode)

;; nlinum-hl - Line numbering mode
(use-package nlinum-hl
  :ensure t
  :config
  (global-nlinum-mode)
  (setq nlinum-format "%d ")
  (column-number-mode 1))

;; diff-hl
(use-package diff-hl
  :ensure t
  :diminish diff-hl-mode
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (window-system)
                      (global-diff-hl-mode)
                    (progn
                      (setq diff-hl-side 'right)
                      (global-diff-hl-mode)
                      (diff-hl-margin-mode)))))
    ;; Emacs not running in daemon mode
    (if (window-system)
        (global-diff-hl-mode)
      (progn
        (setq diff-hl-side 'right)
        (global-diff-hl-mode)
        (diff-hl-margin-mode)))
  ;; Highlight changed files in the fringe of dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; Eshell
(use-package eshell
  :ensure t
  :config
  ;; Eshell prompt customization
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'drestivo/eshell-prompt)
  (add-hook 'eshell-exit-hook
            (lambda ()
              (delete-window)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; (setq eshell-destroy-buffer-when-process-dies t)
              ;; Programs that need special displays
              (add-to-list 'eshell-visual-subcommands '("git" "diff" "help" "log" "show"))
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)
              (define-key eshell-mode-map (kbd "C-c C-;")  'helm-eshell-prompts))))

;; All the icons
(use-package all-the-icons
  :ensure t
  :config
  ;; The below command needs to be run only once manually to install the needed fonts
  ;; (all-the-icons-install-fonts)
  )

;; Beautify org buffers
(use-package org-beautify-theme
  :ensure t
  :defer t
  ;; This theme is loaded when entering ORG mode. Please see the above ORG section
  )

;; YANG mode
(use-package yang-mode
  :ensure t)

;; Display the keys you typed in a special buffer: *command-log*
(use-package command-log-mode
  :ensure t
  :defer t)

;; A Dockerfile mode for Emacs
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Pop-up a shell
(use-package shell-pop
  :ensure t
  :config
  (custom-set-variables
   '(shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
   '(shell-pop-term-shell "eshell")
   '(shell-pop-universal-key "C-x t")
   '(shell-pop-window-size 50)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))

;; Fish-like autosuggestions in eshell
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)

;; When find-file and dired-mode try to access a non writable file
;; auto-sudoedit re-opens the file automatically using sudo in TRAMP
(use-package auto-sudoedit
  :ensure t
  :diminish auto-sudoedit-mode
  :config
  (auto-sudoedit-mode 1))


;;; init.el ends here
