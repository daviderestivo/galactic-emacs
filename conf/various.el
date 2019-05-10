;;; various.el --- Various settings

;;
;; Copyright (C) 2016-2019 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/emacs-config/blob/master/conf/various.el
;; Version: 0.1
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

;; If Emacs is running in daemon mode, print Emacs server utpime every
;; half an hour
(if (daemonp)
    (run-at-time "00:00" 1800 (lambda () (message
                                     (concat "[" (current-time-string) "]" " GNU Emacs server uptime: "
                                             (emacs-uptime))))))

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

;; Enable winner mode
;; Winner Mode is a global minor mode. When activated, it allows
;; you to “undo” (and “redo”) changes in the window configuration
;; with the key commands ‘C-c left’ and ‘C-c right’.
(winner-mode t)

;; Change default directory to ~
(cd "~")

;; Insert right brackets when left one is typed
(electric-pair-mode 1)

;; Keep a list of recently opened files
(setq-default recent-save-file "~/.emacs.d/recentf")
(recentf-mode 1)

;; Ediff settings
;; Split horizontally and avoid floating ediff window
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function (quote ediff-setup-windows-plain))

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
(global-set-key (kbd "C-=") 'drestivo-copy-line)
;; No newline is added when pasting
(setq drestivo-copy-line-append-newline nil)

;; Make isearch treat space dash underscore newline as same
(setq search-whitespace-regexp "[-_ \n]")

;; Type y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ask for confirmation before exiting Emacs
(if (not (daemonp))
    (setq confirm-kill-emacs 'y-or-n-p))

;; Make typing delete/overwrites selected text
(delete-selection-mode 1)

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

;; Save the cursor position for every file you opened
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Rebind `other-window' to "M-o"
(global-set-key (kbd "M-o") 'other-window)

;; Automatically follow symlinks but displays a warning
(setq vc-follow-symlinks nil)

;; M-SPACE binds to `just-one-space' that delete all but one white
;; space at a point. The `cycle-spacing' when called multiple times,
;; cycles through:
;; - replacing all spaces with a single space
;; - removing all spaces
;; - restoring the original spacing
(global-set-key [remap just-one-space] 'cycle-spacing)


;;; various.el ends here
