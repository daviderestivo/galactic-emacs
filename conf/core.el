;;; core.el --- Galactic Emacs core configuration -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2021 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/core.el
;; Version: 12.0.0
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

;; Galactic Emacs core configuration.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


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

;; List-manipulation utilities
(use-package dash
  :ensure t)

;; Log Emacs startup time in *Messages*
(add-hook 'emacs-startup-hook
          (lambda ()
            (message (format "Emacs startup time: %s" (emacs-init-time)))))

;; Start Emacs server in background when Emacs is idle for more than
;; 15 seconds
(require 'server)
(run-with-idle-timer 15 nil
                     (lambda ()
                       (message "Start GNU Emacs server...")
                       (server-start)
                       (message "Start GNU Emacs server...done")))

;; If Emacs server is running print server utpime every half an hour
(run-at-time "00:00" 1800
             (lambda ()
               (if (server-running-p)
                   (message (concat "[" (current-time-string) "]"
                                    " GNU Emacs server uptime: "
                                    (emacs-uptime))))))

;; Enable garbage collect messages
(setq garbage-collection-messages t)
;; Run garbage collection only when Emacs is idle for more than 60
;; seconds
(run-with-idle-timer 60 t (lambda () (galactic-emacs-garbage-collect)))

;; Resolve the conflict where a new command wants to direct its output
;; to the buffer ‘*Async Shell Command*’ creating a new buffer without
;; prompting for a confirmation
(setq async-shell-command-buffer 'new-buffer)

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

;; Change default directory to ~
(cd "~")

;; Keep a list of recently opened files
(setq-default recent-save-file "~/.emacs.d/recentf")
(recentf-mode 1)

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
(global-set-key (kbd "C-=") 'galactic-emacs-copy-line)
;; No newline is added when pasting
(setq galactic-emacs-copy-line-append-newline nil)

;; Make isearch treat space dash underscore newline as same
(setq search-whitespace-regexp "[-_ \n]")

;; Type y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ask for confirmation before exiting Emacs
(if (not (daemonp))
    (setq confirm-kill-emacs 'y-or-n-p))

;; Kill process buffer without confirmation
(setq kill-buffer-query-functions nil)

;; Make typing delete/overwrites selected text
(delete-selection-mode 1)

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

;; Avoid performance issues in files with very long lines.
(unless (version<= emacs-version "27")
  (global-so-long-mode 1))

;; Disable native-comp warnings on Em
(unless (version< emacs-version "28")
  (if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
      (add-to-list 'warning-suppress-types '(comp))))


;;; core.el ends here
