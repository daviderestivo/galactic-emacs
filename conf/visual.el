;;; visual.el --- Visual settings

;;
;; Copyright (C) 2016-2018 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/emacs-config/blob/master/conf/visual.el
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

;; Disable the toolbar and the scroll-bar. Press F9 to enable the scroll-bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key (kbd "<f9>") 'toggle-scroll-bar)

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

;; Customize ediff background colors
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-background
             ediff-even-diff-face-A "grey20")
            (set-face-background
             ediff-even-diff-face-B "grey20")
            (set-face-background
             ediff-even-diff-face-C "grey20")
            (set-face-background
             ediff-odd-diff-face-A  "grey20")
            (set-face-background
             ediff-odd-diff-face-B  "grey20")
            (set-face-background
             ediff-odd-diff-face-C  "grey20")))

;; Unset the frame title and remove the icon
(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)

;;; visual.el ends here
