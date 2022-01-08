;;; spell-checking.el --- Galactic Emacs spell checking configuration -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2022 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/spell-checking.el
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

;; Galactic Emacs spell checking configuration based on Flyspell.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


(use-package flyspell
  :ensure-system-package (aspell . "brew install aspell || sudo apt-get install aspell")
  :config
  ;; Spell checking configuration
  (setq ispell-program-name "aspell")
  ;; Enable flyspell for text files and enable superword mode
  (dolist (mode '(text-mode-hook))
    (add-hook mode (lambda ()
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
    (add-hook mode (lambda ()
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
  (global-set-key (kbd "M-<f8>") 'flyspell-goto-next-error)
  ;; In Mac OS X the right mouse button does not seem to trigger
  ;; [mouse-2], so you cannot right click a word to get a suggestion.
  ;; This can be fixed with the below:
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3]
         #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3]
         #'undefined))))


;;; spell-checking.el ends here
