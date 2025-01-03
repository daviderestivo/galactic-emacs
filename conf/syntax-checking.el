;;; syntax-checking.el --- Galactic Emacs syntax checking configuration -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2025 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/syntax-checking.el
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

;; Galactic Emacs syntax checking configuration based on Flycheck.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


;; On the fly syntax checking for GNU Emacs
(use-package flycheck
  :ensure t
  :defer t
  :after (flycheck-color-mode-line flycheck-pos-tip)
  :hook
  (flycheck-mode . flycheck-pos-tip-mode)
  (flycheck-mode . flycheck-color-mode-line-mode))

;; An Emacs minor-mode for Flycheck which colors the mode-line
;; according to the Flycheck state of the current buffer.
(use-package flycheck-color-mode-line
  :ensure t
  :defer t)

;; Flycheck errors display in tooltip
(use-package flycheck-pos-tip
  :ensure t
  :defer t)

;;; syntax-checking.el ends here
