;;; pdumper-init.el --- Galactic Emacs pdumper init file -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2025 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/pdumper-init.el
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

;; Galactic Emacs pdumper init file.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


;;; Code:
(require 'package)

;; Load autoload files and populate load-path’s
(package-initialize)

;; Emacs dumped image
(setq galactic-emacs-pdumper-dumped t)

;; Backup load-path since it is not stored in the dump image
(setq galactic-emacs-pdumper-load-path load-path)

;; Load custom.el
(load-file (expand-file-name "custom.el"
                             user-emacs-directory))

;; Load pdumper-config.el
(load-file (expand-file-name "conf/pdumper-config.el"
                             user-emacs-directory))

;; Create dump directory if it does not exists
(make-directory (expand-file-name ".cache/dumps"
                                  user-emacs-directory) t)

;; Require use-package
(require 'use-package)
;; Require all other packages
(dolist
    (package galactic-emacs-pdumper-included-packages)
  (require package))

;; Dump image
(dump-emacs-portable galactic-emacs-pdumper-dump-file)


;;; pdumper-init.el ends here
