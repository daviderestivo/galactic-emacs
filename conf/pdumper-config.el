;;; pdumper-config.el --- Galactic Emacs pdumper configuration file

;;
;; Copyright (C) 2016-2020 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/pdumper-config.el
;; Version: 11.0.0
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

;; Galactic Emacs pdumper configuration file.


(require 'cl-lib)

;; Pdumper init file
(setq galactic-emacs-pdumper-init-file
      (expand-file-name "conf/pdumper-init.el"
                        user-emacs-directory))

;; Pdumper dump file
(setq galactic-emacs-pdumper-dump-file
      (expand-file-name ".cache/dumps/emacs.pdmp"
                        user-emacs-directory))

;; Pdumper buffer name
(setq galactic-emacs-pdumper-buffer-name "*galactic-emacs-dumper*")

;; Pdumper excluded packages
(setq galactic-emacs-pdumper-excluded-packages
      '(ob-ipython org-beautify-theme org-bullets org-download org-mind-map org-plus-contrib))

;; Pdumper included packages
(setq galactic-emacs-pdumper-included-packages
      (cl-set-difference
       package-selected-packages galactic-emacs-pdumper-excluded-packages))

(defun galactic-emacs-dump-emacs ()
  "Dump Emacs in a subprocess."
  (interactive)
  (split-window-below)
  (other-window 0)
  (switch-to-buffer galactic-emacs-pdumper-buffer-name)
  (read-only-mode)
  (local-set-key (kbd "q") (lambda () (interactive)
                             (kill-this-buffer)
                             (delete-window)))
  (make-process
   :name "galactic-emacs-dumper"
   :buffer galactic-emacs-pdumper-buffer-name
   :command
   (list "emacs"
         "--batch"
         "-q"
         "-l" galactic-emacs-pdumper-init-file)))


;;; pdumper-config.el ends here
