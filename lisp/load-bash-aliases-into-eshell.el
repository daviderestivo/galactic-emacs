;;; load-bash-aliases-into-eshell.el - Convert bash aliases into eshell ones

;;
;; Copyright (C) 2018 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/emacs-config/blob/master/lisp/bash-to-eshell-aliases.el
;; Version: 0.0.1
;; Keywords: emacs bash eshell alias


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

;; Convert bash aliases into eshell ones

(require 'seq)

(setq lbsa/bashrc-file "~/.bashrc")

(defun lbsa/read-lines-from-file (FILEPATH)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents FILEPATH)
    (split-string (buffer-string) "\n" t)))

(defun lbsa/extract-bash-aliases (LIST)
  "Takes a LIST of strings, and transforms it into a LIST of shell aliases."
  (seq-filter (lambda (element)
	    (and
	     (string-match-p "alias" element)
	     (not (string-match-p "^#" element))))
	  LIST))

(defun lbsa/load-bash-aliases-into-eshell ()
  "Takes a BASHFILE, trims it to a list of alias commands, and
convert it to a list of eshell aliases."
  (let ((aliases (list)))
    (dolist (element (lbsa/extract-bash-aliases (lbsa/read-lines-from-file lbsa/bash-to-eshell-file)))
      (push (split-string (replace-regexp-in-string "alias " "" element) "=")
            eshell-command-aliases-list))))


(defun lbsa/bash-to-eshell-aliases (BASHFILE)
  "Takes a BASHFILE, trims it to a list of alias commands, and inserts them as eshell aliases."
  (interactive "f")
  (eshell)
  (dolist (element (lbsa/extract-bash-aliases (lbsa/read-lines-from-file BASHFILE)))
    (let ((trimmed (replace-regexp-in-string "=" " " element)))
      (goto-char (point-max))
      (insert trimmed)
      (eshell-send-input))))

(provide 'lbsa/load-bash-aliases-into-eshell)
;;; load-bash-aliases-into-eshell.el ends here
