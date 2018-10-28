;;; load-bash-shell-aliases.el --- Convert bash aliases into eshell ones -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Davide Restivo

;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; Version: 0.0.3
;; URL: https://github.com/daviderestivo/load-bash-shell-aliases
;; Package-Requires: ((emacs "24.1"))
;; Keywords: emacs bash eshell alias

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

;;; Code:

(defgroup lbsa nil
  "Convert bash aliases into eshell ones"
  :group 'emacs)

(defcustom lbsa/bashrc-file "~/.bashrc"
  "Bash alias file."
  :type 'string)

(defcustom lbsa/exclude-aliases-regexp "^alias magit\\|^alias oc"
  "Regexp to exclude Bash aliases to be converted into eshell ones."
  :type 'string)

(defun lbsa/read-bash-file (BASHFILE)
  "Read BASHFILE and return a list of lines after merging continuation lines."
  (with-temp-buffer
    (progn
      (insert-file-contents BASHFILE)
      ;; Merge continuation lines into single line. The below regexp
      ;; matches a '\' at the end of a line followed by one or
      ;; multiple TAB or spaces.
      (while (re-search-forward "\\\\[ \t]*\n" nil t)
        (replace-match ""))
      ;; Return a list of lines
      (split-string (buffer-string) "\n" t))))

(defun lbsa/extract-bash-aliases (LIST)
  "Take a LIST of strings and extract Bash aliases from it."
  (seq-filter (lambda (element)
	        (and
	         (string-match-p "alias" element)
	         (not (string-match-p "^#" element))
                 (not (string-match-p lbsa/exclude-aliases-regexp element))))
	      LIST))

(defun lbsa/load-bash-aliases-into-eshell ()
  "Convert bash aliases into eshell ones.

Take the file specified in `lbsa/bashrc-file', trims it to a
list of alias commands, and inserts them as eshell aliases."
  (interactive)
  (if (file-exists-p lbsa/bashrc-file)
      (progn
        (eshell)
        (dolist
            (element
             (lbsa/extract-bash-aliases (lbsa/read-bash-file lbsa/bashrc-file)))
          ;; After multiple withespaces and tabs into single
          ;; withespace convert a bash alias into an eshell one by
          ;; removing the "=" sign.
          (let ((trimmed (replace-regexp-in-string "=\\|[ \t]+" " " element)))
            (goto-char (point-max))
            (insert trimmed)
            (eshell-send-input))))
    (message "The Bash file set on lbsa/bashrc-file does not exists!")))

(provide 'lbsa/load-bash-aliases-into-eshell)
;;; load-bash-shell-aliases.el ends here
