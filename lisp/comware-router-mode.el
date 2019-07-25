;;; comware-router-mode.el --- Major mode for editing Comware configuration files

;; Copyright (C) 2019 Davide Restivo

;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; Version: 0.1
;; URL: https://github.com/daviderestivo/comware-router-mode
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience faces

;;; Commentary:
;;
;;  A major mode for editing Comware router and switches configuration
;;  file.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
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

;;; Change Log:
;; 0.1 - 2019/07/25 - First version

;;; Code:

;; Hook
(defvar comware-router-mode-hook nil
  "Hook called by \"comware-router-mode\"")

(defvar comware-router-mode-map
  (let
      ((comware-router-mode-map (make-keymap)))
    (define-key comware-router-mode-map "\C-j" 'newline-and-indent)
    comware-router-mode-map)
  "Keymap for Comware router configuration major mode")

;; Font locking definitions
(defvar comware-router-ipadd-face 'comware-router-ipadd-face "Face for IP addresses")
(defface comware-router-ipadd-face
  '((t :inherit font-lock-constant-face))
  "Face for IP addresses"
  :group 'comware-router-mode)

(defvar comware-router-service-command-face 'comware-router-service-command-face "Face for router service commands")
(defface comware-router-service-command-face
  '((t :inherit font-lock-keyword-face))
  "Face for router service commands"
  :group 'comware-router-mode)

(defvar comware-router-command-face 'comware-router-command-face "Face for router commands")
(defface comware-router-command-face
  '((t :inherit font-lock-constant-face))
  "Face for router commands"
  :group 'comware-router-mode)

(defvar comware-router-undo-face 'comware-router-undo-face "Face for \"undo\"")
(defface comware-router-undo-face
  '(
    (t (:underline t))
    )
  "Face for \"undo\""
  :group 'comware-router-mode)

(defconst comware-router-font-lock-keywords
  (let* (
         ;; Define categories of commands
         (comware-router-command '("arp"
                                   "clock"
                                   "domain"
                                   "http"
                                   "https"
                                   "hwtacacs"
                                   "info-center"
                                   "license"
                                   "line"
                                   "local-user"
                                   "ntp-service"
                                   "password-recovery"
                                   "return"
                                   "role"
                                   "scheduler"
                                   "scp"
                                   "sftp"
                                   "snmp-agent"
                                   "ssh"
                                   "sysname"
                                   "user-group"
                                   "version"))
         (comware-router-service-command  '("acl"
                                            "address-family"
                                            "bgp"
                                            "dhcp"
                                            "dhcp policy"
                                            "dhcp server"
                                            "dns"
                                            "interface"
                                            "ip community-list"
                                            "ip prefix-list"
                                            "ip route-static"
                                            "ip vpn-instance"
                                            "nqa"
                                            "ospf"
                                            "rip"
                                            "route-policy"
                                            "rtm"
                                            "vsi"))
         ;; Generate regexp strings for each category of commands
         (comware-router-command-regexp         (concat "^[[:space:]]*"
                                                        (regexp-opt comware-router-command 'word)
                                                        "\\([[:space:][:alnum:][:graph:]]*\\)"))
         (comware-router-service-command-regexp (concat "^[[:space:]]*"
                                                        (regexp-opt comware-router-service-command 'words)
                                                        "\\([[:space:][:alnum:][:graph:]]*\\)")))
    `(
      (,comware-router-command-regexp 1 comware-router-command-face)
      (,comware-router-command-regexp 2 font-lock-function-name-face)
      (,comware-router-service-command-regexp  1 comware-router-service-command-face)
      (,comware-router-service-command-regexp  2 font-lock-function-name-face)
      ("\\<\\(undo [[:space:][:alnum:][:graph:]]*\\)\\>" 1 comware-router-undo-face)
      ("\\<\\([0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\)\\>" 1 comware-router-ipadd-face)
      "Font locking definitions for comware router mode")))

;; Imenu
(defvar comware-router-imenu-expression
  '(
    ("Interfaces"        "^[\t ]*interface *\\(.*\\)" 1)
    ("VRFs"              "^ip vpn-instance *\\(.*\\)" 1)
    ("Routing protocols" "\\(^bgp [0-9]*\\|^ospf [0-9]*\\|^rip [0-9]*\\)" 1)
    ))

;; Indentation
(defun comware-router-indent-line ()
  "Indent current line as comware router config line"
  (indent-relative-first-indent-point))

;; Custom syntax table
(defvar comware-router-mode-syntax-table (make-syntax-table)
  "Syntax table for comware router mode")
(modify-syntax-entry ?_  "w" comware-router-mode-syntax-table) ;All _'s are part of words.
(modify-syntax-entry ?:  "w" comware-router-mode-syntax-table) ;All :'s are part of words.
(modify-syntax-entry ?-  "w" comware-router-mode-syntax-table) ;All -'s are part of words.
(modify-syntax-entry ?#  "<" comware-router-mode-syntax-table) ;All #'s start comments.
(modify-syntax-entry ?\n ">" comware-router-mode-syntax-table) ;All newlines end comments.
(modify-syntax-entry ?\r ">" comware-router-mode-syntax-table) ;All linefeeds end comments.

;; Entry point
(defun comware-router-mode  ()
  "Major mode for editing Comware routers/switches configuration files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table comware-router-mode-syntax-table)
  (use-local-map comware-router-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(comware-router-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'comware-router-indent-line)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *")
  (setq imenu-case-fold-search t)
  (set (make-local-variable 'imenu-generic-expression) comware-router-imenu-expression)
  (imenu-add-to-menubar "Comware")
  (setq major-mode 'comware-router-mode
	mode-name "Comware")
  (run-hooks comware-router-mode-hook))

(add-to-list 'auto-mode-alist '("\\.cmw\\'" . comware-router-mode))

(provide 'comware-router-mode)

;;; comware-router-mode.el ends here
