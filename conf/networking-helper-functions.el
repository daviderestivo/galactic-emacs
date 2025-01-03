;;; networking-helper-functions.el --- Galactic Emacs networking helper functions -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2025 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/networking-helper-functions.el
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

;; Galactic Emacs networking helper functions
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


(defun ipv6-address-normalize (ipv6-address)
  "Convert an IPV6-ADDRESS into normal form.

For example \"2001:0DB8:AC10:FE01::1A\" is normalized to
\"2001:0DB8:AC10:FE01:0000:0000:0000:001A\""
  (let*
      ((ipv6-address-list (split-string ipv6-address "[:]"))
       (ipv6-address-list-len (length ipv6-address-list))
       (start (seq-take ipv6-address-list (seq-position ipv6-address-list "")))
       (end (seq-drop ipv6-address-list (1+ (seq-position ipv6-address-list ""))))
       (middle (make-list (- 9 ipv6-address-list-len) ""))
       (output (seq-concatenate 'list start middle end)))
    (mapconcat #'identity
               (mapcar (lambda (hex) (format "%04X" (string-to-number hex 16)))
                       output)
               ":")))

(defun ipv4-to-ipv6-convert (ipv6-subnet ipv4-address)
  "Convert a decimal IPV4-ADDRESS into hexadecimal and append it to
IPV6-SUBNET"
  (let
      ((hex-ipv4 (string-join
                  (mapcar (lambda (dec) (format "%02X" (string-to-number dec)))
                          (split-string ipv4-address "[.]")))))
    (concat ipv6-subnet (substring hex-ipv4 0 4) ":" (substring hex-ipv4 4 8))))

(defun ipv6-to-ipv4-convert (ipv6-address)
  "Convert the last 32 bits of an IPV6-ADDRESS into dotted
decimal IPv4 address"
  (let*
      ((normalized-ipv6-address (ipv6-address-normalize ipv6-address))
       (normalized-ipv6-address-list (split-string normalized-ipv6-address "[:]"))
       (ipv4-fields (seq-drop normalized-ipv6-address-list 6))
       (ipv4-address-list (seq-concatenate 'list
                                           (list (substring (car ipv4-fields) 0 2))
                                           (list (substring (car ipv4-fields) 2 4))
                                           (list (substring (car (cdr ipv4-fields)) 0 2))
                                           (list (substring (car (cdr ipv4-fields)) 2 4)))))
    (string-join
     (mapcar (lambda (hex) (format "%d" (string-to-number hex 16)))
             ipv4-address-list) ".")))

;; TODO
(defun ipv6-address-shorten (ipv6-address)
  ""
  )


;;; networking-helper-functions.el ends here
