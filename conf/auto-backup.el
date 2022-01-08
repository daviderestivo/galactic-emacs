;;; auto-backup.el --- Galactic Emacs auto-backup settings -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2016-2022 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/auto-backup.el
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

;; Galactic Emacs auto-backup configuration.
;;
;; This file is part of the Galactic Emacs configuration. Feel free to
;; drop me an email in case of questions or if you want to
;; collaborate.


;; Auto save very often
;; Save every 20 characters typed (this is the minimum)
(setq auto-save-interval 20)
;;
;; An explanation of `kept-old-versions' and `kept-new-versions'
;; variables (taken from the GNU/Emacs manual):
;;
;; "The two variables kept-old-versions and kept-new-versions
;; control this deletion. Their values are, respectively, the number
;; of oldest (lowest-numbered) backups to keep and the number of
;; newest (highest-numbered) ones to keep, each time a new backup is
;; made. The backups in the middle (excluding those oldest and newest)
;; are the excess middle versions—those backups are deleted. These
;; variables' values are used when it is time to delete excess
;; versions, just after a new backup version is made; the newly made
;; backup is included in the count in kept-new-versions. By default,
;; both variables are 2."
;;
;; An example to understand Emacs backup retention logic. Let's assume
;; kept-new-versions and kept-old-versions are both set to 2. Here’s a
;; simulation in Emacs Lisp of 8 saves using lists, showing what
;; backups are available after each save when N is 2.
;;
;;  (1)
;;  (2 1)
;;  (3 2 1)
;;  (4 3 2 1)
;;  (5 4 2 1)
;;  (6 5 2 1)
;;  (7 6 2 1)
;;  (8 7 2 1)
;;
(setq vc-make-backup-files t) ;; Backup version controlled files
(setq backup-by-copying t)    ;; Backup the files by copying it
(setq delete-old-versions t   ;; Delete excess backup files silently
      kept-old-versions 2     ;; Oldest versions to keep when a new
      ;; numbered backup is made (default: 2)
      kept-new-versions 10    ;; Newest versions to keep when a new
      ;; numbered backup is made (default: 2)
      version-control t)      ;; Version numbers for backup files

;; By default, Emacs creates a backup only when you save the first
;; time ('per-session' backup). Here we describe how to make Emacs do a
;; backup on every save ('per-save' backup), not just the first.

(defvar galactic-emacs-backup-location (expand-file-name "~/.saves")
  "Base directory for backup files.")

(defvar galactic-emacs-backup-file-size-limit (* 10 1024 1024)
  "Maximum size of a file (in bytes) that should be copied at each save point.

If a file is greater than this size, don't make a backup of it.
Default is 10 MB")

(defvar galactic-emacs-backup-trash-dir (expand-file-name "~/.saves/trash")
  "Directory for unwanted backups.")

(defvar galactic-emacs-backup-exclude-regexp nil
  "Don't back up files matching this regexp.

Files whose full name matches this regexp are backed up to
`galactic-emacs-backup-trash-dir'. Set to nil to disable this.")

;; Default and per-save backups go here:
;; N.B. backtick and comma allow evaluation of expression
;; when forming list
(setq backup-directory-alist
      `(("" . ,(expand-file-name "per-save" galactic-emacs-backup-location))))

;; Add trash directories if needed
(if galactic-emacs-backup-exclude-regexp
    (add-to-list 'backup-directory-alist `(,galactic-emacs-backup-exclude-regexp . ,galactic-emacs-backup-trash-dir)))

(defun galactic-emacs-backup-every-save ()
  "Backup files every time they are saved.

Files are backed up to `galactic-emacs-backup-location' in
sub-directories \"per-session\" once per Emacs session, and
\"per-save\" every time a file is saved.

Files whose names match the REGEXP in
`galactic-emacs-backup-exclude-regexp' are copied to
`galactic-emacs-backup-trash-dir' instead of the normal backup
directory.

Files larger than `galactic-emacs-backup-file-size-limit' are not
backed up."

  ;; Make a special "per session" backup at the first save of each
  ;; Emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist
           `(("." . ,(expand-file-name "per-session" galactic-emacs-backup-location)))))
      ;; Add trash directory if needed
      (if galactic-emacs-backup-exclude-regexp
          (add-to-list
           'backup-directory-alist
           `(,galactic-emacs-backup-exclude-regexp . ,galactic-emacs-backup-trash-dir)))
      ;; Is the file too large?
      (if (<= (buffer-size) galactic-emacs-backup-file-size-limit)
          (progn
            (message "Made per session backup of %s" (buffer-name))
            (backup-buffer))
        (message "WARNING: File %s too large to backup - increase value of galactic-emacs-backup-file-size-limit" (buffer-name)))))
  ;; Make a per-save backup on each save. The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    ;;
    ;; Is the file too large?
    ;;
    (if (<= (buffer-size) galactic-emacs-backup-file-size-limit)
        (progn
          (message "Made per save backup of %s" (buffer-name))
          (backup-buffer))
      (message "WARNING: File %s too large to backup - increase value of galactic-emacs-backup-file-size-limit" (buffer-name)))))

;; Add to save hook
(add-hook 'before-save-hook 'galactic-emacs-backup-every-save)


;;; auto-backup.el ends here
