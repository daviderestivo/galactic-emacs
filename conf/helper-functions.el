;;; helper-functions.el --- Galactic Emacs Helper functions

;;
;; Copyright (C) 2016-2019 Davide Restivo
;;
;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Maintainer: Davide Restivo <davide.restivo@yahoo.it>
;; URL: https://github.com/daviderestivo/galactic-emacs/blob/master/conf/helper-functions.el
;; Version: 0.2
;; Package-Requires: ((dash "2.14.1"))
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

;; Galactic Emacs helper functions

(require 'cl-macs)

;; Search for a keyword on the ORG directory using ag
;; Requires "The Silver Searcher" (ag) to be installed:
;; On macOS use: 'brew install the_silver_searcher'
;; On a Debian based GNU/Linux distro use: 'apt-get install silversearcher-ag'
(defun galactic-emacs-org-directory-search-ag ()
  "Search for a keyword in the ORG folder using ag"
  (interactive)
  (if (not (eq org-directory nil))
      (helm-do-ag org-directory)
    (message "error: org-directory not set.")))

;; Reload Emacs init file
(defun galactic-emacs-reload-init-file ()
  "Reload your init.el file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Redefine battery-pmset because of https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-09/msg00952.html
(defun galactic-emacs-battery-pmset ()
  "Get battery status information using `pmset'.

The following %-sequences are provided:
%L Power source (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%h Remaining time in hours
%m Remaining time in minutes
%t Remaining time in the form `h:min'"
  (let (power-source load-percentage battery-status battery-status-symbol
	             remaining-time hours minutes)
    (with-temp-buffer
      (ignore-errors (call-process "pmset" nil t nil "-g" "ps"))
      (goto-char (point-min))
      (when (re-search-forward "\\(?:Currentl?y\\|Now\\) drawing from '\\(AC\\|Battery\\) Power'" nil t)
	(setq power-source (match-string 1))
	(when (re-search-forward "^ -InternalBattery-0\\([ \t]+\(id\=[0-9]+\)\\)*[ \t]+" nil t)
	  (when (looking-at "\\([0-9]\\{1,3\\}\\)%")
	    (setq load-percentage (match-string 1))
	    (goto-char (match-end 0))
	    (cond ((looking-at "; charging")
		   (setq battery-status "charging"
			 battery-status-symbol "+"))
		  ((< (string-to-number load-percentage) battery-load-critical)
		   (setq battery-status "critical"
			 battery-status-symbol "!"))
		  ((< (string-to-number load-percentage) battery-load-low)
		   (setq battery-status "low"
			 battery-status-symbol "-"))
		  (t
		   (setq battery-status "high"
			 battery-status-symbol "")))
	    (when (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\) remaining"  nil t)
	      (setq remaining-time (match-string 1))
	      (let ((h (string-to-number (match-string 2)))
		    (m (string-to-number (match-string 3))))
		(setq hours (number-to-string (+ h (if (< m 30) 0 1)))
		      minutes (number-to-string (+ (* h 60) m)))))))))
    (list (cons ?L (or power-source "N/A"))
	  (cons ?p (or load-percentage "N/A"))
	  (cons ?B (or battery-status "N/A"))
	  (cons ?b (or battery-status-symbol ""))
	  (cons ?h (or hours "N/A"))
	  (cons ?m (or minutes "N/A"))
	  (cons ?t (or remaining-time "N/A")))))

;; Create a new buffer without prompting for the name. Bound to F7
(defun galactic-emacs-new-empty-buffer ()
  "Create a new empty buffer. New buffer will be named “untitled” or
“untitled<2>”, “untitled<3>”, ..."
  (interactive)
  (let ((new-buf (generate-new-buffer "untitled")))
    (switch-to-buffer new-buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))
(global-set-key (kbd "<f7>") 'galactic-emacs-new-empty-buffer)

;; copy-line - Source https://www.emacswiki.org/emacs/CopyingWholeLines
(defun galactic-emacs-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
    Ease of use features:
     - Move to start of next line.
     - Appends the copy on sequential calls.
     - Use newline as last char even on the last line of the buffer.
     - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'galactic-emacs-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (if galactic-emacs-copy-line-append-newline
      (kill-append "\n" nil))
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun galactic-emacs-helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun galactic-emacs-org-download-method (link)
  "This is an helper function for org-download.

It creates an \"./image\" folder within the same directory of the ORG file.
Images are separated inside that image folder by additional folders one per
ORG file.

More info can be found here: https://github.com/abo-abo/org-download/issues/40.
See the commit message for an example:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir (concat
              (file-name-directory (buffer-file-name))
              (format "%s/%s/%s"
                      "images"
                      (file-name-base (buffer-file-name))
                      (org-download--dir-2)))))
    (progn
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Check if directory exists otherwise creates it
      (unless (file-exists-p dir)
        (make-directory dir t))
      (message (format "Image: %s saved!" (expand-file-name filename-with-timestamp dir)))
      (expand-file-name filename-with-timestamp dir))))

(defun galactic-emacs-insert-date ()
  (interactive)
  "Insert current datetime into buffer without a newline."
  (insert (concat "Date: " (shell-command-to-string "printf %s \"$(date)\""))))
(global-set-key (kbd "M-+") 'galactic-emacs-insert-date)

(defmacro galactic-emacs-with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun galactic-emacs-eshell-prompt ()
  "Customize eshell prompt.

This function requires `all-the-icons' package to be installed
(https://github.com/domtronn/all-the-icons.el)."
  (if (display-graphic-p)
      (setq galactic-emacs-header-bg "#282C34")
    ;; The background used when Emacs runs in a terminal
    (setq galactic-emacs-header-bg "black"))
  ;; In order to set the eshell prompt correctly we need to
  ;; distinguish between the case where we are in a local folder or
  ;; the case where we are connected to a remote server via TRAMP
  ;; (i.e.). The shell need to be temporary restored to the
  ;; default one.
  (let ((shell-file-name "/bin/sh"))
    (progn
      (if (file-remote-p default-directory)
          (progn
            (setq galactic-emacs-user-login-name (replace-regexp-in-string "\n$" ""
                                                                           (shell-command-to-string "whoami"))
                  galactic-emacs-system-name (replace-regexp-in-string "\n$" ""
                                                                       (shell-command-to-string "hostname"))
                  galactic-emacs-user-uid (string-to-number (replace-regexp-in-string "\n$" ""
                                                                                      (shell-command-to-string "id -u")))))
        (progn
          (setq galactic-emacs-user-login-name (user-login-name)
                ;; Remove the domain name from the local eshell prompt
                galactic-emacs-system-name (if (string-match-p (regexp-quote ".") system-name)
                                               (car (split-string (system-name) "\\."))
                                             (system-name))
                galactic-emacs-user-uid (user-uid))))
      (concat
       "┌─ "
       (if (display-graphic-p)
           (all-the-icons-faicon "folder-open-o")
         "")
       " "
       (galactic-emacs-with-face (concat (eshell/pwd) " ") :background galactic-emacs-header-bg)
       (if (string= (ignore-errors (vc-responsible-backend default-directory)) "Git")
           (when (ignore-errors (vc-git--run-command-string default-directory "status" "-s"))
             (progn
               (setq git-status (split-string (vc-git--run-command-string default-directory "status" "-s")))
               (galactic-emacs-with-face
                (format "[%s %s %s] "
                        (if (display-graphic-p)
                            (all-the-icons-faicon "git-square")
                          "Git")
                        (if (display-graphic-p)
                            (concat (all-the-icons-octicon  "git-branch") ":" (car (vc-git-branches)))
                          (concat "branch" ":" (car (vc-git-branches))))
                        (concat
                         "status:"
                         (if (member "A" git-status)  "A" "-")   ;; Added files (not committed)
                         (if (member "M" git-status)  "M" "-")   ;; Modified files
                         (if (member "D" git-status)  "D" "-")   ;; Deleted files
                         (if (member "??" git-status) "U" "-"))) ;; Untracked files
                :background galactic-emacs-header-bg :foreground "LightGreen"))))
       (galactic-emacs-with-face
        (concat
         "["
         (if (display-graphic-p)
             (concat (all-the-icons-material "schedule") " "))
         (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
         "]") :background galactic-emacs-header-bg :foreground "gainsboro")
       (galactic-emacs-with-face "\n└─> " :background galactic-emacs-header-bg)
       (galactic-emacs-with-face galactic-emacs-user-login-name :foreground "LightBlue")
       "@"
       (galactic-emacs-with-face galactic-emacs-system-name :foreground "LightGreen")
       (if (= galactic-emacs-user-uid 0)
           (galactic-emacs-with-face " #" :foreground "LightRed")
         " $")
       " "))))

;;
;; ORG helper functions
;; Link: https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
;;
(defun galactic-emacs-org-show-current-heading-tidily ()
  (interactive)
  "In an org file shows current entry, keeping other entries collapsed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

;;
;; Emacs frame appearance
;;
(defun galactic-emacs-setup-frame-appearance (&optional frame)
  "This function is used to setup the Emacs frame appearance in
Graphical User Interface (GUI) mode.

This function has to be invoked:
 - as a hook of `after-make-frame-functions' in order to
   run on every newly created frame. In this case the FRAME
   actual parameter is used
 - as a function `galactic-emacs-setup-frame-appearance' called
   inside your init.el file:

   (galactic-emacs-setup-frame-appearance)

   In this case the FRAME actual parameter is not needed.
   Call `galactic-emacs-setup-frame-appearance' inside init.el is
   required because the first created frame, when emacs is not
   running in daemon mode, does not have the FRAME actual
   parameter set."

  (if (or (display-graphic-p) (daemonp))
      (progn
        ;; Setup sml theme and modeline battery format
        (setq sml/theme 'atom-one-dark)
        (sml/setup)
        (setq battery-mode-line-format (concat " [" (all-the-icons-material "battery_std") "%b%p%%" "]"))
        ;; Setup other fame attributes, width, height, alpha, ...
        (if frame
            (progn
              (select-frame frame)
              ;; Always bring a newly created frame on top
              (select-frame-set-input-focus frame)
              ;; Transparent frame
              (set-frame-parameter frame
                                   `(alpha . ,galactic-emacs-frame-alpha))
              ;; Natural title bar
              (set-frame-parameter frame 'ns-transparent-titlebar 't)
              (set-frame-parameter frame 'ns-appearance 'dark)
              (set-frame-parameter frame 'height galactic-emacs-frame-height)
              (set-frame-parameter frame 'width  galactic-emacs-frame-width))
          ;; Used for the first created frame when emacs is not
          ;; running in daemon mode. See description above.
          (progn
            ;; Transparent frame
            (add-to-list 'default-frame-alist
                         `(alpha . ,galactic-emacs-frame-alpha))
            ;; Natural title bar
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            (add-to-list 'default-frame-alist '(ns-appearance . dark))
            (add-to-list 'default-frame-alist
                         `(height . ,galactic-emacs-frame-height))
            (add-to-list 'default-frame-alist
                         `(width . ,galactic-emacs-frame-width)))))))

(defun galactic-emacs-disable-number-and-visual-line ()
  (visual-line-mode 0)
  (if (version< emacs-version "26.1")
      (linum-mode 0)
    (display-line-numbers-mode 0)))

(defun galactic-emacs--outdated-packages-get ()
  "Return the list of outdated packages.

The returned list has the following structure:
((\"yang-mode\" \"yang-mode-20180306.1206\" \"yang-mode-20180306.1207\")
 (\"yasnippet\" \"yasnippet-20181015.1211\" \"yasnippet-20181015.1212\"))

Each element of the list is itself a list where the CAR is the name of
the outdated package and the CDR is the list of all the installed versions."
  (--> (directory-files (expand-file-name package-user-dir))
       (-group-by (lambda (ele) (replace-regexp-in-string "-[0-9.]+" "" ele)) it)
       (-filter (lambda (ele) (> (length ele) 2)) it)))

(defun galactic-emacs--outdated-packages-write-results-buffer (contents)
  "Write results in a buffer"
  (set-buffer
   (get-buffer-create "*Outdated Packages*"))
  (insert contents)
  (set-buffer-modified-p nil)
  (split-window-below)
  (other-window 0)
  (switch-to-buffer "*Outdated Packages*")
  (local-set-key (kbd "q") (lambda () (interactive)
                             (kill-this-buffer)
                             (delete-window))))

(defun galactic-emacs-outdated-packages-print ()
  "Print outdated packages."
  (interactive)
  (let ((outdated-package-list (galactic-emacs--outdated-packages-get)))
    (galactic-emacs--outdated-packages-write-results-buffer
     (format "%s\n"
             (if outdated-package-list
                 outdated-package-list
               "No outdated packages found.")))))

(defun galactic-emacs-outdated-packages-purge ()
  "Remove all except the latest version of the installed packages."
  (interactive)
  (let ((log-message "")
        (packages-purge-list (--> (galactic-emacs--outdated-packages-get)
                                  (mapcar (lambda (ele) (-sort #'string> (cdr ele))) it))))
    ;; `packages-purge-list' is a list of lists, so we nest two dolist
    (if packages-purge-list
        (progn
          (dolist (nested-list packages-purge-list)
            ;; The packages are ordered from newer to oldest. We need
            ;; to remove everything except the newer
            (dolist (ele (cdr nested-list))
              (progn
                (setq log-message (concat log-message
                                          (format "Deleting: %s ...\n" ele)))
                (delete-directory (concat (expand-file-name package-user-dir) "/" ele) t))))
          (setq log-message (concat log-message
                                    "... all outdated packages have been deleted.\n"))
          (galactic-emacs--outdated-packages-write-results-buffer log-message))
      (galactic-emacs--outdated-packages-write-results-buffer "No outdated packages to be deleted found.\n"))))

(defun galactic-emacs-update-config ()
  "Update Galactic Emacs configuration to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (progn
      (message "Updating Galactic Emacs configuration...")
      (cd dir)
      (shell-command "git pull")
      (message "Load new Galactic Emacs configuration...")
      (galactic-emacs-reload-init-file)
      (message "Update finished."))))

(defun galactic-emacs-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  ;; Display garbage collect start time in minibuffer and *Messages*
  (message (concat
            (format-time-string "[%Y-%m-%d %T] ") "Start garbage collect...\n"))
  ;; Don't show garbage collect details in minibuffer
  (let ((inhibit-message t))
    (message "Type    Used    Free    Total")
    (message "-----------------------------")
    (message (cl-loop for (type size used free) in (garbage-collect)
                      for used  = (* used size)
                      for free  = (* (or free 0) size)
                      for total = (file-size-human-readable (+ used free))
                      for used  = (file-size-human-readable used)
                      for free  = (file-size-human-readable free)
                      concat (format "%s: %s + %s = %s\n" type used free total))))
  ;; Display the total number of bytes of pure storage allocated
  (message (concat "Total number of pure storage bytes: "
                   (number-to-string pure-bytes-used)))
  ;; Display total time spent in garbage collection during this Emacs session
  (message (concat "Total number of seconds spent in gcs: "
                   (format "%.2f" gc-elapsed)))
  ;; Display the total number of garbage collections done during this Emacs session
  (message (concat "Total number of gcs: "
                   (number-to-string gcs-done) "\n"))
  ;; Display garbage collect end time in minibuffer and *Messages*
  (message (concat
            (format-time-string "[%Y-%m-%d %T] ") "...finished garbage collect.")))


;;; helper-functions.el ends here
