;;; smart-mode-line-atom-one-dark-theme.el --- Atom-one-dark theme for smart-mode-line

;; Copyright (C) 2018 Davide Restivo <davide.restivo@yahoo.it>

;; Author: Davide Restivo <davide.restivo@yahoo.it>
;; Version: 0.2
;; URL: https://github.com/daviderestivo/sml-atom-one-dark-theme
;; Package-Requires: ((emacs "24.3") (smart-mode-line "2.10"))
;; Keywords: mode-line themes faces

;;; Commentary:
;;
;;  Atom-one-dark theme for smart-mode-line

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
;; 0.2 - 2018/09/15 - Add packages dependencies, version and keywords
;; 0.1 - 2018/06/30 - Created File.
;;; Code:

(deftheme smart-mode-line-atom-one-dark
  "Atom-one-dark theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-atom-one-dark
 '(mode-line-inactive ((t :background "#282C34" :box (:line-width 3 :color "#282C34"))))
 '(mode-line     ((t :background "#2C323C" :box (:line-width 3 :color "#2C323C"))))
 '(sml/global    ((t :inherit font-lock-preprocessor-face)))
 '(sml/filename  ((t :inherit mode-line-buffer-id)))
 '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
 '(sml/read-only ((t :inherit (font-lock-type-face sml/not-modified))))
 '(sml/modes     ((t :foreground nil :inherit sml/filename :weight normal)))
 ;; Helm
 '(helm-candidate-number ((t :background "#2C323C"))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-atom-one-dark)
;;; smart-mode-line-atom-one-dark-theme.el ends here.
