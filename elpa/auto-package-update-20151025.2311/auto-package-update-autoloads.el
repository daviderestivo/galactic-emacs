;;; auto-package-update-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "auto-package-update" "auto-package-update.el"
;;;;;;  (22629 24490 0 0))
;;; Generated autoloads from auto-package-update.el

(autoload 'auto-package-update-now "auto-package-update" "\
Update installed Emacs packages.

\(fn)" t nil)

(autoload 'auto-package-update-at-time "auto-package-update" "\
Try to update every day at the specified TIME.

\(fn TIME)" nil nil)

(autoload 'auto-package-update-maybe "auto-package-update" "\
Update installed Emacs packages if at least `auto-package-update-interval' days have passed since the last update.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("auto-package-update-pkg.el") (22629 24469
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; auto-package-update-autoloads.el ends here
