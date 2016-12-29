This package provides functionality for automatically updating your Emacs
packages periodically. It is specially useful for people that work in
multiple machines and tend to forget to manually update packages from time to
time.

The main idea is that you set a desired periodicity for the updates, and when
you start Emacs, the packages will be automatically updated if enough days
have passed since the last update.

Requirements:

This package was tested for GNU Emacs 24.4 and above. Older Emacsen are not
supported yet.

Installation:

You can install via `MELPA`, or manually by downloading `auto-package-update.el` and
adding the following to your init file:

```elisp
(add-to-list 'load-path "/path/to/auto-package-update")
(require 'auto-package-update)
```
