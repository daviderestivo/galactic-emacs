On OS X (and perhaps elsewhere) the $PATH environment variable and
`exec-path' used by a windowed Emacs instance will usually be the
system-wide default path, rather than that seen in a terminal
window.

This library allows the user to set Emacs' `exec-path' and $PATH
from the shell path, so that `shell-command', `compile' and the
like work as expected.

It also allows other environment variables to be retrieved from the
shell, so that Emacs will see the same values you get in a terminal.

If you use a non-POSIX-standard shell like "tcsh" or "fish", your
shell will be asked to execute "sh" as a subshell in order to print
out the variables in a format which can be reliably parsed. "sh"
must be a POSIX-compliant shell in this case.

Note that shell variables which have not been exported as
environment variables (e.g. using the "export" keyword) may not be
visible to `exec-path-from-shell'.

Installation:

ELPA packages are available on Marmalade and MELPA. Alternatively, place
this file on a directory in your `load-path', and explicitly require it.

Usage:

    (require 'exec-path-from-shell) ;; if not using the ELPA package
    (exec-path-from-shell-initialize)

Customize `exec-path-from-shell-variables' to modify the list of
variables imported.

If you use your Emacs config on other platforms, you can instead
make initialization conditional as follows:

    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

Alternatively, you can use `exec-path-from-shell-copy-envs' or
`exec-path-from-shell-copy-env' directly, e.g.

    (exec-path-from-shell-copy-env "PYTHONPATH")
