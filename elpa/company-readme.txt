Company is a modular completion framework.  Modules for retrieving completion
candidates are called backends, modules for displaying them are frontends.

Company comes with many backends, e.g. `company-etags'.  These are
distributed in separate files and can be used individually.

Enable `company-mode' in all buffers with M-x global-company-mode.  For
further information look at the documentation for `company-mode' (C-h f
company-mode RET).

If you want to start a specific backend, call it interactively or use
`company-begin-backend'.  For example:
M-x company-abbrev will prompt for and insert an abbrev.

To write your own backend, look at the documentation for `company-backends'.
Here is a simple example completing "foo":

(defun company-my-backend (command &optional arg &rest ignored)
  (pcase command
    (`prefix (company-grab-symbol))
    (`candidates (list "foobar" "foobaz" "foobarbaz"))
    (`meta (format "This value is named %s" arg))))

Sometimes it is a good idea to mix several backends together, for example to
enrich gtags with dabbrev-code results (to emulate local variables).  To do
this, add a list with both backends as an element in `company-backends'.
