This package adds git integration to Ibuffer.  Two columns are
defined, git-status and git-status-mini.

To actually make these columns show up, you need to customize
`ibuffer-formats'.  The symbol `git-status-mini' can be inserted
where you want it, and `git-status' should be inserted with
something as something like `(git-status 8 8 :left)', where 8 is
the number you picked for `ibuffer-git-column-length'.
