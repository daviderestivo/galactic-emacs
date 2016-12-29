The main function, `ace-window' is meant to replace `other-window'.
In fact, when there are only two windows present, `other-window' is
called.  If there are more, each window will have its first
character highlighted.  Pressing that character will switch to that
window.

To setup this package, just add to your .emacs:

   (global-set-key (kbd "M-p") 'ace-window)

replacing "M-p"  with an appropriate shortcut.

Depending on your window usage patterns, you might want to set

   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

This way they are all on the home row, although the intuitive
ordering is lost.

If you don't want the gray background that makes the red selection
characters stand out more, set this:

   (setq aw-background nil)

If you want to know the selection characters ahead of time, you can
turn on `ace-window-display-mode'.

When prefixed with one `universal-argument', instead of switching
to selected window, the selected window is swapped with current one.

When prefixed with two `universal-argument', the selected window is
deleted instead.
