markdown-mode is a major mode for editing [Markdown][]-formatted
text. The latest stable version is markdown-mode 2.1, released on
January 9, 2016. See the [release notes][] for details.
markdown-mode is free software, licensed under the GNU GPL.

![Markdown Mode Screenshot](http://jblevins.org/projects/markdown-mode/screenshots/20160108-001.png)

[Markdown]: http://daringfireball.net/projects/markdown/
[release notes]: http://jblevins.org/projects/markdown-mode/rev-2-1

Installation:

The recommended way to install markdown-mode is to install the package
from [MELPA Stable](https://stable.melpa.org/#/markdown-mode)
using `package.el'. First, configure `package.el' and the MELPA Stable
repository by adding the following to your `.emacs', `init.el',
or equivalent startup file:

    (require 'package)
    (add-to-list 'package-archives
                 '("melpa-stable" . "https://stable.melpa.org/packages/"))
    (package-initialize)

Then, after restarting Emacs or evaluating the above statements, issue
the following command: `M-x package-install RET markdown-mode RET`.
When installed this way, the major modes `markdown-mode' and `gfm-mode'
will be autoloaded and `markdown-mode' will be used for file names
ending in either `.md` or `.markdown`.

Alternatively, if you manage loading packages with [use-package][]
then you can automatically install and configure `markdown-mode' by
adding a declaration such as this one to your init file (as an
example; adjust settings as desired):

    (use-package markdown-mode
      :ensure t
      :commands (markdown-mode gfm-mode)
      :mode (("README\\.md\\'" . gfm-mode)
             ("\\.md\\'" . markdown-mode)
             ("\\.markdown\\'" . markdown-mode))
      :init (setq markdown-command "multimarkdown"))

[MELPA Stable]: http://stable.melpa.org/
[use-package]: https://github.com/jwiegley/use-package

**Direct Download**

Alternatively you can manually download and install markdown-mode.
First, download the [latest stable version][markdown-mode.el] and
save the file where Emacs can find it (i.e., a directory in your
`load-path'). You can then configure `markdown-mode' and `gfm-mode'
to load automatically by adding the following to your init file:

    (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

    (autoload 'gfm-mode "markdown-mode"
       "Major mode for editing GitHub Flavored Markdown files" t)
    (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

[markdown-mode.el]: http://jblevins.org/projects/markdown-mode/markdown-mode.el

**Development Version**

To follow or contribute to markdown-mode development, you can
browse or clone the Git repository
[on GitHub](https://github.com/jrblevin/markdown-mode):

    git clone https://github.com/jrblevin/markdown-mode.git

If you prefer to install and use the development version, which may
become unstable at some times, you can either clone the Git
repository as above or install markdown-mode from
[MELPA](https://melpa.org/#/markdown-mode).

If you clone the repository directly, then make sure that Emacs can
find it by adding the following line to your startup file:

    (add-to-list 'load-path "/path/to/markdown-mode/repository")

**Packaged Installation**

markdown-mode is also available in several package managers. You
may want to confirm that the package you install contains the
latest stable version first (and please notify the package
maintainer if not).

   * Debian Linux: [elpa-markdown-mode][] and [emacs-goodies-el][]
   * Ubuntu Linux: [elpa-markdown-mode][elpa-ubuntu] and [emacs-goodies-el][emacs-goodies-el-ubuntu]
   * RedHat and Fedora Linux: [emacs-goodies][]
   * NetBSD: [textproc/markdown-mode][]
   * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
   * FreeBSD: [textproc/markdown-mode.el][freebsd-port]

 [elpa-markdown-mode]: https://packages.debian.org/sid/lisp/elpa-markdown-mode
 [elpa-ubuntu]: http://packages.ubuntu.com/search?keywords=elpa-markdown-mode
 [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
 [emacs-goodies-el-ubuntu]: http://packages.ubuntu.com/search?keywords=emacs-goodies-el
 [emacs-goodies]: https://apps.fedoraproject.org/packages/emacs-goodies
 [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
 [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
 [macports-ticket]: http://trac.macports.org/ticket/35716
 [freebsd-port]: http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el

**Dependencies**

`markdown-mode' depends on `cl-lib', which has been bundled with
GNU Emacs since 24.3.  Users of GNU Emacs 24.1 and 24.2 can install
`cl-lib' with `package.el'.
