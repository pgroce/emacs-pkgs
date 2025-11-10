

autovenv aims to automatically manage the use of Python virtual
environments in Emacs by always making sure the "right" virtual
environment is active for the curent buffer.

autovenv only tends to the core configuration of the Emacs
environment to support virtual environments: Setting the
VIRTUAL_ENV environment variable, and adding the relevant bin
directory to exec-path and the PATH environment
variable. Furthermore, its algorithm for locating virtual
environments is very simple. As a result, it has no dependencies
that don't ship with Emacs.

That said, the user can override the location process to use a
different scheme if desired, and hooks around activation and
deactivation permit maintenance of additional state if needed, such
as reloading LSPs, so autovenv can serve as the core of a workflow
that encompasses many other tools, such as Projectile or LSP Mode.

This is a fork of auto-virtualenv.el
<https://github.com/marcwebbie/auto-virtualenv> by Markwebbie. Most
of it has been rewritten, but some of the original code survives,
and the template has been very helpful.

To use this package, add the `autoenv-find-and-activate' function
to hooks associated with changes to virtual
environments. `find-file-hook', for instance, is a good candidate,
as are hooks for shell buffers that are run when the
default-directory changes. If desired, customize the hooks in the
`autovenv' Customize group to execute custom behavior on activation
or deactivation, or override the location of a virtual environment
with a custom fuction.

The following use-package declaration illustrates one of the
simplest configurations:

  (use-package autovenv
    :ensure t
    :hook ((find-file-hook
            eshell-directory-change-hook) . autovenv-find-and-activate))

