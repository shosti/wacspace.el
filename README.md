wacspace.el [![Build Status](https://travis-ci.org/shosti/wacspace.el.png)](https://travis-ci.org/shosti/wacspace.el)
===========

The WACky WorkSPACE manager for emACS.

## Installation and Basic Setup

Wacspace is available on [Marmalade](http://marmalade-repo.org/) and
[Melpa](http://melpa.milkbox.net/), so you can install easily with
`M-x package-install`. You can also just put `wacspace.el` on your
load path and `(require 'wacspace)`. Wacspace requires cl-lib
(included in Emacs >= 24.3) and
[dash.el](https://github.com/magnars/dash.el) (available on Marmalade
and Melpa).

The easiest way to set up wacspace is to just put `(wacs-set-up-keys)`
into your Emacs configuration somewhere. This will set up `C-z` as a
prefix key with the various wacspace commands in convenient bindings.
If for some reason you don't want to use `C-z` as a prefix, you can
put something like the following in your configuration:

```cl
(wacs-set-up-prefix)
(global-set-key (kbd "C-c C-w") 'wacs-prefix-map)
```

(In this example, `C-c C-w` would be the wacspace prefix.) If you
don't want to use the prefix map, you can also use your own custom
keybindings. The only commands you need to bind are `wacspace`,
`wacspace-save`, and `wacspace-switch-project`.

## Usage

Once you have a good configuration (see below), using wacspace is very
easy: just press `C-z C-w` (or `M-x wacspace`) when you're in a
buffer, and your workspace (windows, buffers, and frame) will be
automatically set up according to your current mode. `C-z C-w` will
use the default configuration for the mode, but you can also use
alternate workspaces (wacspace supports up to 10, including the
default). To use workspace number 3, for example, press `C-z C-3` (or
`C-3 M-x wacspace`).

### Saving and restoring

The first time you use `wacspace` on a buffer, your workspace will be
set up based on your configuration (see below). Once your workspace is
set up, wacspace will automatically save your window configuration. If
you use `wacspace` on any of the buffers that were set up this way, it
will jump to the window setup without re-running your configuration.

You can also save your configuration at any time using `C-z C-s` (or
`M-x wacspace-save`). `wacspace-save` can also use a numeric prefix,
in which case using `wacspace` with that numeric prefix will access
that saved workspace. When you kill a buffer in the configuration and
use `wacspace`, it will again set up the workspace according to your
configuration. Thus, `wacspace` should "just work" most of the time—if
you want concrete behavior examples, check out
[features/wacs-save.feature](https://github.com/shosti/wacspace.el/blob/master/features/wacspace-save.feature).
If you want to force reconfiguration of a workspace, use `C-u C-z
C-w`, which will clear the saved workspaces associated with the
current buffer.

If the variable `wacs-save-frame` is set to `t` (which it is by
default unless you're running Emacs in a terminal), `wacspace` will
save and restore frame configuration as well as window configuration.

### Quickly switching between projects

Managing project workspaces is easy with wacspace. When you use
`wacspace`, wacspace will associate your workspace with a project name
(by default, the name of the enclosing folder that contains a `.git`,
but this is configurable). You can then easily switch between projects
using `C-z C-p` (or `M-x wacspace-switch-project`), which will prompt
for a project name (for best results, use `ido-mode`, which you should
be using anyway). Wacspace will even remember which prefix key you
used last in that particular project, so you can resume right where
you left off. Wacspace also comes with a number of functions that help
you set up project-specific helper windows (see later).

## Configuration

Configuring wacspace is not much harder—you usually only need to use
one macro, `defwacspace`. Here's an example configuraton to get you
started:

```cl
(defwacspace (ruby-mode (:var rinari-minor-mode))
  (:before rinari-console)
  (:default
   (:winconf 3winv)
   (:aux1 (:buffer "*rails console*"))
   (:aux2 (:cmd eshell)))
  (:1
   (:frame full))
  (:2
   (:winconf 2winh)
   (:frame left))
  (:3
   (:main (:buffer "*rails console*"))
   (:aux1 (:buffer :main))
   (:winconf 2winh)
   (:frame right)
```

The basic form is `(defwacspace (major-mode &optional aux-cond) &body
config)`. The `aux-cond` can be either be a variable of the form
`(:var VAR)` (such as `(:var rinari-minor-mode)`, or any other minor
mode variable for that matter) or a function of the form `(:fn FN)`
(such as `(:fn (lambda () (string-match "spec\\.rb$" (buffer-name))))`.
You can also use an inline lambda instead of a `(:fn FN)` pair, or a
variable name instead of a `(:var VAR)` pair (in all cases, there is
no need for quoting).

The configuration currently supports the following options:

- `:before` A function to run before setting up the workspace.
- `:default` The default configuration.
- `:[1-9]` The configuration to use with the corresponding prefix.
  keys. Note that these inherit from the default configuration.
- `:after` A function to run after setting up the workspace.
- `:base-file` A filename to look for to find the project root (useful
  for the path helper functions). Defaults to `".git"`.
- `:project-name-fn` A function to customize the project name (should
  return a string).

Within the configurations, the following options are available:

- `:winconf` The window configuration to use (see later).
- `:frame` The frame alignment to use (see later).
- `:main` The top-left window.
- `:aux[1-5]` Auxiliary window number 1-5 (in the order of
  `other-window`).

There are 2 options to set up a window:

- `:buffer` Switch to buffer name (or `:main`, which signifies the
  buffer from which `wacspace` was invoked). If
  `wacs-regexp-buffer-switching` is set to `t` (which it is by
  default), wacspace will try to switch to the most recent buffer with
  a regexp match; if none is found, it will switch to a new buffer
  with that name.
- `:cmd` A command to invoke.

### Aliases and defaults

You can also specify default wacspaces, which will be run when no
wacspace associated with a major mode is found:

```cl
(defwacspace (:default)
 (:default
  (:aux1 (:cmd wacs-eshell))))

(defwacspace (:default rinari-minor-mode)
 (:before rinari-console)
 (:aux1 (:buffer "*rails console*"))
```

Another useful option is to specify wacspace aliases:

```cl
(defwacsalias (js-mode rinari-minor-mode)
 (ruby-mode rinari-minor-mode))
```

As you might expect, the above example means that when you're in a
buffer in `js-mode` and `rinari-minor-mode`, `wacspace` will run as if
you're in `ruby-mode` and `rinari-minor-mode`.

One thing to be aware of is the specific order in which `wacspace`
will look for configurations:

1. Wacspaces with auxiliary conditions
2. Aliases with auxiliary conditions
3. Wacspaces without auxiliary conditions
4. Aliases without auxiliary conditions
5. `:default` wacspace with auxiliary conditions
6. `:default` wacspace without auxiliary conditions

### Path Helpers

Wacspace comes with some nice path functions to help set up auxiliary
windows. When `wacspace` is invoked, the variable `wacs-main-buffer`
is automatically bound to the buffer from which it was invoked, so you
can use it in any functions that set up windows or that test for
auxiliary conditions. The function `wacs-project-dir` finds the base
project directory by looking for a file or folder equal to
`wacs-project-base-file`. This variable defaults to `".git"` but can
be set globally or on a per-wacspace basis with the option
`:base-file`.

Wacspace also provides functions `wacs-eshell` and `wacs-shell`,
designed to be used within wacspace configurations, that open an
eshell or shell (respectively) in the project directory. If you have
multiple files in the same directory, they will share the same eshell
or shell buffer.

### Window Configurations

A winconf is basically just a function to set up your windows. You can
define your own using `defwinconf`:

```cl
(defwinconf my-cool-winconf
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (split-window-below)
  (other-window 3)
  (split-window-left))
```

(This is a terrible example of a usable window configuration). You can
assume that your function will start with just one window and you
don't have to worry about having the primary window active in the end.
Wacspace comes with some nice winconfs built in:

#### 1win

    +----------------------+
    |                      |
    |                      |
    |                      |
    |        :main         |
    |                      |
    |                      |
    |                      |
    +----------------------+

#### 3winv

    +----------+-----------+
    |          |           |
    |          |  :aux1    |
    |          |           |
    |  :main   +-----------+
    |          |           |
    |          |  :aux2    |
    |          |           |
    +----------+-----------+

#### 2winv

    +----------+-----------+
    |          |           |
    |          |           |
    |          |           |
    |  :main   |   :aux1   |
    |          |           |
    |          |           |
    |          |           |
    +----------+-----------+

#### 2winh

    +----------+
    |          |
    |  :main   |
    |          |
    +----------+
    |          |
    |  :aux1   |
    |          |
    +----------+

#### 4win


    +----------+-----------+
    |          |           |
    |  :main   |   :aux2   |
    |          |           |
    +----------+-----------+
    |          |           |
    |  :aux1   |   :aux3   |
    |          |           |
    +----------+-----------+

### Frame Configuration

Due to the wide variety of GUI/terminal experiences for Emacs and
screen resolutions, I haven't attempted to include frame-setting
functions with wacspace. Instead, you can use your own functions and
set them using `wacs-set-frame-fn`:

```cl
(wacs-set-frame-function full my-fill-screen-fn)
```

In the future, I might add helper functions to set up your own frame
functions.

### TODO/Contributing

Wacspace is in early alpha phase, and I plan to add more features and
fix behavior. Any comments/suggestions/pull requests are much
appreciated. Eventually, I would also like to include a variety of
default configurations for various modes.
