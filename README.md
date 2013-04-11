wacspace.el
===========

The WACky WorkSPACE manager for emACS.

## Installation

Wacspace is available on [Marmalade](http://marmalade-repo.org/) and
[Melpa](http://melpa.milkbox.net/), so you can install easily with
`M-x package-install`. You can also just put `wacspace.el` on your
load path and `(require 'wacspace)`. Wacspace requires cl-lib
(included in Emacs >= 24.3) and
[dash.el](https://github.com/magnars/dash.el) (available on Marmalade
and Melpa).

## Usage

Using wacspace is very easy: just bind `wacspace` and `wacspace-save`
to convenient key sequences like so:

```cl
(global-set-key (kbd "C-c w") 'wacspace)
(global-set-key (kbd "C-c s") 'wacspace-save)
```

(You can use any keybindings you like.) Then, use `wacspace` when
you're visiting a file of a particular type, with or without a prefix
key. Your workspace (windows, buffers, and frame alignment) will be
automatically set up. To use an alternate workspace for a given file
type, use numeric prefix keys—wacspace supports up to 10
configurations for a given file type.

### Saving and restoring

The first time you use `wacspace` on a buffer, your workspace will be
set up based on your configuration (see below). Once your workspace is
set up, wacspace will automatically save your window configuration. If
you use `wacspace` on any of the buffers that were set up this way, it
will jump to the window setup without re-running your configuration.

You can also save your configuration at any time using
(unsurprisingly) `wacspace-save`. `wacspace-save` can also use a
numeric prefix, in which case using `wacspace` with that numeric
prefix will access that saved workspace. When you kill a buffer in the
configuration and use `wacspace`, it will again set up the workspace
according to your configuration. Thus, `wacspace` should "just work"
most of the time—if you want concrete behavior examples, check out
[features/wacs-save.feature](https://github.com/shosti/wacspace.el/blob/master/features/wacspace-save.feature).

The frame alignment is not saved/restored like the rest of the
configuration. Instead, if you specify a frame alignment for a prefix
key in your configuration, that frame alignment will always be
restored when you use `wacspace` with that prefix key. This behavior
is subject to change in future versions of wacspace. For instance,
with the example configuration below, if you set up a workspace with
prefix key `1`, any time you run `C-1 wacspace` in any of the windows
set up by that workspace, the frame will be full.

## Configuration

Configuring wacspace is a little trickier, but you usually only need
to use one macro: `defwacspace`. Here's an example configuraton to get
you started:

```cl
(defwacspace (ruby-mode rinari-minor-mode)
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
config)`. The `aux-cond` can either be a variable (such as
`rinari-minor-mode`, or any other minor mode variable for that matter)
or an inline lambda, such as `(lambda () (string-match "spec\\.rb$"
(buffer-name)))` (note that there is no need for quoting the lambda).
If you don't provide an auxiliary condition, the wacspace will be the
default for the major mode.

The configuration currently supports the following options:

- `:before` A function to run before setting up the workspace
- `:default` The default configuration
- `:[1-9]` The configuration to use with the corresponding prefix
  keys. Note that these inherit from the default configuration.
- `:after` A function to run after setting up the workspace

Within the configurations, the following options are available:

- `:winconf` The window configuration to use (see later)
- `:frame` The frame alignment to use (see later)
- `:main` The top-left window
- `:aux[1-5]` Auxiliary window number 1-5 (in the order of
  `other-window`)

There are 2 options to set up a window:

- `:buffer` Switch to buffer name (or `:main`, which signifies the
  buffer from which `wacspace` was invoked). If
  `wacs-regexp-buffer-switching` is set to `t` (which it is by
  default), wacspace will try to switch to the most recent buffer with
  a regexp match; if none is found, it will switch to a new buffer
  with that name.
- `:cmd` a command to invoke

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
