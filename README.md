wacspace.el
===========

The WACky WorkSPACE manager for emACS.

## Installation

Wacspace will hopefully soon be available on
[marmalade](http://marmalade-repo.org/) and
[Melpa](http://melpa.milkbox.net/), so you can install easily with
`M-x package-install`. Until then, you can put `wacspace.el` on your load
path and `(require 'wacspace)`. Wacspace requires Emacs version 24.3
or greater.

## Usage

Using wacspace is very easy: just bind `wacspace` to a convenient key
sequence like so:

    (global-set-key (kbd "C-c w") 'wacspace) ; or whichever keys you prefer

Then, use it when you're visiting a file of a particular type. Your
workspace (windows, buffers, and frame alignment) will be
automatically set up based on the major mode and other conditions in
your configurations. To use an alternate workspace for a given file
type, use numeric prefix keys—wacspace supports up to 10
configurations for a given file type.

## Configuration

Configuring wacspace is a little harder, but you usually only need to
use one macro: `defwacspace`. Here's an example to get you started:

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

The basic form is `(defwacspace (major-mode &optional aux-cond) &body
config)`. The `aux-cond` can either be a variable (such as
`rinari-minor-mode`, or any other minor mode variable for that matter)
or an inline lambda, such as `(lambda () (string-match "spec\\.rb$"
(buffer-name)))` (note that there is no need for quoting). If you
don't provide an auxiliary condition, the wacspace will be the default
for the major mode and will be used when none of the auxiliary
conditions for other wacspaces are met.

The configuration currently supports the following options:

- `:before`: A function to run before setting up the workspace
- `:default`: The default configuration
- `:[1-9]`: The configuration to use with the corresponding prefix
  keys. Note that these inherit from the default configuration.
- `:after`: A function to run after setting up the workspace

Within the configurations, the following options are available:

- `:winconf`: The window configuration to use (see later)
- `:frame`: The frame alignment to use (see later)
- `:main`: The top-left window
- `:aux[1-5]`: Auxiliary window number [1-5] (in the order of
  `other-window`)

There are 2 options to set up a window:

- `:buffer`: Switch to buffer name (or `:main`, which signifies the
  buffer from which `wacspace` was invoked)
- `:cmd`: a command to invoke

### Window Configurations

A winconf is basically just a function to set up your windows. You can
define your own using `defwinconf`:

    (defwinconf my-cool-winconf
      (split-window-right)
      (split-window-right)
      (split-window-below)
      (split-window-below)
      (other-window 3)
      (split-window-left))

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
    |          |           |
    +----------+-----------+

#### 2winh

    +----------+
    |          |
    |  :main   |
    |          |
    |          |
    +----------+
    |          |
    |  :aux1   |
    |          |
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
    |          |           |
    +----------+-----------+

### Frame Configuration

Due to the wide variety of GUI/terminal experiences for Emacs and
screen resolutions, I haven't attempted to include frame-setting
functions with wacspace. Instead, you can use your own functions and
set them using `wacs-set-frame-fn`:

    (wacs-set-frame-function full my-fill-screen-fn)

In the future, I might add helper functions to set up your own frame
functions.

### TODO/Contributing

Wacspace is in early alpha phase, and I plan to add a lot more
features, such as saving and restoring workspaces. Any feature/pull
requests are welcome.  Eventually, I would also like to include a
variety of default configurations for various modes.
