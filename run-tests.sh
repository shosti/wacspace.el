DASH=$(find .cask/*/*/*/dash.el)
cask exec ecukes --win "$@"
emacs -batch -l cl-lib -l ert -l tests/unit-tests.el -l $DASH \
    -l wacs-util.el \
    -l wacs-configuration.el \
    -l wacs-interactive.el \
    -l wacs-helper.el \
    -l wacs-init.el \
    -l wacspace.el \
    -f ert-run-tests-batch-and-exit
