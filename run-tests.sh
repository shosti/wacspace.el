ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)
DASH=$(find elpa/dash-*/dash.el)
carton exec "$ECUKES" "$@"
emacs -batch -l cl-lib -l ert -l tests/unit-tests.el -l $DASH \
    -l wacs-util.el \
    -l wacs-configuration.el \
    -l wacs-interactive.el \
    -l wacs-helper.el \
    -l wacs-init.el \
    -l wacspace.el \
    -f ert-run-tests-batch-and-exit
