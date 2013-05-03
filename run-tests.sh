ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)
DASH=$(find elpa/dash-*/dash.el)
carton exec "$ECUKES" "$@"
emacs -batch -l ert/ert.el -l ert/unit-tests.el -l $DASH -l wacspace.el -f ert-run-tests-batch-and-exit
