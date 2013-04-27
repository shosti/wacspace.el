ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)
DASH=$(find elpa/dash-*/dash.el)
emacs -batch -l ert.el -l ert/unit-tests.el -l $DASH -l wacspace.el -f ert-run-tests-batch-and-exit
 carton exec "$ECUKES" "$@"
