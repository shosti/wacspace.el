ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)
DASH=$(find elpa/dash-*/dash.el)
carton exec "$ECUKES" "$@"
emacs -batch -l cl-lib -l ert -l tests/unit-tests.el -l $DASH -l wacspace.el -f ert-run-tests-batch-and-exit
