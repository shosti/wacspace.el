ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)
carton exec "$ECUKES" "$@"

emacs -batch -l ert -l ert/unit-tests.el -l wacspace.el -f ert-run-tests-batch-and-exit
