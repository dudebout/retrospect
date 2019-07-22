.PHONY: test
test:
	@emacs -Q --batch -L . --eval "(progn\
	(load-file \"retrospect-tests.el\")\
	(ert-run-tests-batch-and-exit))"

.PHONY: run
run:
	@emacs -Q -L . --eval "(progn\
	(load-file \"retrospect-tests.el\")\
	(run-interactive \"end-to-end\" end-to-end-buckets))"
