emacs = emacs -Q

# The following ensures that the retrospect.el in the current directory is used
# to run tests outside of nix-shell.
ifndef IN_NIX_SHELL
export EMACSLOADPATH=.:
endif

.PHONY: all
all: test

.PHONY: test
test: ert-test readme-test

# ------------------------------------------------------------------------------
# Unit testing
# ------------------------------------------------------------------------------
.PHONY: ert-test
ert-test:
	@$(emacs) --batch --eval "(progn\
	(load-file \"retrospect-tests.el\")\
	(ert-run-tests-batch-and-exit))"

# ------------------------------------------------------------------------------
# README snippets testing
#
# Running `make readme-run` and typing `C-c t` should bring up the *retrospect*
# buffer with a filled up Sole Bucket.
# ------------------------------------------------------------------------------
readme_el = readme.el

# Basic automated test for the snippet in the README. It is not a full
# functional test. It only ensures that:
#   + the code runs to completion without errors
#   + and produces a somewhat correct output; if the Sole bucket section is
#     empty, the length of the buffer
.PHONY: readme-test
readme-test: $(readme_el)
	@$(emacs) --batch --eval "(progn\
	(load-file \"readme.el\")\
	(retrospect)\
	(unless (> (length (buffer-string)) 100) (error \"\")))"

.PHONY: readme-run
readme-run: $(readme_el)
	@$(emacs) --load $<

# Extract the only elisp snippet, and point at an existing input file.
$(readme_el):  README.md
	@ sed -n '/^```elisp$$/,/^```$$/p;/^```$$/q' $< \
	| sed 's#PATH_TO_ORG_FILE#test/input.org#' \
	| sed '1d;$$d' \
	> $@

.PHONY: clean
clean:
	rm -f $(readme_el) result*

# ------------------------------------------------------------------------------
# Demo
#
# Running `make demo` should bring up the *retrospect* buffer with three filled
# up buckets.
# ------------------------------------------------------------------------------
.PHONY: demo
demo:
	@$(emacs) --eval "(progn\
	(load-file \"retrospect-tests.el\")\
	(setq retrospect-buckets demo-buckets)\
	(run-interactive \"input\"))"
