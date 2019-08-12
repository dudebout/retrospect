# A retrospective mode for org

Determine how time you logged in org was spent. Use this information to bill
clients, or understand where you spend your time, etc. `retrospect` categorizes
org entries with logged time into buckets, which are defined in the
`retrospect-buckets` variable, and displays a summary in the `*retrospect*`
buffer.

# See `retrospect` in action

The easiest way to see `retrospect` in action is to run `make demo` from the
root of this repository. This demo uses buckets defined in
[`retrospect-tests.el`](retrospect-tests.el) to analyze the file
[`input.org`](test/input.org) and display the content of
[`demo.golden`](test/demo.golden). This output is part of the repository because
it is used in automated tests run via `make test`.

The output is displayed in the `*retrospect*` buffer. This buffer uses
`org-mode` as its major mode with `retrospect-mode` minor mode enabled. The
`*retrospect*` buffer is read-only and `retrospect-mode` provides a number of
key bindings described below.

Buffer-level key bindings:

  + `g`: refresh the `*retrospect*` buffer
  + `q`: bury the `*retrospect*` buffer

Org link navigation key bindings:

  + `n`/`p`: navigate between the different links
  + `<return>`: visit a link

# Setting up your own buckets

Use the following snippet to get started with your own data:

```elisp
(require 'retrospect)
(setq retrospect-source-filename "PATH_TO_ORG_FILE"
      retrospect-buckets
        '(:names
          ((sole-bucket . "Sole Bucket"))
          :classifier
          (lambda () 'sole-bucket)))
(global-set-key (kbd "C-c t") #'retrospect)
```

This snippet configure a single bucket, identified by the symbol
`sole-bucket`. Its humnan-friendly name, used in the `*retrospect*` buffer, is
`Sole Bucket`. The classifier `(lambda () sole-bucket)` is applied to each org
entry in the source file. Since it always returns the symbol `sole-bucket`, all
the entries end up in the sole bucket.

# `retrospect` key concepts

Buckets form a partition of the org entries in the input file, i.e., each entry
is in one and only one bucket. Entries are prevented from being put in multiple
buckets to avoid counting time spent multiple times.

Only the buckets listed under `retrospect-buckets`'s `:names` property are
displayed in the `*retrospect*` buffer.
