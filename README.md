# A retrospective mode for org

Determine how time you logged in org was spent. Use this information to bill
clients, or understand where you spend your time, etc. `retrospect` categorizes
org entries with logged time into buckets, which are defined in the
`retrospect-buckets` variable, and displays a summary in the `*retrospect*`
buffer.

# See `retrospect` in action

The easiest way to see `retrospect` in action is to run `make run` from the root
of this repository. This demo uses buckets defined in
[`retrospect-tests.el`](retrospect-tests.el) to analyze the file
[`end-to-end.org`](test/end-to-end.org) and display the content of
[`end-to-end.golden`](test/end-to-end.golden). This output is part of the
repository because it is used in automated tests run via `make test`.

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
          :classifiers
          (((lambda () t) . sole-bucket))))
(global-set-key (kbd "C-c t") #'retrospect)
```

This snippet configure a single bucket, identified by the symbol
`sole-bucket`. Its humnan-friendly name, used in the `*retrospect*` buffer, is
`Sole Bucket`. The predicate `(lambda () t)` is applied to each org entry in the
source file. Since it always returns `t`, all the entries end up in the sole
bucket.
