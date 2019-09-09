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
  + `s`: toggle the display of the summary section
  + `d`: toggle the display of the detail section
  + `e`: toggle the display of empty buckets
  + `l`: toggle the use of org-links to the heading with logged time
  + `f`: toggle the formatting of time between absolute and percentage of total
    logged time


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

The buckets list under `retrospect-buckets`'s `:names` property is meant to
cover all the buckets under which time is being logged. If time is logged
against a bucket not in that list, that time will be displayed in the `* Errors`
section of the `*retrospect*` buffer.

The time range over which `retrospect` is looking for logged time is specified
via the `retrospect-time-range`'s properties `:tstart` and `:tend`.

A summary section can be displayed by setting the variable
`retrospect-display-summary` to `t`. That section displays the percentage of
total time logged against each bucket.

Before computing the summary, it is possible to transfer the content of certain
buckets into other buckets, or drop it, via the variable
`retrospect-summary-transfers`. This allows one to dissect the logged time data
in different ways.

By default, `retrospect`  does not display empty buckets,  i.e., buckets against
which  no time  was logged.  Setting `retrospect-display-empty-buckets`  to `t`,
displays their content in the summary and in the details sections

# `nix` expressions

`nix` expressions are provided to run repeatable tests. To take advantage of
them, replace uses of `make X` above by `nix-shell --run 'make X'`.

`shell.nix` provides an environment containing `emacsWithpackages [retrospect]`,
where the `retrospect` derivation is built from the local `retrospect.el` using
the `melpaBuild` recipe in `package.nix`.
