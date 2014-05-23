Laziness profiler for Racket
=================

(Prototype) Laziness profiler for Racket

Examples from [POPL 2014 paper][popl2014] in `examples-popl2014` directory.

[popl2014]: http://www.ccs.neu.edu/home/stchang/pubs/Chang-Felleisen-POPL2014.pdf

Invoke the profiler via `#lang`: replace `#lang racket` with `#lang s-exp
"lazy-profile.rkt"`. This will:

1. Instrument the program.
2. Run the instrumented program.
3. Analyze the collected information and present results.


On performance and optimal use of the tool (from the paper, paraphrasing):

> There is significant overhead (2-3 orders of magnitude). Thus, programmers
  should profile small representative inputs rather than an entire code
  base. Programs utilizing laziness, however, are especially suited for this
  style of testing because they generate lots of excess unused data. So long as
  the ratio of unused to used data remains large, the spots for laziness should
  remain the same, regardless of whether the absolute amount of data is large
  or small.

E.g., profiling `nqueens` for `n=5` produces the same laziness suggestions as
`n=8`.

The profiler also employs some heuristics to improve performance. Specifically,
it only instruments arguments to data constructors and user-defined functions,
and skips instrumenting arguments to primtives, literals, and
identifiers. Additionally, a function defined via a `define/prim` form is
treated as a primtive function. NOTE: the values produced by non-instrumented
expressions are not counted when computing the laziness potential weight value,
so there may be some tradeoff between performance and accuracy of the
profiler's suggestions.


# Some descriptions for example files:

`nqueens-for-profiling.rkt`: get profiling results for nqueens

`nqueens-eager.rkt`: nqueens example with no laziness -- slow
`nqueens-streams.rkt`: nqueens example with lists converted to streams -- slower
`nqueens-streams+lazy-fold.rkt`: nqueens with streams + lazy foldr -- faster
`nqueens-using-profiler-results.rkt`: nqueens with only lazy filter and fold -- fastest