// atsfunc.dats
#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"

extern fun fib (n: uint): uint = "ext#ats_fib"
implement fib (n: uint): uint = case+ n of
  | 0U => 0U
  | 1U => 1U
  | _  => fib(n - 2U) + fib(n - 1U)

