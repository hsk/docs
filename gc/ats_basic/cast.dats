// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib cast.dats -o cast; ./cast

#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload STDLIB = "libc/SATS/stdlib.sats"
#define BUFSZ 1024

typedef charptr = $extype"char*"

fun fsed (inp: string) : void = let
  var buf = @[byte][BUFSZ]()
  val CAT = "cat"
  val SED = "sed"
  val inp = $UN.cast{charptr}(inp)
  val bufp = $UN.cast{charptr}(addr@buf)
  val script = "\ns/'><\\/BODY'/'><SCRIPT SRC=\".\\/assets\\/ATS2TUTORIAL-BOOK.js\"><\\/SCRIPT><\\/BODY'/\n"
  val _ = $extfcall(int, "snprintf", bufp, BUFSZ, "%s %s | %s %s > o_%s", CAT, inp, SED, script, inp)
  val () = fprintln! (stdout_ref, "fsed: command = ", $UN.cast{string}(bufp))
in
  ignoret($STDLIB.system($UN.cast{string}(bufp)))
end

implement main0{n} (argc, argv) = let
  val argv1 = ptr_succ<string> ($UN.castvwtp1{ptr}(argv))
  val () = println!("argv1", argv1)
  implement(env) array_foreach$fwork<string><env> (x, env) = fsed(x)
in
  ignoret(arrayref_foreach($UN.castvwtp1{arrayref(string,n-1)}(argv1), i2sz(argc-1)))
end
