// string_foreach.dats
// patscc string_foreach.dats -DATS_MEMALLOC_LIBC -latslib -o string_foreach
staload "libats/ML/SATS/string.sats"

implement main0 () = { 
  val str = "abcdefg" 
  val ()  = string_foreach(str, lam(c) => print_char(c)) 
  val ()  = print_newline()
}
