// poly.dats

#include "share/atspre_staload.hats"

fun{a:t@ype} add(f: a, g: a) :a = gadd_val(f, g)
fun{a:t@ype} sub(f: a, g: a) :a = gsub_val(f, g)
fun{a:t@ype} mul(f: a, g: a) :a = gmul_val(f, g)
fun{a:t@ype} div(f: a, g: a) :a = gdiv_val(f, g)
fun{a:t@ype} mod(f: a, g: a) :a = gmod_val(f, g)

implement main0() = {
  val () = println!("add(1,2)=",add<int>(1,2));
  val () = println!("sub(1,2)=",sub<int>(1,2));
  val () = println!("mul(1,2)=",mul<int>(1,2));
  val () = println!("div(1,2)=",div<int>(1,2));
  val () = println!("mod(1,2)=",mod<int>(1,2));
}
