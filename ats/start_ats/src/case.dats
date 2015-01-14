// case.dats

#include "share/atspre_staload.hats"

fn f1(v:int):int = case v of 1 => 2 | _ => 3

fn f2(v:int):int =
	case v of 
	| 1 => 2
	| _ => 3

fn f3(v:int):int =
	begin case v of 
		| 1 => 2
		| _ => 3
	end

implement main0() = begin
  println!(f1(3));
  println!(f2(3));
  println!(f3(3));
  ()
end
