#![crate_id(name = "fib")]

extern crate std;
extern crate time;

fn fib(n : int) -> int {
  if n < 2 {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

trait Fib {
	fn fib(&self) -> int;
}

impl Fib for int {
	fn fib(&self) -> int {
		if (self+0 < 2) {
			1
		} else {
			(self-2).fib() + (self-1).fib()
		}
	}
}

fn tim() -> i64 {
	let t = time::get_time();
	(t.sec * 1000) + ((t.nsec / 1000000) as i64)
}
fn main() {
  let start = tim();
  println!("{}", fib(40));
  println!("{}ms", tim()-start);

  let start = tim();
  println!("{}", 40.fib());
  println!("{}ms", tim()-start);
}

