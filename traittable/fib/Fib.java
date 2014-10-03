class Fib {
	int n;
	Fib(int n) {
		this.n = n;
	}

	int fib() {
		if (n < 2) return 1;
		return new Fib(n - 1).fib() + new Fib(n - 2).fib();
	}

	static int f(int n) {
		if (n < 2) return 1;
		return f(n - 1) + f(n - 2);
	}

	public static void main(String[] argv) {
		int n = 40;
		long start;

		start = System.currentTimeMillis();
		System.out.println(f(n));
		System.out.println(System.currentTimeMillis() - start + "ms");

		start = System.currentTimeMillis();
		System.out.println(new Fib(n).fib());
		System.out.println(System.currentTimeMillis() - start + "ms");

		start = System.currentTimeMillis();
		System.out.println(Fib2._fib(new EInt(n)));
		System.out.println(System.currentTimeMillis() - start + "ms");
	}
}

abstract class Fib2 {
	Fib2(int classId) {
		v.classId(this, classId);
	}
	abstract int fib(Any self);

	static int _fib(Any self) {
		return ((Fib2)v.arr[self.id]).fib(self);
	}
	static VTable v = new VTable();
	static Fib2[] arr;
	static {
		new Fib2(EInt.classId) {
			int fib(Any self) {
				int x = ((EInt)self).x;
				if (x < 2) return 1;
				return Fib2._fib(new EInt(x-1))+Fib2._fib(new EInt(x-2));
			}
		};
	}

	public static void main(String[] argv) {
	}
}
