package traittable;

abstract class Eval {
	Eval(int classId) {
		v.classId(this, classId);
	}
	abstract int eval(E self);

	static int _eval(E self) {
		return ((Eval)v.arr[self.id]).eval(self);
	}
	static VTable v = new VTable();

	static {
		new Eval(EAdd.classId) {
			int eval(E self) {
				EAdd e = (EAdd)self;
				return Eval._eval(e.a)+Eval._eval(e.b);
			}
		};
		new Eval(EInt.classId) {
			int eval(E self) {
				EInt e = (EInt)self;
				return e.x;
			}
		};
	}

	public static void main(String[] argv) {
		System.out.println(Eval._eval(new EAdd(new EInt(1), new EInt(2))));
	}
}