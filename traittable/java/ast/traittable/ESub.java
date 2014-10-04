package traittable;

class ESub extends E {
	E a;
	E b;

	ESub(E a, E b) {
		super(classId);
		this.a = a;
		this.b = b;
	}
	static int classId = genId();
	static {
		new Eval(classId) {
			int eval(E self) {
				ESub e = (ESub)self;
				return _eval(e.a)-_eval(e.b);
			}
		};
		new ToString(classId) {
			String toString(Any self) {
				ESub e = (ESub)self;
				return "ESub("+_toString(e.a)+", "+_toString(e.b)+")";
			}
		};
	}

	static public void main(String[] argv) {
		E e = new ESub(new EInt(1), new EInt(2));
		System.out.println("e="+ToString._toString(e));
		System.out.println(Eval._eval(e));
	}
};
