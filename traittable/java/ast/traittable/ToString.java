package traittable;

abstract class ToString {
	ToString(int classId) {
		v.classId(this, classId);
	}
	abstract String toString(Any self);

	static String _toString(Any self) {
		return ((ToString)v.arr[self.id]).toString(self);
	}
	static VTable v = new VTable();

	static {
		new ToString(EInt.classId) {
			String toString(Any a) {
				EInt p = (EInt)a;
				return "EInt("+p.x+")";
			}
		};
		new ToString(EAdd.classId) {
			String toString(Any a) {
				EAdd p = (EAdd)a;
				return "EAdd("+ToString._toString(p.a)+", "+ToString._toString(p.a)+")";
			}
		};
	}

}