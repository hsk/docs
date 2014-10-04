package traittable;

class EAdd extends E {
	E a;
	E b;

	EAdd(E a, E b) {
		super(classId);
		this.a = a;
		this.b = b;
	}
	static int classId = genId();
};
