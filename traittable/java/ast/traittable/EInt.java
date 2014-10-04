package traittable;

class EInt extends E {
	int x;

	EInt(int x) {
		super(classId);
		this.x = x;
	}
	static int classId = genId();
};
