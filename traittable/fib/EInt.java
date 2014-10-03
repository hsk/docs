class EInt extends Any {
	int x;

	EInt(int x) {
		super(classId);
		this.x = x;
	}
	static int classId = genId();
};
