class Any {
	Any(int id) {
		this.id = id;
	}
	int id;

	static int counter = -1;
	static int genId() {
		counter ++;
		return counter;
	}
}
