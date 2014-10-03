class VTable {
	Object[] arr = new Object[0];
	void classId(Object t, int classId) {
		if (arr.length <= classId) {
			Object[] arr2 = new Object[arr.length + 1000];
			System.arraycopy(arr, 0, arr2, 0, arr.length);
			arr = arr2;
		}
		arr[classId] = t;
	}
}
