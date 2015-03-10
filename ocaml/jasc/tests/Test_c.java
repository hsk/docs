package tests;
class Test_c {
	public static void main(String[] argv) {
		char[] a = new char[10];
		// castore
		a[0] = (char)1;
		// caload
		System.out.println(a[0]);
		Object obj = null;
		// checkcast
		Test_c s = (Test_c)obj;
	}
}
