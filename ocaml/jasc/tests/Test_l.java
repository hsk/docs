package tests;
class Test_l {
	public static void main(String[] argv) {
		long l = 1;
		float f = (float)l;
		double d = (double)l;
		int i1 = (int)l;

		long[] a = new long[1];
		l = a[0];
		a[0] = l;

		boolean b1 = l < 1;
		b1 = l > 1;

		l = l + 2;
		l = l / 3;
		l = l * 4;
		l = - l;
		l = l % 2;
		l = l - 2;
		l = l | 1234;
		l = l & 1234;
		l = l ^ 1234;
		l = l << 2;
		l = l >> 2;
		l = l >>> 2;


		byte b = (byte)l;
		char c = (char)l;
		short s = (short)l;
		int i = (int)l;


	}

	public static int a() {
		return 1;
	}
}
