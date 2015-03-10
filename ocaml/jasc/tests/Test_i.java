package tests;
class Test_i {
	public static void main(String[] argv) {
		int i = 1;
		float f = (float)i;
		double d = (double)i;
		long l = (long)i;

		int[] a = new int[1];
		i = a[0];
		a[0] = i;

		boolean b1 = i < 1;
		b1 = i > 1;

		i = i + 2;
		i = i / 3;
		i = i * 4;
		i = - i;
		Object obj = null;
		b1 = obj instanceof String;
		i = i % 2;
		i = i - 2;
		i = i | 1234;
		i = i & 1234;
		i = i ^ 1234;
		i = i << 2;
		i = i >> 2;
		i = i >>> 2;


		byte b = (byte)i;
		char c = (char)i;
		short s = (short)i;

		i++;
		i--;
		i+=100000;
		i-=100000;

	}

	public static int a() {
		return 1;
	}
}
