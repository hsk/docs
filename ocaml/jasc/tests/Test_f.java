package tests;
class Test_f {
	public static void main(String[] argv) {
		double d = 1.0;
		float f = (float)d;
		int i = (int)d;
		long l = (long)d;

		i = (int)f;
		l = (long)f;
		d = (float)f;

		f = (float)i;
		d = (double)i;
		l = (long)i;

		f = (float)l;
		d = (double)l;
		i = (int)l;
		float[] a = new float[1];
		f = a[0];
		a[0] = f;
		boolean b = f < 1.0f;
		b = f > 1.0f;

		f = f + 2.0f;
		f = f / 3.0f;
		f = f * 4.0f;
		f = - f;
		f = f % 2.0f;
		f = f - 2.0f;

	}
	public static float a() {
		return 1.1f;
	}
}
