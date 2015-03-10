package tests;
class Test_d {
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

		boolean b = d < 1.0;
		b = d > 1.0;

		d = d + 2.0;
		d = d / 2.0;
		d = d * 2.0;
		d = - d;
		d = d % 2.0;
		d = d - 2.0;

	}
	public static double a() {
		double d = 1.0;
		if(d < 10) return d+1.2;
		return d+1.1;
	}
}
