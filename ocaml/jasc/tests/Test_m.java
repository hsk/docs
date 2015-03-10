package tests;
class Test_m {
	public static void main(String[] argv) {
		int[][] aa = new int[10][10];
		aa[0] = new int[10];
		int[] a = aa[0];
		
		synchronized(a) {
			System.out.println("sync");
		}
	}
}
