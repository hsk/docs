package tests;
class Test_b {
  public static void main(String[] argv) {
  	byte[] ba = new byte[10];
  	ba[0] = (byte)1;
  	System.out.println(ba[0]);

  	boolean b = true;
  	b = b || true;
  	b = b && true;
  }
}
