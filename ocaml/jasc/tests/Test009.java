package tests;
class Test009 {
  public static void main(String[] argv) {
    String[] a = new String[100000];
    a[0] = "aaa";
    a[50000] = "bbbb";
    a[1] = null;
    System.out.println(a[0]);
    System.out.println(a[50000]);
    System.out.println(a.length);
  }
}
