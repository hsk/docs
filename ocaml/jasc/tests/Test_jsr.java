package tests;
class Test_jsr {
  
  static void t() {
    try {
        throw new Exception ("eee");
    } catch (Exception e) {
        System.out.println("e1");
    } finally {
        System.out.println("e2");
    }
  }
  public static void main(String[] argv) {
    t();
  }
}
