package tests;
public class Test_interface2 implements Test_interface {
  public int a = 1;
  public int test() { return 10; }
  public static void main(String[] argv) {
    Test_interface i = (Test_interface)new Test_interface2();
    i.test();
  }
}
