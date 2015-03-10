package tests;

class Test_switch3 {
  public static void main(String[] argv) {
    a:{
    switch(1) {
      case 1: break a;
    }
    switch(1) {
      case 0: break a;
      case 1: break a;
      case 2: break a;
      default: break a;
    }}
  }
}
