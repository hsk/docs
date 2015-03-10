package tests;
class Test010 {
  public static void main(String[] argv) {
    try {
      int a = 0;
      int a2 = 1;
      int a3 = 2;
      int a4 = 3;
      int a5 = 4;
      int a6 = 5;
      int a7 = 7;
      if(a==1 && a2!=0 && a3>0 && a4>=0 && a5<0 && a6 <= 0 && a7 == 1 && a7 != 1 )
        throw new Exception("error");
    } catch (Exception e) {
    }
  }
}
