class a {
  static int add(int a, int b) { int c = a + b; return c + 1; }

  static int call() {
  	
  	int a = 1;
  	int b = 2;
  	int c = add(a,a+b);
  	return c;
  }

  static int[] malloc() {
    int[] a;

    a = new int[10];
    return a;
  }
}
