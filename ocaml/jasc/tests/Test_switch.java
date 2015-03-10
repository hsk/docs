package tests;
class Test_switch {
	public static void main(String[] argv) {
		int n = 5;
		int a = 1;
		switch(n) {
			case 1: a = 1; break;
			case 2: a = 1; break;
		}
		
		switch(n) {
			case 1: a = 1; break;
			case 2: a = 1; break;
			case 3: a = 1; break;
		}
		switch(n) {
			case 0: a = 1; break;
			case 1: a = 1; break;
			case 2: a = 1; break;
		}
	}
}

