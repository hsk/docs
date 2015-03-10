.bytecode 52.0
.source Test006.java

.class  tests/Test006
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 1
	.line 5
		0: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		3: 	aload_0
		4: 	iconst_0
		5: 	aaload
		6: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 6
		9: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		12: 	iconst_1
		13: 	invokevirtual java/io/PrintStream/println(I)V
	.line 7
		16: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		19: 	iconst_2
		20: 	invokevirtual java/io/PrintStream/println(I)V
	.line 8
		23: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		26: 	iconst_3
		27: 	invokevirtual java/io/PrintStream/println(I)V
	.line 9
		30: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		33: 	iconst_4
		34: 	invokevirtual java/io/PrintStream/println(I)V
	.line 10
		37: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		40: 	iconst_5
		41: 	invokevirtual java/io/PrintStream/println(I)V
	.line 11
		44: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		47: 	bipush 6
		49: 	invokevirtual java/io/PrintStream/println(I)V
	.line 12
		52: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

