.bytecode 49.0
.source Test008.java

.class  tests/Test008
.super java.lang.Object


.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 2
	.line 4
		0: 	bipush 10
		2: 	newarray int
		4: 	astore_1
	.line 5
		5: 	aload_1
		6: 	iconst_0
		7: 	iconst_0
		8: 	iastore
	.line 6
		9: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		12: 	aload_1
		13: 	iconst_0
		14: 	iaload
		15: 	invokevirtual java/io/PrintStream/println(I)V
	.line 7
		18: 	return
.end method

