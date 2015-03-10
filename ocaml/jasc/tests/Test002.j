.bytecode 52.0
.source Test002.java

.class  tests/Test002
.super java.lang.Object


.method  <init>()V
	.limit stack 2
	.limit locals 1
	.line 3
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
	.line 4
		4: 	getstatic java/lang/System/out Ljava/io/PrintStream;
		7: 	ldc "test"
		9: 	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	.line 5
		12: 	return
.end method

