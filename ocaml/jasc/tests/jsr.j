.source jsr.j

.class  tests/jsr
.super java.lang.Object

.method static public main([Ljava/lang/String;)V
	.limit stack 0
	.limit locals 1
	invokestatic tests/jsr/t2()V
	return
.end method

.method static t2()V
	.limit stack 5
	.limit locals 2
    ldc "sub"
	jsr SUB
    ldc "sub2"
	jsr SUB
	return
SUB :
	astore_0
	astore_1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload_1
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	ret 0

.end method
