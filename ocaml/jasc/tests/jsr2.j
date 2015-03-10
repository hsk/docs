.source jsr2.j

.class  tests/jsr2
.super java.lang.Object

.method static public main([Ljava/lang/String;)V
	.limit stack 0
	.limit locals 1
	invokestatic tests/jsr2/t2()V
	return
.end method

.method static t2()V
	.limit stack 1
	.limit locals 1
	jsr SUB
	return
SUB :
	astore_0
	ret 0

.end method
