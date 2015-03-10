.bytecode 49.0
.source Test_i.java

.class  tests/Test_i
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 3
	.limit locals 13
	.line 4
		0: 	iconst_1
		1: 	istore_1
	.line 5
		2: 	iload_1
		3: 	i2f
		4: 	fstore_2
	.line 6
		5: 	iload_1
		6: 	i2d
		7: 	dstore_3
	.line 7
		8: 	iload_1
		9: 	i2l
		10: 	lstore 5
	.line 9
		12: 	iconst_1
		13: 	newarray int
		15: 	astore 7
	.line 10
		17: 	aload 7
		19: 	iconst_0
		20: 	iaload
		21: 	istore_1
	.line 11
		22: 	aload 7
		24: 	iconst_0
		25: 	iload_1
		26: 	iastore
	.line 13
		27: 	iload_1
		28: 	iconst_1
		29: 	if_icmpge 36
		32: 	iconst_1
		33: 	goto 37
		36: 	iconst_0
		37: 	istore 8
	.line 14
		39: 	iload_1
		40: 	iconst_1
		41: 	if_icmple 48
		44: 	iconst_1
		45: 	goto 49
		48: 	iconst_0
		49: 	istore 8
	.line 16
		51: 	iload_1
		52: 	iconst_2
		53: 	iadd
		54: 	istore_1
	.line 17
		55: 	iload_1
		56: 	iconst_3
		57: 	idiv
		58: 	istore_1
	.line 18
		59: 	iload_1
		60: 	iconst_4
		61: 	imult
		62: 	istore_1
	.line 19
		63: 	iload_1
		64: 	ineg
		65: 	istore_1
	.line 20
		66: 	aconst_null
		67: 	astore 9
	.line 21
		69: 	aload 9
		71: 	instanceof java/lang/String
		74: 	istore 8
	.line 22
		76: 	iload_1
		77: 	iconst_2
		78: 	irem
		79: 	istore_1
	.line 23
		80: 	iload_1
		81: 	iconst_2
		82: 	isub
		83: 	istore_1
	.line 24
		84: 	iload_1
		85: 	sipush 1234
		88: 	ior
		89: 	istore_1
	.line 25
		90: 	iload_1
		91: 	sipush 1234
		94: 	iand
		95: 	istore_1
	.line 26
		96: 	iload_1
		97: 	sipush 1234
		100: 	ixor
		101: 	istore_1
	.line 27
		102: 	iload_1
		103: 	iconst_2
		104: 	ishl
		105: 	istore_1
	.line 28
		106: 	iload_1
		107: 	iconst_2
		108: 	ishr
		109: 	istore_1
	.line 29
		110: 	iload_1
		111: 	iconst_2
		112: 	iushr
		113: 	istore_1
	.line 32
		114: 	iload_1
		115: 	i2b
		116: 	istore 10
	.line 33
		118: 	iload_1
		119: 	i2c
		120: 	istore 11
	.line 34
		122: 	iload_1
		123: 	i2s
		124: 	istore 12
	.line 36
		126: 	iinc 1 1
	.line 37
		129: 	iinc 1 -1
	.line 38
		132: 	iload_1
		133: 	ldc 100000
		135: 	iadd
		136: 	istore_1
	.line 39
		137: 	iload_1
		138: 	ldc 100000
		140: 	isub
		141: 	istore_1
	.line 41
		142: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public a()I
	.limit stack 1
	.limit locals 0
	.line 44
		0: 	iconst_1
		1: 	ireturn
.end method

