.bytecode 49.0
.source Test_d.java

.class  tests/Test_d
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 4
	.limit locals 8
	.line 4
		0: 	dconst_1
		1: 	dstore_1
	.line 5
		2: 	dload_1
		3: 	d2f
		4: 	fstore_3
	.line 6
		5: 	dload_1
		6: 	d2i
		7: 	istore 4
	.line 7
		9: 	dload_1
		10: 	d2l
		11: 	lstore 5
	.line 9
		13: 	fload 3
		14: 	f2i
		15: 	istore 4
	.line 10
		17: 	fload 3
		18: 	f2l
		19: 	lstore 5
	.line 11
		21: 	fload 3
		22: 	f2d
		23: 	dstore_1
	.line 13
		24: 	iload 4
		26: 	i2f
		27: 	fstore_3
	.line 14
		28: 	iload 4
		30: 	i2d
		31: 	dstore_1
	.line 15
		32: 	iload 4
		34: 	i2l
		35: 	lstore 5
	.line 17
		37: 	lload 5
		39: 	l2f
		40: 	fstore_3
	.line 18
		41: 	lload 5
		43: 	l2d
		44: 	dstore_1
	.line 19
		45: 	lload 5
		47: 	l2i
		48: 	istore 4
	.line 21
		50: 	dload_1
		51: 	dconst_1
		52: 	dcmpg
		53: 	ifge 60
		56: 	iconst_1
		57: 	goto 61
		60: 	iconst_0
		61: 	istore 7
	.line 22
		63: 	dload_1
		64: 	dconst_1
		65: 	dcmpl
		66: 	ifle 73
		69: 	iconst_1
		70: 	goto 74
		73: 	iconst_0
		74: 	istore 7
	.line 24
		76: 	dload_1
		77: 	ldc2_w 2.
		80: 	dadd
		81: 	dstore_1
	.line 25
		82: 	dload_1
		83: 	ldc2_w 2.
		86: 	ddiv
		87: 	dstore_1
	.line 26
		88: 	dload_1
		89: 	ldc2_w 2.
		92: 	dmult
		93: 	dstore_1
	.line 27
		94: 	dload_1
		95: 	dneg
		96: 	dstore_1
	.line 28
		97: 	dload_1
		98: 	ldc2_w 2.
		101: 	drem
		102: 	dstore_1
	.line 29
		103: 	dload_1
		104: 	ldc2_w 2.
		107: 	dsub
		108: 	dstore_1
	.line 31
		109: 	return
.end method

.method  <init>()V
	.limit stack 1
	.limit locals 1
	.line 2
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

.method static public a()D
	.limit stack 4
	.limit locals 2
	.line 33
		0: 	dconst_1
		1: 	dstore_0
	.line 34
		2: 	dload_0
		3: 	ldc2_w 10.
		6: 	dcmpg
		7: 	ifge 16
		10: 	dload_0
		11: 	ldc2_w 1.2
		14: 	dadd
		15: 	dreturn
	.line 35
		16: 	dload_0
		17: 	ldc2_w 1.1
		20: 	dadd
		21: 	dreturn
.end method

