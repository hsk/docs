.source Test_switch2.java

.class  tests/Test_switch2
.super java.lang.Object


.method static public main([Ljava/lang/String;)V
	.limit stack 3
		 	iconst_1
		 	lookupswitch 
				1 : 		Hello
				2 :         Goodbye
				default : 	Foo

			nop
			nop
			nop
		 	iconst_1
		 	tableswitch 1
					Hello
					Goodbye
					Goodbye
			default : 	Foo
Hello:
Goodbye:
Foo:
		 	return
.end method
