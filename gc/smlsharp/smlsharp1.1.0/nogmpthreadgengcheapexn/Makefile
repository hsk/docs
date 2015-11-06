main1:
	gcc main1.c
	gcc main1.c error.c
	gcc main1.c obstack.c error.c
	gcc main1.c splay.c
	gcc -m32 main1.c	error.c obstack.c splay.c control.c objspace.c object.c 

	gcc -m32 \
		error.c \
		obstack.c \
		splay.c \
		control.c \
		objspace.c \
		object.c \
		main1.c

	gcc -m32 \
		error.c \
		obstack.c \
		splay.c \
		control.c \
		objspace.c \
		object.c \
		prim.c \
		main1.c

all:
	gcc -m32 \
		error.c \
		obstack.c \
		splay.c \
		control.c \
		objspace.c \
		object.c \
		main.c \
		smlmain.c
		#prim.c \

	./a.out

all2:
	gcc -m32 \
		error.c \
		obstack.c \
		splay.c \
		control.c \
		objspace.c \
		object.c \
		main.c \
		smlmain2.c
		#prim.c \

	./a.out
