main1:
	gcc main1.c
	gcc main1.c error.c
	gcc main1.c obstack.c error.c
	gcc main1.c exn.c error.c
	gcc main1.c splay.c
	gcc -m32 main1.c	error.c exn.c obstack.c splay.c heap_malloc.c control.c objspace.c object.c -lgmp 

	gcc -m32 \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_malloc.c \
		control.c \
		objspace.c \
		object.c \
		main1.c

	gcc -m32 \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_malloc.c \
		control.c \
		objspace.c \
		object.c \
		init.c \
		main1.c

	gcc -m32 \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_malloc.c \
		control.c \
		objspace.c \
		object.c \
		init.c \
		prim.c \
		main1.c

all:
	gcc -m32 \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_malloc.c \
		control.c \
		objspace.c \
		object.c \
		init.c \
		main.c \
		smlmain.c\
		#prim.c \

	./a.out

all2:
	gcc -m32 \
		error.c \
		exn.c \
		obstack.c \
		splay.c \
		heap_malloc.c \
		control.c \
		objspace.c \
		object.c \
		init.c \
		main.c \
		smlmain2.c\
		#prim.c\

	./a.out

