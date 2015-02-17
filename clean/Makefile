default: hello1 run
install:
	wget http://clean.cs.ru.nl/download/Clean24/macosx/clean2.4.zip
	unzip clean2.4.zip
	cd clean; make; make install
run:
	./hello1

hello1: hello1.icl
	clm -nt -nr hello1 -o hello1

