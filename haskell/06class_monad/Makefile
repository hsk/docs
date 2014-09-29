all: ls ok

run: ls ok
	./ls
	./ok

ls: ls.hs
	ghc -o ls ls.hs

ok: ok.hs
	ghc -o ok ok.hs

clean:
	rm -rf *.hi ok ls