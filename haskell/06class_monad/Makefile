all: ls ok

run: ls ok
	./ls
	./ok

ls: ls.hs
	ghc -o ls ls.hs

ok: ok.hs
	ghc -o ok ok.hs
md: ok.hs
	./hmddoc ok.hs ls.hs

doctest:
	doctest ok.hs
	doctest ls.hs

clean:
	rm -rf *.hi ok ls
