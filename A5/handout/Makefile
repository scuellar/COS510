all : dminml

dminml : *.hs Main.hs Parser.hs
	ghc -o dminml Main.hs

test : dminml
	./dminml fact_typed.dml
	./dminml simple_untyped.dml
	./dminml fact_untyped.dml
	./dminml just_try.dml
	./dminml just_try2.dml

Parser.hs : Parser.y
	happy Parser.y

clean : 
	rm -f *~ *.hi *.o
	rm -f Parser.hs Parser.info
