all: 
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native gen

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
