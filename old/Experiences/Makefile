all: 
	ocamlbuild -yaccflag -v -lib unix fibo.native; ocamlbuild -yaccflag -v -lib unix fibo_memo.native; ln -s _build/fibo.native .

byte: 
	ocamlbuild -yaccflag -v fibo.byte; ocamlbuild -yaccflag -v fibo_memo.byte 

clean: 
	ocamlbuild -clean
