all: opt

opt:
	ocamlbuild -use-ocamlfind -I src src/index.native
	cp index.native index

test:
	ocamlbuild -use-ocamlfind -I src src/test.top

clean:
	ocamlbuild -clean