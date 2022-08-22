all: miniml tests

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte


clean:
	rm -rf _build *.byte