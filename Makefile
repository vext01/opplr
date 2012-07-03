PPL_LIBS=-I /usr/local/lib/ppl
GMP_LIBS=-I /usr/local/lib/ocaml/gmp

all:
	ocamlfind ocamlc ${PPL_LIBS} ${GMP_LIBS} -linkpkg -package batteries -o oppl gmp.cma ppl_ocaml.cma oppl.ml

