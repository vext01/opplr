PPL_OLIBS=-I /usr/local/lib/ppl
GMP_OLIBS=-I /usr/local/lib/ocaml/gmp
PPL_CXXLIBS=-cclib /usr/local/lib/libppl.a

all:
	ocamlfind ocamlc ${PPL_OLIBS} ${GMP_OLIBS} -linkpkg -package batteries ${PPL_CXXLIBS} -o oppl gmp.cma ppl_ocaml.cma oppl.ml

top:
	ocamlfind ocamlmktop ${PPL_OLIBS} ${GMP_OLIBS} -linkpkg -package batteries -o oppl_top ${PPL_CXXLIBS} gmp.cma ppl_ocaml.cma
	./oppl_top ${PPL_OLIBS} ppl_ocaml.cma

