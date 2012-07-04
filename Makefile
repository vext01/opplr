PPL_OLIBS=-I /usr/local/lib/ppl
GMP_OLIBS=-I /usr/local/lib/ocaml/gmp
PPL_CXXLIBS=-cclib /usr/local/lib/libppl.a
OCAML_PACKAGES=-package batteries \
	       -package batteries.syntax

all:
	ocamlfind ocamlc ${PPL_OLIBS} ${GMP_OLIBS} -syntax camlp4o -linkpkg ${OCAML_PACKAGES} ${PPL_CXXLIBS} -o oppl gmp.cma ppl_ocaml.cma oppl.ml

top:
	ocamlfind ocamlmktop ${PPL_OLIBS} ${GMP_OLIBS} -linkpkg ${OCAML_PACKAGES} -o oppl_top ${PPL_CXXLIBS} gmp.cma ppl_ocaml.cma
	./oppl_top ${PPL_OLIBS} ppl_ocaml.cma

