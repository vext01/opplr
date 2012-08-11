PPL_OLIBS=-I /usr/local/lib/ocaml/site-lib/ppl
GMP_OLIBS=-I /usr/local/lib/ocaml/gmp
PPL_CXXLIBS=-cclib -lppl
OCAML_PACKAGES=-package batteries \
	       -package batteries.syntax

.PHONY: all
all: opplr.opt opplr

# Native code
opplr.opt: opplr.ml
	ocamlfind ocamlopt ${PPL_OLIBS} ${GMP_OLIBS} -syntax camlp4o \
		-linkpkg ${OCAML_PACKAGES} ${PPL_CXXLIBS} -o opplr.opt \
		gmp.cmxa ppl_ocaml.cmxa opplr.ml

# Bytecode
opplr: opplr.ml
	ocamlfind ocamlc ${PPL_OLIBS} ${GMP_OLIBS} -syntax camlp4o \
		-linkpkg ${OCAML_PACKAGES} ${PPL_CXXLIBS} -o opplr \
		gmp.cma ppl_ocaml.cma opplr.ml

# An Ocaml toplevel with PPL integrated
.PHONY: ppl-top
ppl-top:
	ocamlfind ocamlmktop ${PPL_OLIBS} ${GMP_OLIBS} \
		-linkpkg ${OCAML_PACKAGES} -o ppl_top ${PPL_CXXLIBS} \
		gmp.cma ppl_ocaml.cma ${PPL_OLIBS} ppl_ocaml.cma
.PHONY: clean
clean:
	rm -f opplr
	rm -f opplr.opt
	rm -f ppl-top
