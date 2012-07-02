all:
	ocamlfind ocamlc -linkpkg -package batteries -o oppl oppl_parse.ml

