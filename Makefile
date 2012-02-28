all: display.cmx
	ocamlfind opt -thread -package str,unix,threads,graphics -linkpkg display.cmx mlirc.ml -o mlirc.exe

display.cmx:
	ocamlfind opt -c -package graphics display.ml

