# Since the makefile is not in the same directory as the source files, we need to specify the path to the source files here.
src = ../../src/
test = ../../test/

##############################
$(src)1_scanner.ml : $(src)1_scanner.mll
	cd $(src) && ocamllex 1_scanner.mll
	cd $(src) && ocamlbuild 1_scanner.native
