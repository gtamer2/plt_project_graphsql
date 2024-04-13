
src = ../../src/

test = ../../test/

$(src)%.cmo : $(src)%.ml
	cd $(src) \ ocamlc -w A -c $<

$(src)%.cmi : $(src)%.mli
	cd $(src) \ ocamlc -w A -c $<

$(src)1_scanner.ml : $(src)1_scanner.mll
	ocamllex $^

$(src)1_scanner.cmo : $(src)2_parser.cmi
$(src)1_scanner.cmx : $(src)2_parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf $(src)*.cmi $(src)*.cmo $(src)2_parser.ml $(src)2_parser.mli $(src)1_scanner.ml $(test)example.out $(src)4_graphsql_eval