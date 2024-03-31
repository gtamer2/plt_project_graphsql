# not working at the moment

# variables

src_parser = src/parser/

test = test/



all : $(test)example.out

##############################


$(src_parser)4_graphsql_eval : $(src_parser)2_parser.cmo $(src_parser)1_scanner.cmo $(src_parser)4_graphsql_eval
	ocamlc -w A -o $(src_parser)4_graphsql_eval $^

$(src_parser)%.cmo : $(src_parser)%.ml
	cd $(src_parser) \ ocamlc -w A -c $<

$(src_parser)%.cmi : $(src_parser)%.mli
	cd $(src_parser) \ ocamlc -w A -c $<

$(src_parser)1_scanner.ml : $(src_parser)1_scanner.mll
	cd $(src_parser) \ ocamllex $^

$(src_parser)2_parser.ml $(src_parser)2_parser.mli : $(src_parser)2_parser.mly
	cd $(src_parser) \ ocamlyacc $^

$(test)example.out : $(src_parser)4_graphsql_eval $(test)example.tb
	./$(src_parser)4_graphsql_eval < $(test)example.tb > $(test)example.out

# Depedencies from ocamldep
$(src_parser)4_graphsql_eval.cmo : $(src_parser)1_scanner.cmo $(src_parser)2_parser.cmi $(src_parser)3_ast.cmi
$(src_parser)4_graphsql_eval.cmx : $(src_parser)1_scanner.cmx $(src_parser)2_parser.cmx $(src_parser)3_ast.cmi
$(src_parser)2_parser.cmo : $(src_parser)3_ast.cmi $(src_parser)2_parser.cmi
$(src_parser)2_parser.cmx : $(src_parser)3_ast.cmi $(src_parser)2_parser.cmi
$(src_parser)1_scanner.cmo : $(src_parser)2_parser.cmi
$(src_parser)1_scanner.cmx : $(src_parser)2_parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf $(src_parser)*.cmi $(src_parser)*.cmo $(src_parser)2_parser.ml $(src_parser)2_parser.mli $(src_parser)1_scanner.ml $(test)example.out $(src_parser)4_graphsql_eval