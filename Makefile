# not working at the moment

# variables

src = src/

test = test/

# all : $(test)example.out

##############################


# $(src)4_graphsql_eval : $(src)2_parser.cmo $(src)1_scanner.cmo $(src)4_graphsql_eval
# 	ocamlc -w A -o $(src)4_graphsql_eval $^

$(src)%.cmo : $(src)%.ml
	cd $(src) \ ocamlc -w A -c $<

$(src)%.cmi : $(src)%.mli
	cd $(src) \ ocamlc -w A -c $<

$(src)1_scanner.ml : $(src)1_scanner.mll
	ocamllex $^

# $(src)2_parser.ml $(src)2_parser.mli : $(src)2_parser.mly
# 	cd $(src) \ ocamlyacc $^

# $(test)example.out : $(src)4_graphsql_eval $(test)example.tb
# 	./$(src)4_graphsql_eval < $(test)example.tb > $(test)example.out

# Depedencies from ocamldep
# $(src)4_graphsql_eval.cmo : $(src)1_scanner.cmo $(src)2_parser.cmi $(src)3_ast.cmi
# $(src)4_graphsql_eval.cmx : $(src)1_scanner.cmx $(src)2_parser.cmx $(src)3_ast.cmi
# $(src)2_parser.cmo : $(src)3_ast.cmi $(src)2_parser.cmi
# $(src)2_parser.cmx : $(src)3_ast.cmi $(src)2_parser.cmi
$(src)1_scanner.cmo : $(src)2_parser.cmi
$(src)1_scanner.cmx : $(src)2_parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf $(src)*.cmi $(src)*.cmo $(src)2_parser.ml $(src)2_parser.mli $(src)1_scanner.ml $(test)example.out $(src)4_graphsql_eval