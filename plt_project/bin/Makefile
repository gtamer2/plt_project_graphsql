# default target that 'make' will attempt to build if no target specified
# all : calc_test.out graph_init.out
# all : graph_init.out graph_init2.out semant_test.out semant_test2.out  # NOTE -- this is oour woorking version
# all : calc_test.out

# WARN - CHATGPT
# Define the default target
all: microc.native


# WARN - CHATGPT
##############################
semant_eval : ast.cmo sast.cmo parser.cmo scanner.cmo semantic_checker.cmo semant_eval.cmo
	ocamlc -w A -o semant_eval ast.cmo sast.cmo parser.cmo scanner.cmo semantic_checker.cmo semant_eval.cmo

graphsql : ast.cmo parser.cmo scanner.cmo graphsql.cmo
	ocamlc -w A -o graphsql ast.cmo parser.cmo scanner.cmo graphsql.cmo

# Explicit rule for ast.cmo to ensure it's compiled before its dependents
ast.cmo: ast.ml
	ocamlc -w A -c ast.ml

# Pattern Rules for Compilation
# build object & interface files from Ocaml source & interface files
%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^


# WARNING --- CHAT GPT CODE
	graphsql.native : ast.cmo parser.cmo scanner.cmo graphsql.cmo
		ocamlfind ocamlopt -o graphsql.native -linkpkg -package $PACKAGES$ ast.cmo parser.cmo scanner.cmo graphsql.cmo
# WARNING END --- CHAT GPT CODE


# Define the rule for building microc.native
microc.native:
	ocamlbuild -pkgs llvm graphsql.native

# calc_test.out : graphsql calc_test.tb
# 	./graphsql < calc_test.tb > calc_test.out

graph_init.out : graphsql graph_init.tb
	./graphsql < graph_init.tb > graph_init.out

graph_init2.out : graphsql graph_init2.tb
	./graphsql < graph_init2.tb > graph_init2.out

semant_test.out : semant_eval semant_test.tb
	./semant_eval < semant_test.tb > semant_test.out

semant_test2.out : semant_eval semant_test2.tb
	./semant_eval < semant_test2.tb > semant_test2.out

# Depedencies from ocamldep
graphsql.cmo : scanner.cmo parser.cmi ast.cmo
graphsql.cmx : scanner.cmx parser.cmx ast.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmo parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

# Dependencies for semantic_checker
semant_eval.cmo : scanner.cmo parser.cmi ast.cmo sast.cmo semantic_checker.cmo
semant_eval.cmx : scanner.cmx parser.cmx ast.cmo sast.cmo semantic_checker.cmx
semantic_checker.cmo : ast.cmo sast.cmo
semantic_checker.cmx : ast.cmo sast.cmo



##############################

# .PHONY tells make that clean is not a file name, but a command to be executed
.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml calc_test.out graphsql semant_eval


##############################
# Makefile is used to automate the build process, order of rules does not determine execution order (det. by dependencies)

# Overview of the Process
# 1. Generate scanner and parser code (.ml, .mli) from their definitions (.mly, .mll) 
# 2. Compile Ocaml Sources to Object Files (.cmo, .cmi)
# 3. Link Object Files to Create Executable 'calc'
# 4. Execute the 'calc' executable

# .ml files: These are OCaml source code files containing definitions of functions and types written in OCaml.
# .mli files: These are interface files for .ml files. They explicitly declare the signatures of functions and 
# 	types that are exposed to other parts of the program, acting as a contract that the corresponding .ml file must fulfill.