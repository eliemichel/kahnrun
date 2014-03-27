CMX=vecutils.cmx material.cmx ast.cmx parser.cmx lexer.cmx raycast.cmx main.cmx
GEN=lexer.ml parser.ml parser.mli
BIN=ray
LIBS=
SOURCES=main.ml ast.ml lexer.mll parser.mly raycast.ml

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m
HIGHLIGHT_COLOR=\033[1m

all: $(BIN)
	@echo "$(HIGHLIGHT_COLOR)  ./$(BIN) --help$(NO_COLOR)"
	@./$(BIN) --help

session:
	@echo "Recovering session..."
	@gedit $(SOURCES) 2>/dev/null &


$(BIN): $(CMX)
	@echo "Linking for $(BIN)..."
	@ocamlopt -o $(BIN) $(LIBS) $(CMX)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"

.SUFFIXES: .mli .ml .cmi .cmx .mll .mly

.mli.cmi:
	ocamlopt -c $<

.ml.cmx: 
	ocamlopt -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	menhir -v $<


clean:
	@echo "Cleaning directory..."
	@rm -v -f .depend *.cmi *.o *.automaton *.conflicts *~ $(BIN) $(CMX) $(GEN)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"


.depend: $(GEN)
	rm -f .depend
	ocamldep *.mli *.ml > .depend

dep: all

.PHONY: all clean session dep

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),session)
ifeq ($(MAKECMDGOALS),dep)
include .depend
endif
endif
endif

