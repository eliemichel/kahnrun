BIN=ray
SOURCES=main.ml utils.ml vecutils.ml material.ml ast.ml lexer.mll parser.mly raycast.ml typer.ml kahn.ml

OCAMLBUILD=ocamlbuild \
		-use-menhir \
		-classic-display \
		-tags annot,debug,thread \
		-libs unix

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

test:
	$(OCAMLBUILD) test.native
	./test.native


$(BIN): $(SOURCES)
	@echo "Building $(BIN)..."
	$(OCAMLBUILD) main.native
	ln -s _build/main.native $(BIN)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"


clean:
	@echo "Cleaning directory..."
	@rm -vrf _build $(BIN) main.native
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"


.PHONY: all clean session



