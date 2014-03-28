BIN=ray
SOURCES=main.ml utils.ml vecutils.ml material.ml ast.ml lexer.mll parser.mly raycast.ml typer.ml

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


$(BIN): $(SOURCES)
	@echo "Building $(BIN)..."
	ocamlbuild -use-menhir main.native
	cp main.native $(BIN)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"


clean:
	@echo "Cleaning directory..."
	@rm -vrf _build $(BIN) main.native
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"


.PHONY: all clean session



