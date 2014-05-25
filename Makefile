OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

all:
	rm -f /tmp/kahn*
	$(OCAMLBUILD) master.$(TARGET)
	$(OCAMLBUILD) example.$(TARGET)
	$(OCAMLBUILD) testclient.$(TARGET)

clean:
	$(OCAMLBUILD) -clean
	rm -f *~
