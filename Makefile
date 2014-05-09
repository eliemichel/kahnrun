OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

example:
	$(OCAMLBUILD) example.$(TARGET)
	$(OCAMLBUILD) example_slave.$(TARGET)
	$(OCAMLBUILD) example_master.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
