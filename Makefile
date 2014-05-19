OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

example:
	$(OCAMLBUILD) master.$(TARGET)
	$(OCAMLBUILD) example.$(TARGET)

handcut:
	$(OCAMLBUILD) master.$(TARGET)
	$(OCAMLBUILD) handcut_slave1.$(TARGET)
	$(OCAMLBUILD) handcut_slave2.$(TARGET)
	$(OCAMLBUILD) handcut_slave3.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
