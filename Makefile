OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native


handcut:
	$(OCAMLBUILD) handcut_master.$(TARGET)
	$(OCAMLBUILD) handcut_slave1.$(TARGET)
	$(OCAMLBUILD) handcut_slave2.$(TARGET)
	$(OCAMLBUILD) handcut_slave3.$(TARGET)


example:
	$(OCAMLBUILD) example.$(TARGET)
	$(OCAMLBUILD) example_slave.$(TARGET)
	$(OCAMLBUILD) example_master.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
