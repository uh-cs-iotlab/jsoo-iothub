JSOO_PACKAGES := yojson deriving-yojson deriving-yojson.syntax deriving.syntax.std
LIB_FOLDER := ../../kahvihub/www/lib/

all: dashboard.js clean

%.byte: %.ml
	ocamlfind ocamlc $(addprefix -package ,js_of_ocaml js_of_ocaml.syntax $(JSOO_PACKAGES)) -syntax camlp4o -linkpkg -o $@ $<

%.js: %.byte
	js_of_ocaml $<

.PHONY: clean

install: dashboard.js
	cp $^ $(LIB_FOLDER)

clean:
	@rm -rf *.byte *.cmi *.cmo

distclean:
	@rm -rf *.byte *.cmi *.cmo *.js
