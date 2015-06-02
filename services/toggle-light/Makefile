NAME=toggle_light
OBJS:=iothub_core.cmo $(NAME).cmo
JSOO_PACKAGES:=yojson deriving-yojson deriving-yojson.syntax deriving.syntax.std
OCAMLC=ocamlfind ocamlc $(addprefix -package ,js_of_ocaml js_of_ocaml.syntax $(JSOO_PACKAGES)) -syntax camlp4o

all: toggle_light.js clean

%.js: %.byte
	js_of_ocaml $<

iothub_core.cmo: ../../core/iothub_core.ml
	$(OCAMLC) -c $< -o $@

%.cmo: %.ml
	$(OCAMLC) -c $<

%.byte: $(OBJS)
	$(OCAMLC) -linkpkg -o $@ $^
		
.PHONY: clean

clean:
	@rm -rf *.byte *.cmi *.cmo ../../core/*.cmi ../../core/*.cmo 

distclean:
	@rm -rf *.byte *.cmi *.cmo *.js

