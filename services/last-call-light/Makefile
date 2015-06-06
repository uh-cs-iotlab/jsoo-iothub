NAME=last_call_light
OBJS:=iothub_core.cmo iothub_data_model.cmo $(NAME).cmo
JSOO_PACKAGES:=yojson deriving-yojson deriving-yojson.syntax deriving.syntax.std
OCAMLC=ocamlfind ocamlc $(addprefix -package ,js_of_ocaml js_of_ocaml.syntax $(JSOO_PACKAGES)) -syntax camlp4o

all: $(NAME).js clean

%.js: %.byte
	js_of_ocaml --opt 3 $<

iothub_core.cmo: ../../core/iothub_core.ml
	$(OCAMLC) -c $< -o $@
	
iothub_data_model.cmo: ../../core/iothub_data_model.ml
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

