
OCAMLBUILD=ocamlbuild -use-ocamlfind
OPTIONS=-package cconv

NAME=ppx_deriving_cconv
TARGETS=$(addprefix $(NAME), .cma .a .cmxa .cmxs)

all:
	$(OCAMLBUILD) $(OPTIONS) $(TARGETS)

clean:
	$(OCAMLBUILD) $(OPTIONS) -clean

install: all
	ocamlfind install $(NAME) META $(addprefix _build/src/,$(TARGETS))

uninstall:
	ocamlfind remove $(NAME)

.PHONY: all clean install uninstall
