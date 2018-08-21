include language.mk

OCAMLC_FLAGS=-g
BUILD= \
	ocamlbuild \
		-r \
		-I src \
		-I src/language/$(LANGUAGE) \
		-use-ocamlfind \
		-use-menhir \
		-yaccflag '--explain'

.PHONY: hatch
hatch: make_output_dirs
	$(BUILD) hatch.byte

hatch.byte: hatch

.PHONY: tests
tests: make_output_dirs
	$(BUILD) tests.byte

tests.byte: tests

.PHONY: make_output_dirs
make_output_dirs:
	bash ./make_output_dirs.sh

.PHONY: clean
clean:
	rm -rf *.byte *.native _build/ output/ logs/
