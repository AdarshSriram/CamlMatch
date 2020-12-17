MODULES=main client state survey command admin author
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
MAIN=main.byte # start system

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
	rm -rf final_project.zip

graph:
	dot graph.dot -Tpdf -o Usergraph.pdf

test:
	$(OCAMLBUILD) -tag 'debug' 'test_state.byte' && ./'test_state.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_client.byte' && ./'test_client.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_survey.byte' && ./'test_survey.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_admin.byte' && ./'test_admin.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_command.byte' && ./'test_command.byte'

test_state:
	$(OCAMLBUILD) -tag 'debug' 'test_state.byte' && ./'test_state.byte'

test_client:
	$(OCAMLBUILD) -tag 'debug' 'test_client.byte' && ./'test_client.byte'

test_survey:
	$(OCAMLBUILD) -tag 'debug' 'test_survey.byte' && ./'test_survey.byte'

test_admin:
	$(OCAMLBUILD) -tag 'debug' 'test_admin.byte' && ./'test_admin.byte'

test_command:
	$(OCAMLBUILD) -tag 'debug' 'test_command.byte' && ./'test_command.byte'

match:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,ocamlgraph \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,ocamlgraph \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

zip: 
	zip final_project.zip *.ml* *.json _tags INSTALL.txt Makefile