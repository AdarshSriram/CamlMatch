MODULES=main client state survey command author
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD=ocamlbuild -use-ocamlfind
MAIN=main.byte # start match/messaging

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
	rm -rf final_project.zip


test:
	$(OCAMLBUILD) -tag 'debug' 'test_state.byte' && ./'test_state.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_client.byte' && ./'test_client.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_survey.byte' && ./'test_survey.byte'

test_state:
	$(OCAMLBUILD) -tag 'debug' 'test_state.byte' && ./'test_state.byte'

test_client:
	$(OCAMLBUILD) -tag 'debug' 'test_client.byte' && ./'test_client.byte'

test_survey:
	$(OCAMLBUILD) -tag 'debug' 'test_survey.byte' && ./'test_survey.byte'

match:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip: 
	zip final_project.zip *.ml* _tags INSTALL.txt Makefile