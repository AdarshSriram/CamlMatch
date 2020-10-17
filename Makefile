MODULES=client store server
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

test:
	$(OCAMLBUILD) -tag 'debug' 'test_store.byte' && ./'test_store.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_client.byte' && ./'test_client.byte'
	$(OCAMLBUILD) -tag 'debug' 'test_server.byte' && ./'test_server.byte'

test_store:
	$(OCAMLBUILD) -tag 'debug' 'test_store.byte' && ./'test_store.byte'

test_client:
	$(OCAMLBUILD) -tag 'debug' 'test_client.byte' && ./'test_client.byte'

test_server:
	$(OCAMLBUILD) -tag 'debug' 'test_server.byte' && ./'test_server.byte'


match:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
