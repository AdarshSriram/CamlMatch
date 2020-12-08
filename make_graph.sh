#!/bin/bash

dot "graph.dot" -Tpdf -o "Usergraph.pdf"
xdg-open "Usergraph.pdf" 2>/dev/null