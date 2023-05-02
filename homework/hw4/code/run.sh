#!/bin/bash

ASTFILENAME=target

python3.10 frontend/ast2json.py $1 > /tmp/$ASTFILENAME.json
dune build main.exe
./_build/default/main.exe /tmp/$ASTFILENAME.json
#./_build/default/main.exe /tmp/$ASTFILENAME.json > _build/output.py
# python3.10 frontend/reformat.py _build/output.py > _build/formatted.py 
