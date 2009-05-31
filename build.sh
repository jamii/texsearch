#!/bin/bash
ocamlfind ocamlopt -linkpkg -package netclient -package json-wheel -package json-static -syntax camlp4o -package json-static util.ml latex.ml edit.ml query.ml bktree.ml index.ml -o index