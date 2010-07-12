#!/bin/bash
ocamlfind ocamlopt -linkpkg -package netclient -package json-wheel -package json-static -package xml-light -package str -syntax camlp4o -package json-static util.ml pid.ml latex.ml bktree.ml edit.ml query.ml index.ml -o index
