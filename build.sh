#!/bin/bash
ocamlfind ocamlopt -linkpkg \
    -package netclient \
    -package json-wheel \
    -package json-static \
    -package xml-light \
    -package str \
    -syntax camlp4o \
    -package json-static \
    util.mli util.ml \
    pid.mli pid.ml \
    latex.mli latex.ml \
    edit.ml \
    suffix_array.mli suffix_array.ml \
    query.ml \
    index.ml \
-o index
