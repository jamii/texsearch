#!/bin/bash

ocamlfind ocamlmktop -linkpkg -g \
    -package extlib \
    -package str \
    -syntax camlp4o \
    -package json-static \
    dynArray.mli dynArray.ml \
    util.mli util.ml \
    latex.mli latex.ml \
    edit.ml \
    suffix_array.mli suffix_array.ml \
    suffix_array_test.ml \
-o test

ocamlfind ocamlopt -linkpkg \
    -package extlib \
    -package netclient \
    -package json-wheel \
    -package json-static \
    -package xml-light \
    -package str \
    -syntax camlp4o \
    -package json-static \
    dynArray.mli dynArray.ml \
    util.mli util.ml \
    pid.mli pid.ml \
    latex.mli latex.ml \
    edit.ml \
    suffix_array.mli suffix_array.ml \
    query.ml \
    index.ml \
-o index