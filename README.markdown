Texsearch is a search index specialised for LaTeX equations, forming part of the backend for Springer's [latexsearch.com](http://latexsearch.com). Latexsearch currently indexes more than 2 million documents drawn from Springer journals and books.

Every LaTeX equation in the corpus is parsed and evaluated on entry to produce an AST. The similarity between a pair of equations is calculated as the Levenshtein distance between their respective ASTs as a fraction of the total size of the ASTs. Given a LateX equation as a search term, texsearch will retrieve all equations in the corpus whose similarity to the search term falls under a specified margin.

The index is based on a suffix array which is capable of performing vicinity searches over any quasi-metric space using any query function satisfying:

    For all a. query a >= 0
    For all a, b. query b - query a <= dist a b

This index is stored in-memory and is relatively compact - the index for latexsearch.com is under 800MB.

# Architecture

Couchdb is the root process. The preprocessor and index are run as _external services on couchdb. Raw data is stored in the 'documents' db on couchdb. The search index is stored in the file 'data/index'. 

Springer documents are uploaded to the server as xml files. The command 'db.py --add some_doc.xml' extracts latex formulae and metadata from some_doc.xml, runs the latex through the preprocessor and stores the results in couchdb. The command 'index -update' uses the couchdb change log to locate new or modified documents and update the index file. Restarting the index external service causes it to load the new index file.

# Requirements

Tested with:

couchdb 0.6.0

ocaml 3.12.0
ancient 0.9.0
json-wheel 1.0.6
json-static 0.9.8
ocamlnet 3.2
pcre-ocaml 6.2.2
xml-light 2.2

python 2.6.6
couchdb 0.6 (python lib)
httplib2 0.5.0
plastex 0.9.2

