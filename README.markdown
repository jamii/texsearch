Texsearch is a search index specialised for LaTeX equations, forming part of the backend for Springer's [latexsearch.com](http://latexsearch.com). Latexsearch currently indexes more than 1.5 million documents drawn from Springer journals and books.

Every LaTeX equation in the corpus is parsed and evaluated on entry to produce an AST. The similarity between a pair of equations is calculated as the Levenshtein distance between their respective ASTs as a fraction of the total size of the ASTs. Given a LateX equation as a search term, texsearch will retrieve all equations in the corpus whose similarity to the search term falls under a specified margin.

The index is based on a suffix array which is capable of performing vicinity searches over any quasi-metric space using any query function satisfying:

    For all a. query a >= 0
    For all a, b. query b - query a <= dist a b

This index is stored in-memory and is relatively compact - the index for latexsearch.com is under 800MB.