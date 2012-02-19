#!/bin/sh

# generate graphviz diagrams
for f in *.dot; do
  dot -Tps2 -o ${f%dot}eps $f
  epstopdf ${f%dot}eps
done

pdflatex diff.tex
bibtex diff
# pretty sure we have to re-run this twice to include the newly generated bib
# and references
pdflatex diff.tex
pdflatex diff.tex
