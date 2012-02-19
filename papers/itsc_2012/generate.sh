#!/bin/sh

# generate graphviz diagrams
for f in *.dot; do
  dot -Tps2 -o ${f%dot}eps $f
  epstopdf ${f%dot}eps
done

pdflatex root.tex
bibtex root
# pretty sure we have to re-run this twice to include the newly generated bib
# and references
pdflatex root.tex
pdflatex root.tex
