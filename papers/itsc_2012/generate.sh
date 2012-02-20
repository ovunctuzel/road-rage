#!/bin/sh

if [[ $# -eq 1 ]]; then
  INPUT=$1
else
  INPUT=draft2.tex
fi

# generate graphviz diagrams
for f in *.dot; do
  dot -Tps2 -o ${f%dot}eps $f
  epstopdf ${f%dot}eps
done

pdflatex $INPUT
bibtex ${INPUT%tex}
# pretty sure we have to re-run this twice to include the newly generated bib
# and references
pdflatex $INPUT
pdflatex $INPUT
