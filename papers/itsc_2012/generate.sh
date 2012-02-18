#!/bin/sh
pdflatex root.tex
bibtex root
# pretty sure we have to re-run this twice to include the newly generated bib
# and references
pdflatex root.tex
pdflatex root.tex
