#!/bin/sh

V1=draft1.tex
V2=peter.tex

latexdiff $V1 $V2 > diff.tex

. generate.sh diff.tex
