#!/bin/sh

V1=peter.tex
V2=draft2.tex

latexdiff $V1 $V2 > diff.tex

. generate.sh diff.tex
