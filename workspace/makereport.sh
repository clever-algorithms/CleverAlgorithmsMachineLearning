#!/bin/sh

INPUT=$1

pdflatex ${INPUT};true
bibtex ${INPUT};true
pdflatex ${INPUT};true
pdflatex ${INPUT};true

