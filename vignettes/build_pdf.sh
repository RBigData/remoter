#!/bin/sh

cleanVignette(){
  rm -f *.aux *.bbl *.blg *.log *.out *.toc *.dvi
}

buildVignette(){
  pdflatex $1
  bibtex $1
  #makeindex $1
  #pdflatex $1
  pdflatex $1
  pdflatex $1
  Rscript -e "tools::compactPDF('$1', gs_quality='ebook')"
}


cleanVignette
buildVignette remoter.Rnw
buildVignette remote_machines.Rnw
cleanVignette


mv -f *.pdf ../inst/doc/
cp -f *.Rnw ../inst/doc/
