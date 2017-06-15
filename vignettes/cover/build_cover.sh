#!/bin/sh

PKGVER=`grep "Version:" ../../DESCRIPTION | sed -e "s/Version: //"`
sed -i -e "s/thispackageversion}{.*}/thispackageversion}{${PKGVER}}/" cover.tex

buildCover(){
  xelatex $1
  xelatex $1
  ### NOTE compactPDF strips out the logo for some reason ???
  #Rscript -e "tools::compactPDF('.', gs_quality='ebook')"
}

cleanCover(){
  rm -rf *.dvi *.aux *.bbl *.blg *.log *.out *.toc *.idx *.lof *.lot *.ind *.ilg
}

rm -rf *.pdf
cleanCover
buildCover remoter.tex
buildCover remote_machines.tex
cleanCover

