#!/bin/sh

fixVersion(){
  PKGVER=`grep "Version:" ../DESCRIPTION | sed -e "s/Version: //"`
  sed -i -e "s/myversion{.*}/myversion{${PKGVER}}/" $1
}

buildCover(){
  cd cover
  ./build_cover.sh
  cd ..
}

cleanVignette(){
  rm -f *.aux *.bbl *.blg *.log *.out *.toc *.dvi *.backup
}

buildVignette(){
  cleanVignette
  
  fixVersion $1
  pdflatex $1
  bibname=`echo "$1" | sed -e 's/\..*//'`
  bibtex $bibname
  pdflatex $1
  pdflatex $1
  Rscript -e "tools::compactPDF('$1', gs_quality='ebook')"
}

publish(){
  mv -f *.pdf ../inst/doc/
  cp -f *.Rnw ../inst/doc/
}

#buildCover

buildVignette remoter.Rnw
buildVignette remote_machines.Rnw

cleanVignette
publish

