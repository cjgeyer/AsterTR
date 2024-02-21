
%.pdf : %.tex
	latexmk -pdf $<

all: tr698r.pdf

techreport.v2.tex : MSexta.Aster.csv techreport.v2.Rnw
	Rscript -e 'knitr::knit("techreport.v2.Rnw")'

tr698r.pdf : techreport.v2.pdf
	ln $< $@

