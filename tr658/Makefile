
%.tex : %.Rnw
	R CMD Sweave $<

tr658.pdf : tr658.tex aphids.tex chamae2.tex chamae.tex newnew.tex start.tex
	latexmk -pdf $<
