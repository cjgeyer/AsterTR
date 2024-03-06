
%.tex : %.Rnw
	R CMD Sweave $<

tr658.pdf : tr658.tex aphids.tex chamae2.tex chamae.tex newnew.tex start.tex
	latexmk -pdf $<

start.tex : start.Rnw chamae-alpha.rda chamae2-alpha.rda

chamae.tex : chamae.Rnw chamae-alpha.rda

chamae2.tex : chamae2.Rnw chamae2-alpha.rda
