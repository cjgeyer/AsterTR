
%.tex : %.Rnw
	R CMD Sweave $<

tr644.pdf : tr644.tex abstract.tex body.tex coneflr.tex macros.tex nats.tex predict.tex sim.tex unit.tex
	latexmk -pdf $<

