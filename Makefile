
%.pdf : %.tex
	latexmk -pdf $<

all: tr698r.pdf tr696.pdf tr692.pdf tr675r.pdf tr674r.pdf tr671r.pdf \
        tr670.pdf

tr698r.pdf :
	cd tr698 && $(MAKE)
	ln tr698/tr698r.pdf .

tr675r.pdf : betaTR.pdf
	ln $< $@

tr696.tex : tr696.Rnw
	R CMD Sweave $<

tr692.tex : tr692.Rnw
	R CMD Sweave $<

betaTR.tex : betaTR.Rnw
	R CMD Sweave $<

tr674r.tex : tr674r.Rnw
	R CMD Sweave $<

tr671r.tex : tr671r.Rnw leap-funs.R
	R CMD Sweave $<

tr670.tex : tr670.Rnw
	R CMD Sweave $<

tr669.tex : tr669.Rnw
	R CMD Sweave $<

tr666.tex : tr666.Rnw
	R CMD Sweave $<

tr661.tex : tr661.Rnw
	R CMD Sweave $<

tr658.pdf :
	cd tr658 && $(MAKE)
	ln tr658/tr658.pdf .

tr644.pdf :
	cd tr644 && $(MAKE)
	ln tr644/tr644.pdf .

