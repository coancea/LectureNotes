PAPERPDF=lecture-notes-pmph.pdf
all: $(PAPERPDF)

PDFLATEX=pdflatex -halt-on-error
#latex

%.pdf: %.tex futhark.tex Makefile
	$(PDFLATEX) $<
	bibtex $*
	$(PDFLATEX) $<
	$(PDFLATEX) $<
	rm -f *~ *.bbl *.log *.blg *.aux *.out *.toc

clean:
	rm -f *~ *.bbl *.log *.blg *.aux *.out *.toc $(PAPERPDF)
