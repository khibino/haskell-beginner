
%.pdf: %.dvi
	dvipdfmx $(@:.pdf=.dvi)

%.dvi %.log %.aux: %.tex
	platex $<

%.ppm: %.png
	convert $< $@

%.bb: %.ppm
	bbox $< > $@

%.o: %.hs
	ghc -c -Wall $<

all: And0.bb arrow.pdf

hsbuild: CircuitDef.o And0.o Circuit.o Circuit2.o 

clean:
	$(RM) *.o *.hi
	$(RM) *.dvi *.log *.aux *.toc
	$(RM) *.vrb *.out *.nav *.snm
	$(RM) *.bb

veryclean: clean
	$(RM) *.pdf
