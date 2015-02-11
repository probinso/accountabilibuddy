TITLE=secrets

default: $(TITLE).tex
	pdflatex --shell-escape $(TITLE).tex
	pdflatex --shell-escape $(TITLE).tex

clean: 
	rm -rf $(TITLE).aux $(TITLE).log *~

veryclean: clean
	rm -rf $(TITLE).pdf
