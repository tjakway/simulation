STEM=rocket_notes
SRC=$(STEM).tex
SRC_BIB=$(STEM).bib
PDF_DRAFT_OUT=$(STEM).pdf
PANDOC_OPTS=--verbose --latex-engine=pdflatex --smart --normalize
DOCX_OUT=Export.docx
ODT_OUT=Export.odt
EXPORT_SRC=Export.tex

LATEXMK_OPTIONS=-pdflatex="pdflatex --shell-escape %O %S" -bibtex -pdf --shell-escape

.PHONY: .all

.all : paper

.PHONY: paper

paper: $(SRC)
	latexmk $(LATEXMK_OPTIONS) $(SRC)

.PHONY: clean


#suppress errors if there's no pdf/dvi/bbl files
clean:
	latexmk -c $(SRC) ; rm -f $(PDF_DRAFT_OUT) *.dvi *.bbl *.run.xml Export.tex *.aux *.bcf *.blg *.fdb_latexmk *.fls *.log *-blx.bib Export.odt Export.docx Export.pdf;
	
.PHONY: view

view: paper
	latexmk $(LATEXMK_OPTIONS) -pvc $(SRC)

.PHONY: export

#remove draft option when exporting
export: clean
	sed 's=\\usepackage\[draft\]{ThomasEssays}=\\usepackage{ThomasEssays}=g' $(SRC) > $(EXPORT_SRC) ; latexmk $(LATEXMK_OPTIONS) $(EXPORT_SRC)

docx: export
	pandoc $(PANDOC_OPTS) --bibliography $(SRC_BIB) --to=docx -o $(DOCX_OUT) $(EXPORT_SRC)

odt: export
	pandoc $(PANDOC_OPTS) --bibliography $(SRC_BIB) --to=odt -o $(ODT_OUT) $(EXPORT_SRC)

.PHONY: spellcheck
spellcheck:
	hunspell -r -p .hunspell_en_US -t $(SRC)

.PHONY: pdflatex
pdflatex:
	pdflatex --shell-escape $(SRC); biber $(SRC); pdflatex --shell-escape $(SRC); pdflatex --shell-escape $(SRC);

.PHONY: wordcount
wordcount:
	texcount -inc -sum $(SRC)
