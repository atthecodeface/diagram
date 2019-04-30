PANDOC = PATH="/Library/TeX/texbin/:${PATH}" pandoc --pdf-engine=pdflatex  

DIAGRAM_INSTALL_BIN := $(shell which diagram)
all: hml.pdf diagram.pdf

%.svg: dml/%.dml dss/%.dss $(DIAGRAM_INSTALL_BIN)
	@echo "Use diagam on DML/DSS files to create $@"
	@diagram --f dml/$*.dml dss/$*.dss --svg $@


%.pdf: %.md
	@echo "Run pandoc on $*.md to generate $@"
	@${PANDOC} $*.md -o $@


diagram.pdf: grid.svg flow.svg box.svg rot_text.svg rot_box.svg rot_box2.svg rot_grid.svg


