PANDOC = PATH="/Library/TeX/texbin/:${PATH}" pandoc --pdf-engine=pdflatex  

all: hml.pdf diagram.pdf

box.svg: dml/box.dml dss/box.dss
	diagram --f dml/box.dml dss/box.dss --svg box.svg

grid.svg: dml/grid.dml dss/grid.dss
	diagram --f dml/grid.dml dss/grid.dss --svg grid.svg

flow.svg: dml/flow.dml dss/flow.dss
	diagram --f dml/flow.dml dss/flow.dss --svg flow.svg

diagram.pdf: diagram.md grid.svg flow.svg box.svg
	${PANDOC} diagram.md -o diagram.pdf

hml.pdf: hml.md
	${PANDOC} hml.md -o hml.pdf
