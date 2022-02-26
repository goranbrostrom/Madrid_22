R_OPTS=--no-save --no-restore --no-init-file --no-site-file # vanilla, but with --environ

paper.pdf: paper.Rmd
	R ${R_OPTS} -e "rmarkdown::render('paper.Rmd')"
	pdflatex paper.tex
	bibtex paper
	pdflatex paper

