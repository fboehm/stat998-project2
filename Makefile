# Makefile
# Goal: to create a 

all: report2.pdf

report2.pdf: report2.Rmd
		Rscript -e 'rmarkdown::render("report2.Rmd")'

clean: 
		rm report2.pdf

view: 
		open report2.pdf