SD = scripts
FD = figures
DD = data
RD = data-raw
RP = reports
CMD = R CMD BATCH --no-save
SCR = Rscript -e

torun: $(RP)/temperature-analysis-post-neo.html


clean:
	-rm -f *.Rout
	-rm -f *.pdf

cleanall: 
	-rm -f *.Rout
	-rm -f $(DD)/*
	-rm -f $(FD)/*
	-rm -f reports/*.html
	-rm -f reports/*.pdf


$(RP)/temperature-analysis-post-neo.html: $(RP)/temperature-analysis-post-neo.Rmd $(DD)/temp_case_data.rda $(RP)/temperature-analysis.html
	$(SCR) "rmarkdown::render('$(RP)/temperature-analysis-post-neo.Rmd')"


$(RP)/temperature-analysis.html: $(RP)/temperature-analysis.Rmd $(DD)/temp_case_data.rda
	$(SCR) "rmarkdown::render('$(RP)/temperature-analysis.Rmd')"

$(FD)/map-samp-mig.png: $(SD)/map.R
	$(CMD) $(SD)/map.R
	

$(DD)/temp_case_data.rda: $(SD)/combine_case_and_temp.R 
	$(CMD) $(SD)/combine_case_and_temp.R
	

$(DD)/case_data.rda: $(SD)/case_dat.R $(DD)/indiv_data.rda
	$(CMD) $(SD)/case_dat.R

$(DD)/indiv_data.rda: $(SD)/make_indiv_data.R
	$(CMD) $(SD)/make_indiv_data.R
	