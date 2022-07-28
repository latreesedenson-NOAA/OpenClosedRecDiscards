# Repository for code to produce open and closed discards for GoM Red Snapper S74 RT

## There are three main folders: 
1. Code
2. InputFiles
3. Output Files

## Code produces the following:
- Charter mode Discards (Federally regulated 1981-2019)
- Private mode Discards (Federally regulated until 2016)
- Private mode Discards (State regulations from 2013-2019)

**Notes:** 
- You only need the state regulations csv (RSN_StatePrivate_Regs_table.csv) to make this run. This comes from SERO as part of the managment history and is saved seperately as a csv with "wave" column names altered for better use in R. The other regulations files for ForHire and FedPrivate can actually be created using the reg_table.R script, if you want to redo them.
- Each R script except for the graphics and combo scripts, check to make sure the genrec data haven't been changed on the S drive. Just source the files and it will tell you what to do from there. The genrec data is loaded from the S drive but can take a long time the first time, it is then saved as R data on github to speed up processing time.
- The working paper is posted [here](http://sedarweb.org/sedar-74-dw-35-red-snapper-general-recreational-open-and-closed-season-discard-development)


This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
