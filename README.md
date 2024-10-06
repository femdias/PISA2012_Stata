# Do-file for reading PISA 2012 in Stata
Stata do-files to read raw PISA2012 txt files

Older PISA microdata (OECD’s Programme for International Student Assessment) is provided only in .txt format with SAS and SPSS reading files, but lacks Stata equivalents. This repository contains Stata do-files to read PISA 2012 raw files and an R script for automating do-file creation. It could potentially be adapted for other PISA datasets, though I haven't tested that. It can also be used for writing reading scripts in other programming languages, with some time investiment.

The folder "output" contains the Stata do-files. The folder "input" contains the SAS reading files that I used as base. Those files can be downloaded (here)[https://www.oecd.org/en/data/datasets/pisa-2012-database.html], where you can also download the .txt raw microdata. The R script "Script_creating_dofile_GitHb.R" creates the do-files in a automized way. "PISA_txt_to_dta.Rproj" and "pathconfig.R" are used for working directory configuration.

Hope this helps someone, any questions feel free to ask me.
