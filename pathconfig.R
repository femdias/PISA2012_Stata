# ###########################
# CONFIG: define paths and filenames for later reference
# ###########################
library('rprojroot')
# Change the basepath depending on your system

basepath <- rprojroot::find_rstudio_root_file()

# Main directories
output <- file.path(basepath, "output" )
input <- file.path(basepath, "input")

for ( dir in list(input,output)){
	if (file.exists(dir)){
	} else {
	dir.create(file.path(dir))
	}
}

