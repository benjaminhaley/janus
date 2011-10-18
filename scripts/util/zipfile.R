# Given an array of zipped files,
# returns a set of unzipped filepaths
# assumes that there is only one zipped
# file per path
# bmh Oct 2011
# 
# Usage:
#	source('../util/zipfile.R')
#   filepaths <- zipfile$unzip(c("file1.zip", "file2.zip"))

zipfile <- list()
zipfile$.__DATA.DIR <- '../../data/'

zipfile$unzip <- function(zip_paths){
	file_paths <- mapply(function(zip_path){
		unzip(zip_path, exdir=zipfile$.__DATA.DIR)
		name <- as.character(unzip(zip_path, list=TRUE)[1,1])
		csv_path <- paste(zipfile$.__DATA.DIR, name, sep="")
		return(csv_path)
	}, zip_paths)
	return(file_paths)
}
