# To load the data from the janus studies. 
# Written by Ben Haley in March of 2011.
# Updated Oct 2011
# Usage:
#   See __FILE__unit_tests.R

# A namespace for data
#
j.data <- list()

# Dependencies 
#
source('../util/webcache.R')
source('../data/ontology.R')

# Configuration
#
j.data$.__URI <- "http://janus.northwestern.edu/janus2/data/"
j.data$.__DATA.DIR <- "../../data/"
j.data$.__CACHED.PATH <- "../../data/j.data.RDS"
j.data$.__TABLE.NAMES <- c("demographics", "macro_pathologies")
j.data$.__TABLE.HEADER.ROWS <- 1
j.data$.__ZIP.EXTENSION <- ".zip"
j.data$.__FILE.EXTENSION <- ".csv"

# @TODO replace this monstrocity with a function that loads the key and other translation features

# Our primary function, return a data frame,
j.data$load <- function(from_cache=FALSE){
	if(from_cache==FALSE){
		uris <- j.data$.__get_uris()
		zip_paths <- j.data$.__download(uris)
		csv_paths <- j.data$.__unzip(zip_paths)
		raw_data <- j.data$.__csv2data.frame(csv_paths, j.data$.__TABLE.HEADER.ROWS)
		data <- j.data$.__normalize(raw_data)
		saveRDS(data, j.data$.__CACHED.PATH) 
	}
	data <- readRDS(j.data$.__CACHED.PATH)
	return(data)
}

# Convert types where needed
#
j.data$.__normalize <- function(raw_data){
	data <- raw_data
	names(data) <- j.data$.__get_normalized_names(names(data))
	data[['necroscopy_date']] <- as.Date(data[['necroscopy_date']], format="%Y-%m-%d" )
	data[['expt']] <- as.factor(data[['expt']])
	data[o$MACROS] <- data.frame(mapply(function(column){
		column[is.na(column)] <- c(FALSE)
		return(column)
	},data[o$MACROS]))
	return(data)
}

# Get the names from the mash of R
j.data$.__get_normalized_names <- function(names){
	period_split <- strsplit(names,".", fixed=TRUE)
	names <- mapply(function(period_split){
		last_elem <- period_split[length(period_split)]
		name <- last_elem
		return(name)
	},period_split)
	return(names)
}


# We need the full uri's for download
j.data$.__get_uris <- function(){
	uris = paste( 
			j.data$.__URI, 
			j.data$.__TABLE.NAMES, 
			j.data$.__ZIP.EXTENSION, 
			sep="" 
			)
	return(uris)
}

# Download w caching whoa!
j.data$.__download <- function(uris){
	local_paths <- mapply(cache$get, uris)
	return(local_paths)
}

# Unzip and keep the originals
j.data$.__unzip <- function(zip_paths){
	csv_paths <- mapply(function(zip_path){
		unzip(zip_path, exdir=j.data$.__DATA.DIR)
		name <- as.character(unzip(zip_path, list=TRUE)[1,1])
		csv_path <- paste(j.data$.__DATA.DIR, name, sep="")
		return(csv_path)
	}, zip_paths)
	return(csv_paths )
}

# Convert the data into R and delete the file
j.data$.__csv2data.frame <- function(paths, header_rows){
	data <- mapply( 
		read.csv, 
		paths, 
		as.is = TRUE,
		header = TRUE, 
		skip=1, 
		row.names = j.data$.__TABLE.HEADER.ROWS 
		)
	data <- data.frame(data)
	mapply( 
	   unlink, 
	   paths 
	)
	return(data)
}

		
