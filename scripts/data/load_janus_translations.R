# To load the translation tables for pathology
# codes used in the janus studies. 
# Written by Ben Haley in October of 2011.
# Usage:
#   source('../data/load_janus_translations.R')
#	t <- j.translations$load(from_cache=TRUE)
#   t$get_description(c("NTYG_L", "PNC_N"))

# A namespace for data
#
j.translations <- list()

# Dependencies 
#
source('../util/webcache.R')
source('../util/localcache.R')
source('../util/zipfile.R')

# Configuration
#
j.translations$.__URI <- 
	"http://janus.northwestern.edu/janus2/data/pathologies_key.zip"
j.translations$.__CACHED.NAME <- "j.translations"
j.translations$.__TABLE.HEADER.ROWS <- 1

# Our primary function, return a data frame,
j.translations$load <- function(from_cache=FALSE){
	is_cached <- localcache$is_cached(j.translations$.__CACHED.NAME)
	if(from_cache && is_cached){
		translations <- localcache$load(j.translations$.__CACHED.NAME)
	}
	if(from_cache && !is_cached){
		print( "Local Cache is empty.  Loading from source")
		from_cache <- FALSE 
	}
	if(from_cache == FALSE){
		uri <- j.translations$.__URI
		zip_path <- webcache$get(uri)
		csv_path <- zipfile$unzip(zip_path)
		translations <- j.translations$.__csv2data.frame(csv_path, j.translations$.__TABLE.HEADER.ROWS)
		localcache$save(translations, j.translations$.__CACHED.NAME) 
	}

	return(translations)
}

# Convert the data into R and delete the file
j.translations$.__csv2data.frame <- function(paths){
	translations <- mapply( 
		read.table,
		paths, 
		header=TRUE, 
		skip=1, 
		sep=","
		as.is=TRUE
	)
	translations <- data.frame(translations)
	mapply( 
	   unlink, 
	   paths 
	)
	return(translations)
}

		
