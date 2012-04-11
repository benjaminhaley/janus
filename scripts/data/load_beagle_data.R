# To load the data from the beagle studies.
# Names will be changed to conform to the Janus
# naming convention.
#
# Written by Ben Haley in March of 2012.
#
# Usage:
#	source('scripts/data/load_janus_data.R')
#  	data <- b.data$load(from_cache=True)


# A namespace for data
#
b.data <- list()

# Dependencies 
#
source('scripts/util/webcache.R')
source('scripts/util/localcache.R')

# Configuration
#
b.data$.__URI <- "http://janus.northwestern.edu/dog_tissues/data/"
b.data$.__CACHED.NAME <- "b.data"
b.data$.__TABLE.NAMES <- c("demographics_and_dosage")     # TODO add pathologies
b.data$.__TABLE.HEADER.ROWS <- 1
b.data$.__TABLE.HEADER.SKIP <- 0
b.data$.__FILE.EXTENSION <- ".csv"


# Our primary function, return a data frame,
b.data$load <- function(from_cache=FALSE){
	is_cached <- localcache$is_cached(b.data$.__CACHED.NAME)
	if(from_cache && is_cached){
		data <- localcache$load(b.data$.__CACHED.NAME)
	}
	if(from_cache && !is_cached){
		print( "Beagle data cache was empty.  Loading from source")
		from_cache <- FALSE 
	}
	if(from_cache == FALSE){
		print( "Loading beagle data from source")		
		uris <- b.data$.__get_uris()
		csv_paths <- webcache$get(uris)
		data <- b.data$.__csv2data.frame(csv_paths, b.data$.__TABLE.HEADER.ROWS)
		localcache$save(data, b.data$.__CACHED.NAME) 
	}

	return(data)
}

# We need the full uri's for download
b.data$.__get_uris <- function(){
	uris = paste( 
			b.data$.__URI, 
			b.data$.__TABLE.NAMES, 
			b.data$.__FILE.EXTENSION, 
			sep="" 
			)
	return(uris)
}

# Convert the data into R and delete the file
b.data$.__csv2data.frame <- function(paths, header_rows){
	data <- mapply( 
		read.csv, 
		paths, 
		MoreArgs = list(
			as.is = TRUE,
			header = TRUE, 
			skip=b.data$.__TABLE.HEADER.SKIP, 
			row.names = b.data$.__TABLE.HEADER.ROWS
		),		
		SIMPLIFY = FALSE  
	)
	data <- data.frame(data[[1]])
	return(data)
}

		
