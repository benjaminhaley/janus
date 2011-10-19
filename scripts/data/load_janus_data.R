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
source('../util/localcache.R')
source('../util/zipfile.R')
c <- ontology$load_columns()

# Configuration
#
j.data$.__URI <- "http://janus.northwestern.edu/janus2/data/"
j.data$.__CACHED.NAME <- "j.data"
j.data$.__TABLE.NAMES <- c("demographics", "macro_pathologies")
j.data$.__TABLE.HEADER.ROWS <- 1
j.data$.__ZIP.EXTENSION <- ".zip"
j.data$.__FILE.EXTENSION <- ".csv"

# @TODO replace this monstrocity with a function that loads the key and other translation features

# Our primary function, return a data frame,
j.data$load <- function(from_cache=FALSE){
	is_cached <- localcache$is_cached(j.data$.__CACHED.NAME)
	if(from_cache && is_cached){
		data <- localcache$load(j.data$.__CACHED.NAME)
	}
	if(from_cache && !is_cached){
		print( "Cache was empty.  Loading from source")
		from_cache <- FALSE 
	}
	if(from_cache == FALSE){
		uris <- j.data$.__get_uris()
		zip_paths <- webcache$get(uris)
		csv_paths <- zipfile$unzip(zip_paths)
		raw_data <- j.data$.__csv2data.frame(csv_paths, j.data$.__TABLE.HEADER.ROWS)
		data <- j.data$.__normalize(raw_data)
		localcache$save(data, j.data$.__CACHED.NAME) 
	}

	return(data)
}

# Convert types where needed
#
j.data$.__normalize <- function(raw_data){
	data <- raw_data
	names(data) <- j.data$.__get_normalized_names(names(data))
	data[['necroscopy_date']] <- as.Date(data[['necroscopy_date']], format="%Y-%m-%d" )
	data[['expt']] <- as.factor(data[['expt']])

	# We will add in macros which are not presented in the dataset
	missing_macros <- c$MACROS_[!c$MACROS_ %in% colnames(data)]
	empty_matrix <- matrix(nrow=nrow(data), ncol=length(missing_macros))
	colnames(empty_matrix) <- missing_macros
	data <- cbind(data, empty_matrix)
	data[c$MACROS_] <- data.frame(mapply(function(column){
		column[is.na(column)] <- c(FALSE)
		return(column)
	},data[c$MACROS_]))
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

		
