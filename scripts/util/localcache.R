# It should be easy to save and restore
# R objects w/o reinitialing them each
# time or determining a unique namespace
# bmh Oct 2011
#
# Usage:
#   source('../util/localcache.R')
#   object <- list(1,2,3)
#   objectname <- "Cool R shit"
#   localcache$save(object, objectname)
#	if(localcache$is_cached(objectname)){
#		return(localcache$load(objectname))
#	}

# A namspace
localcache <- list()

localcache$.__CACHED.DIR <- "../../data/"

localcache$is_cached <- function(objectname){
	filepath <- localcache$.__get_filepath(objectname)
	is_cached <- file.exists(filepath)
	return(is_cached)
}

localcache$load <- function(objectname){
	filepath <- localcache$.__get_filepath(objectname)
	object <- readRDS(filepath)
	return(object)
}

localcache$save <- function(object, objectname){
	filepath <- localcache$.__get_filepath(objectname)
	saveRDS(object, filepath)
}

localcache$.__get_filepath <- function(objectname){
	filepath <- paste(
		localcache$.__CACHED.DIR, objectname, ".RDS", sep=""
		)
	return(filepath)
}

