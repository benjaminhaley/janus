# The one stop shop where you go to get data.
# This script will contain confirgution settings to download a suite of data
# It is not very functional yet, just a namespace filler
# bmh Oct 2011

data <- list()

data$load <- function(datasets, from_cache=FALSE){
	if(datasets == 'janus'){
		source('../data/load_janus_data.R')
		data <- j.data$load(from_cache)
	} else {
		stop("I do not recognize the dataset entered")
	}
	return data
}

