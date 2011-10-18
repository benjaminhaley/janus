# To check if local data is up to date with the web
# and allow a user to get the data from local if it is
# current or remote if it is not.
# 
# NOTE
# This is not made for rapidly evolving files
# And it makes strong assumptions about the modified
# since header that might cause it to behave strangely
# if the header does not conform to its expectations.
# 
# USE
# source('../util/webcache.R')
# url <- http://example.zip
# filepath <- webcache$get(url)
# TRUE == webcache$is_current(url)
# 
# bmh Sept 2011

source('../util/package.R')
dependencies = "RCurl"
package$load(dependencies)

webcache = list()
webcache$.__DATA_DIR = '../../data/'

webcache$get <- function(uris){
	local_paths <- mapply(webcache$.__get_one, uris)
	return(local_paths)
}

webcache$is_current <- function(url){
	ctime <- webcache$.__get_cached_time(url)
	mtime <- webcache$.__get_modified_time(url)
	is_current <- webcache$.__cache_is_up_to_date(ctime, mtime)
	return(is_current)
}

webcache$.__get_one <- function(url){
	if(!webcache$is_current(url)){
		webcache$.__cache(url)
	}
	file <- webcache$.__get_file_from_cache(url)
	return(file)
}

# We need the power to save and retrieve remote files locally
webcache$.__cache <- function(url){
	destfile <- webcache$.__url_2_filepath(url)
	# We use curl ensure that this step is blocking.
	# we don't want to return until the file is downladed.
	method <- "curl"
	download.file(url, destfile, method)
	webcache$.__set_cache_time(url)
}
webcache$.__get_file_from_cache <- function(url){
	filepath <- webcache$.__url_2_filepath(url)
	return(filepath)
}

# We need translationgs between urls and local filenames
webcache$.__url_2_filepath <- function(url){
	filepath <- paste( 
		webcache$.__DATA_DIR,
		URLencode(url, reserved=TRUE),
		sep=""
		)
	return(filepath)
}
webcache$.__url_2_time_filepath <- function(url){
	time_filepath <- paste(
		webcache$.__url_2_filepath(url),
		".time.rds",
		sep=""
		)
	return(time_filepath)
}
# Test
url <- "http://example.com"
filepath <- webcache$.__url_2_filepath(url)
time_filepath <- webcache$.__url_2_time_filepath(url)
stopifnot(filepath == "../../data/http%3a%2f%2fexample.com") 
stopifnot(time_filepath == "../../data/http%3a%2f%2fexample.com.time.rds") 

# We need functions to track when a file was cached
#
webcache$.__cache_is_up_to_date <- function(cached_time, modified_time){
	if(modified_time >= cached_time){
		return(FALSE)
	} else {
		return(TRUE)
	}
}
webcache$.__set_cache_time <- function(url){
	time_filepath <- webcache$.__url_2_time_filepath(url)
	time <- Sys.time()
	saveRDS(time, time_filepath)
}
webcache$.__get_cached_time <- function(url){
	time_filepath <- webcache$.__url_2_time_filepath(url)
	if(file.exists(time_filepath)){
		time <- readRDS(time_filepath)
	} else {
		time <- 0
	}
	return(time)
}
webcache$.__get_modified_time <- function(url){
	h = basicHeaderGatherer()

	# Retrieve the header
	getURI(
		url,
		headerfunction = h$update,
		nobody=TRUE
		)
	lm_header <- h$value()["Last-Modified"]
	if(is.na(lm_header)) {
		last_modified_date <- Sys.time()
	} else {
		last_modified_date <- lm_header
		# Convert to a time object
		# *note I am making a big assumption that the last modified header is formatted
		# according to RFC2822 and in GMT time.  This could cause trouble.
		last_modified_date <- strptime(last_modified_date,  "%a, %d %b %Y %H:%M:%S", tz="GMT")
	}
	return(last_modified_date)
}

# Test 
#
url <- "http://example.com"
before_time <- Sys.time()
Sys.sleep(0.001)
webcache$.__set_cache_time(url)
Sys.sleep(0.001)
after_time <- Sys.time()
cache_time <- webcache$.__get_cached_time(url)
not_up_to_date_if_modified_after_caching = webcache$.__cache_is_up_to_date(cache_time, after_time)
stopifnot( webcache$.__cache_is_up_to_date(cache_time, before_time) == TRUE )
stopifnot( webcache$.__cache_is_up_to_date(cache_time, after_time) == FALSE )
