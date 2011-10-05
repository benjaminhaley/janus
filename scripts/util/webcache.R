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
# source('webcache.R')
# url <- http://example.zip
# filepath <- cache$get(url)
# TRUE == cache$is_current(url)
# 
# bmh Sept 2011

source('../util/package.R')
dependencies = "RCurl"
package$load(dependencies)

cache = list()
cache$.__DATA_DIR = '../../data/'

cache$get <- function(url){
	if(!cache$is_current(url)){
		cache$.__cache(url)
	}
	file <- cache$.__get_file_from_cache(url)
	return(file)
}
cache$is_current <- function(url){
	ctime <- cache$.__get_cached_time(url)
	mtime <- cache$.__get_modified_time(url)
	is_current <- cache$.__cache_is_up_to_date(ctime, mtime)
	return(is_current)
}

# We need the power to save and retrieve remote files locally
cache$.__cache <- function(url){
	destfile <- cache$.__url_2_filepath(url)
	# We use curl ensure that this step is blocking.
	# we don't want to return until the file is downladed.
	method <- "curl"
	download.file(url, destfile, method)
	cache$.__set_cache_time(url)
}
cache$.__get_file_from_cache <- function(url){
	filepath <- cache$.__url_2_filepath(url)
	return(filepath)
}

# We need translationgs between urls and local filenames
cache$.__url_2_filepath <- function(url){
	filepath <- paste( 
		cache$.__DATA_DIR,
		URLencode(url, reserved=TRUE),
		sep=""
		)
	return(filepath)
}
cache$.__url_2_time_filepath <- function(url){
	time_filepath <- paste(
		cache$.__url_2_filepath(url),
		".time.rds",
		sep=""
		)
	return(time_filepath)
}
# Test
url <- "http://example.com"
filepath <- cache$.__url_2_filepath(url)
time_filepath <- cache$.__url_2_time_filepath(url)
stopifnot(filepath == "../../data/http%3a%2f%2fexample.com") 
stopifnot(time_filepath == "../../data/http%3a%2f%2fexample.com.time.rds") 

# We need functions to track when a file was cached
#
cache$.__cache_is_up_to_date <- function(cached_time, modified_time){
	if(modified_time >= cached_time){
		return(FALSE)
	} else {
		return(TRUE)
	}
}
cache$.__set_cache_time <- function(url){
	time_filepath <- cache$.__url_2_time_filepath(url)
	time <- Sys.time()
	saveRDS(time, time_filepath)
}
cache$.__get_cached_time <- function(url){
	time_filepath <- cache$.__url_2_time_filepath(url)
	if(file.exists(time_filepath)){
		time <- readRDS(time_filepath)
	} else {
		time <- 0
	}
	return(time)
}
cache$.__get_modified_time <- function(url){
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
cache$.__set_cache_time(url)
Sys.sleep(0.001)
after_time <- Sys.time()
cache_time <- cache$.__get_cached_time(url)
not_up_to_date_if_modified_after_caching = cache$.__cache_is_up_to_date(cache_time, after_time)
stopifnot( cache$.__cache_is_up_to_date(cache_time, before_time) == TRUE )
stopifnot( cache$.__cache_is_up_to_date(cache_time, after_time) == FALSE )
