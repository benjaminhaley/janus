# We want to be able to parse a dataset more easily
#
# bmh Nov 2011
select <- list()

# Only columns with more than a fixed sum
select$by_count <- function(data_frame, colnames, min=1, max=length(colnames)){
	sums <- colSums(data_frame[colnames])
	ordered <- sort(sums, decreasing=TRUE)
	subset <- ordered[min:max]
	names <- names(subset)
	return(names)
}

# Test
select$._test_data <- data.frame("col1"=c(3, 4), "col2"=c(1, 2))
select$._expected <- "col2"
select$._result <- select$by_count(select$._test_data, c("col1", "col2"), min=2)
stopifnot(identical(select$._expected, select$._result))

# Only select values for data frames
select$by_acceptable <- function(data_frame, acceptable_list){
	for(col in names(acceptable_list)) { 
		data_frame <- data_frame[data_frame[[col]] %in% acceptable_list[[col]],]
	}
	return(data_frame)
}

# Test
select$._test_data <- data.frame("col1"=c(3, 4, 5), "col2"=c(1, 2, 3))
select$._test_list <- list("col1"=c(3, 4), "col2"=c(1, 3))
select$._expected <- data.frame("col1"=c(3), "col2"=c(1))
select$._result <- select$by_acceptable(select$._test_data, select$._test_list)
stopifnot(identical(select$._expected, select$._result))