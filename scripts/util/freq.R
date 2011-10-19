# Functions related to counts and frequencey of the data
# bmh Oct 2011
# 
#

# @TODO this will probably need breaking up into distinct classes

freq <- list()

# Filter columns by the number of events they have
freq$get_columns <- function(data_frame, columns, minimum_sum=0){
	colsum <- colSums(data_frame[columns])
	above_cutoff <- (colsum >= minimum_sum)
	return(columns[above_cutoff])
}

# get_columns Test
data_frame <- data.frame(
	below = c(FALSE,FALSE,TRUE),
	at    = c(FALSE,TRUE, TRUE),
	above = c(TRUE, TRUE, TRUE),
	excluded = c(TRUE, TRUE, TRUE)
	)
columns <- c("below", "at", "above")
minimum_sum <- 2
expected <- c("at", "above")
result <- freq$get_columns(data_frame, columns, minimum_sum)
stopifnot( expected == result )

# I want a function to see the counts by category
# Motivated by Marissa's Characteriazation of her data
freq$get_table <- function(data_frame){
	table_data <- table(data_frame)
	freq_frame <- as.data.frame(table_data)
	non_zero_freq_frame <- freq_frame[freq_frame["Freq"] != 0,]
	rownames(non_zero_freq_frame) <- NULL
	return(non_zero_freq_frame)
}

# get_table Test
data_frame <- data.frame(
	number=c(1,2,2,3,3,3),
	is_odd=c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE)
	)
expected <- data.frame(
	number=c(2,1,3),
	is_odd=c(FALSE, TRUE, TRUE),
	Freq=c(2,1,3)
	)
result <- freq$get_table(data_frame)
stopifnot(expected == result)
