# I want a function to see the counts by category
# Motivated by Marissa's Characteriazation of her data

table_freq <- function(data_frame){
	table_data <- table(data_frame)
	freq_frame <- as.data.frame(table_data)
	non_zero_freq_frame <- freq_frame[freq_frame["Freq"] != 0,]
	rownames(non_zero_freq_frame) <- NULL
	return(non_zero_freq_frame)
}

# Test
data_frame <- data.frame(
	number=c(1,2,2,3,3,3),
	is_odd=c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE)
	)
expected <- data.frame(
	number=c(2,1,3),
	is_odd=c(FALSE, TRUE, TRUE),
	Freq=c(2,1,3)
	)
result <- table_freq(data_frame)
stopifnot(expected == result)
