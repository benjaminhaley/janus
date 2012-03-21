############################################################################### 
#
# Functions useful for cross-validation
#
# bmh March 2012
#
# Usage 
#   source("scripts/util/cv.R")
#   data <- cv$subset(data, validate.percent=0.2, test.percent=0.2)
#
############################################################################### 

cv <- list()

cv$subset <- function(data, validate.percent, test.percent){
	data$set            <- 'train'
	val                 <- (runif(nrow(data)) < validate.percent)
	test                <- (runif(nrow(data)) < test.percent * (1 - validate.percent)^-1)
	data[test, "set"]   <- 'test'
	data[val, "set"]    <- 'val'
	data
}

# Test 
	set.seed(73)
	cv$._input_data <- data.frame(a=1:5)
	cv$._input_val  <- 0.2
	cv$._input_test <- 0.2
	cv$._expected <- c('test', 'val', 'train', 'test', 'train')
	cv$._result <- cv$subset(cv$._input_data, cv$._input_val, cv$._input_test)[["set"]]
	stopifnot(identical(cv$._result, cv$._expected))



