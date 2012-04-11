############################################################################### 
#
# Common performance functions
#
# bmh Marh 2012
#
# Usage 
#   source("scripts/util/perform.R")
#
#   c <- perform$r2(actual, predicted)
#
#   or
#
#   perform$show(actual, predicted, set, perform$r2)
#
############################################################################### 

perform <- list()

perform$r2 <- function(actual, predicted){
	n       = length(actual)
	u       = mean(actual)
	
	var     = sum((actual - u)^2)
	rss     = sum((actual - predicted)^2)
	perform = 1 - rss / var
	
	perform
}

# Test 
	set.seed(73)
	perform$._input_actual     <- c(0, 2, 4)
	perform$._input_predicted  <- c(0, 2, 3)
	perform$._expected         <- 7/8
	perform$._result           <- perform$r2(perform$._input_actual, perform$._input_predicted)
	stopifnot(identical(perform$._result, perform$._expected))

# A well formatted report that shows overfit
perform$show <- 
function(actual, predicted, set, perform.function){
	perform.train      <- perform.function(actual[set == 'train'], predicted[set == 'train'])
	perform.val        <- perform.function(actual[set == 'val'], predicted[set == 'val'])
	overfit            <- perform.train - perform.val
	
	# gussy it up
	pretty.perform.val <- format(perform.val, scientific = FALSE, digits = 3)
	pretty.overfit     <- format(overfit, scientific = FALSE, digits = 3)
	report             <- paste("performance", pretty.perform.val, "overfit by", pretty.overfit)
	
	report
}

# Test 
	perform$._input_actual     <- c(0, 2, 3, 6)
	perform$._input_predicted  <- c(0, 2, 3, 5)
	perform$._input_set        <- c('train', 'train', 'val', 'val')
	perform$._expected         <- "performance 0.778 overfit by 0.222"
	perform$._result           <- perform$show(
	                                  perform$._input_actual, 
	                                  perform$._input_predicted, 
	                                  perform$._input_set, 
	                                  perform$r2
	                              )
	stopifnot(identical(perform$._result, perform$._expected))
