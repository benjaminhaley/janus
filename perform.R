############################################################################### 
#
# Common cost functions
#
# bmh Marh 2012
#
# Usage 
#   source("scripts/util/cost.R")
#
#   c <- cost$r2(actual, predicted)
#
#   or
#
#   cost$show(actual, predicted, set, cost$r2)
#
############################################################################### 

cost <- list()

cost$r2 <- function(actual, predicted){
	n = length(actual)
	u = mean(actual)
	
	var = sum((actual - u)^2)
	rss = sum((actual - predicted)^2)
	cost= 1 - rss / var
	
	cost
}

# Test 
	set.seed(73)
	cost$._input_actual     <- c(0, 2, 4)
	cost$._input_predicted  <- c(0, 2, 3)
	cost$._expected         <- 7/8
	cost$._result           <- cost$r2(cost$._input_actual, cost$._input_predicted)
	stopifnot(identical(cost$._result, cost$._expected))

# A well formatted report that shows overfit
cost$show <- function(actual, predicted, set, cost.function){
	cost.train <- cost.function(actual[set == 'train'], predicted[set == 'train'])
	cost.val   <- cost.function(actual[set == 'val'], predicted[set == 'val'])
	overfit    <- cost.val - cost.train
	
	# gussy it up
	pretty.cost.val <- format(cost.val, scientific = FALSE, digits = 3)
	pretty.overfit  <- format(overfit, scientific = FALSE, digits = 3)
	report          <- paste("cost", pretty.cost.val, "overfit by", pretty.overfit)
	
	report
}

# Test 
	cost$._input_actual     <- c(0, 2, 3, 6)
	cost$._input_predicted  <- c(0, 2, 3, 5)
	cost$._input_set        <- c('train', 'train', 'val', 'val')
	cost$._expected         <- "cost 0.778 overfit by -0.222"
	cost$._result           <- cost$show(cost$._input_actual, cost$._input_predicted, cost$._input_set, cost$r2)
	stopifnot(identical(cost$._result, cost$._expected))
