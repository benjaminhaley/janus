# We want a file that can help us build formulas from sets of
# outcomes and possible interacting parameters.
#
# For example it should be easy to produce formulas
#
#   y1 = x1 + x2 + I(x1^2) + x1*x2 + I(x2^2)
#   y2 = x1 + x2 + I(x1^2) + x1*x2 + I(x2^2)
#
# starting from vectors c(y1, y2), c(x1, x2)
#
# Basic usage:
#
#     source('scripts/util/f.builder.R')
#     ys = c("y1", "y2")
#     xs = c("x1", "x2")
#     f.builder$get_formula(ys, xs, outer.interaction=TRUE, self.interaction=TRUE)
#
#     output:
#         "y1 ~ x1 + x2 + I(x1*x1) + I(x2*x2) + x1*x2" 
#         "y2 ~ x1 + x2 + I(x1*x1) + I(x2*x2) + x1*x2"
#
# bmh Nov 2011


# Our namespace
f.builder <- list()



# Interactions between partners (not self interactions)
f.builder$get_outer_interactions <- function(parameter_vector){
	combo_matrix <- t(combn(parameter_vector, 2))
	interactions <- paste(combo_matrix[,1], combo_matrix[,2], sep="*")
	return(interactions)
}
# Test 
	f.builder$._input <- c("x1", "x2")
	f.builder$._expected <- c("x1*x2")
	f.builder$._result <- f.builder$get_outer_interactions(f.builder$._input)
	stopifnot(identical(f.builder$._result, f.builder$._expected))

f.builder$get_self_interactions <- function(parameter_vector){
	interactions <- paste("I(", parameter_vector, "*", parameter_vector, ")", sep="")
	interactions	
}
# Test 
	f.builder$._input <- c("x1", "x2")
	f.builder$._expected <- c("I(x1*x1)", "I(x2*x2)")
	f.builder$._result <- f.builder$get_self_interactions(f.builder$._input)
	stopifnot(identical(f.builder$._result, f.builder$._expected))


# Take a vector of ys and a right model
# and create all the output formula
f.builder$get_all_formula <- function(ys, right_side){
	all_formula <- paste(ys, right_side, sep=" ~ ")
	return(all_formula)
}
# Test 
	f.builder$._input_ys <- c("y1", "y2")
	f.builder$._input_right <- "x1 + x2"
	f.builder$._expected <- c("y1 ~ x1 + x2", "y2 ~ x1 + x2") 
	f.builder$._result <- f.builder$get_all_formula(f.builder$._input_ys, f.builder$._input_right)
	stopifnot(identical(f.builder$._result, f.builder$._expected))


# Take a vector parameters and return the right
# side of an equation
f.builder$get_right_from_parameters <- function(parameter_vector){
	right <- paste(parameter_vector, collapse=" + " )
	return(right)
}

# Test 
f.builder$._input <- c("a", "b", "c")
f.builder$._expected <- "a + b + c"
f.builder$._result <- f.builder$get_right_from_parameters(f.builder$._input)
stopifnot(identical(f.builder$._result, f.builder$._expected))

# A quick way to get a series of formulas
f.builder$get_formula <- function(ys, xs, outer.interactions=FALSE, self.interactions=FALSE){
	parameters <- xs
	if(self.interactions){
		interactions <- f.builder$get_self_interactions(xs)
		parameters <- c(parameters, interactions)
	}
	if(outer.interactions){
		interactions <- f.builder$get_outer_interactions(xs)
		parameters <- c(parameters, interactions)
	}
	right_side <- f.builder$get_right_from_parameters(parameters)
	formula <- f.builder$get_all_formula(ys, right_side)
	return(formula)
}
# Test 
f.builder$._input_xs <- c("x1", "x2")
f.builder$._input_ys <- c("y1", "y2")
f.builder$._expected <- c(
						   "y1 ~ x1 + x2 + I(x1*x1) + I(x2*x2) + x1*x2",
						   "y2 ~ x1 + x2 + I(x1*x1) + I(x2*x2) + x1*x2"
						)
f.builder$._result <- f.builder$get_formula(f.builder$._input_ys, f.builder$._input_xs, TRUE, TRUE)
stopifnot(identical(f.builder$._result, f.builder$._expected))

