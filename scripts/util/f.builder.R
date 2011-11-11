# We want a file that can help us build formulas from sets of
# outcomes and possible interacting parameters.
#
# For example it should be easy to produce formulas
#
#   y1 = x1 + x2 + x1*x1 + x1*x2 + x2*x2
#   y2 = x1 + x2 + x1*x1 + x1*x2 + x2*x2
#
# starting from vectors c(y1, y2), c(x1, x2)
#
# bmh Nov 2011


# Our namespace
f.builder <- list()



# All possible interactions in a parameter vector 
f.builder$get_self_interactions <- function(parameter_vector){
	combo_matrix <- t(combn(parameter_vector, 2))
	outer_interactions <- paste(combo_matrix[,1], combo_matrix[,2], sep="*")
	self_interactions <- paste(parameter_vector, parameter_vector, sep="*")
	interactions <- c(self_interactions, outer_interactions)
	return(interactions)
}
# Test 
	f.builder$._input <- c("x1", "x2")
	f.builder$._expected <- c("x1*x1", "x2*x2", "x1*x2")
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
f.builder$get_formula <- function(ys, xs, self.interaction=FALSE){
	parameters <- xs
	if(self.interaction){
		interactions <- f.builder$get_self_interactions(xs)
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
						   "y1 ~ x1 + x2 + x1*x1 + x2*x2 + x1*x2",
						   "y2 ~ x1 + x2 + x1*x1 + x2*x2 + x1*x2"
						)
f.builder$._result <- f.builder$get_formula(f.builder$._input_ys, f.builder$._input_xs, TRUE)
stopifnot(identical(f.builder$._result, f.builder$._expected))

