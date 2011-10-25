# Some functions to help us calculate odds ratios
#   
#   odds = probability / (1 - probability)
#   odds_ratio = odds.1 / odds.2
#
# bmh 2011

odds_ratio <- list()

odds_ratio$logit2or <- function(coefficients, control){
	o.r <- exp(coefficients - control)
	return(o.r)
}

# logit2or Test
odds_ratio$._logit_coefficients <- c( 2, -1, 0.5 )
odds_ratio$._control <- 2
odds_ratio$._expected <- c(exp(2), exp(-1), exp(0.5))/exp(2)
odds_ratio$._result <- odds_ratio$logit2or(odds_ratio$._logit_coefficients, odds_ratio$._control)
stopifnot(identical(odds_ratio$._expected,odds_ratio$._result))
