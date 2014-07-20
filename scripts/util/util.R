#############################################################################
#
# Utility functions
#
# General functions and libraries that will be used in a variety of code.
#
# Last update:
# June 2014

# LIBRARIES
library(plyr)
library(dplyr)
library(ggplot2)
library(survival)
library(directlabels)
library(metafor)
library(reshape2)


# table0
# Just like table, but show's NA's by default
# e.g.
#   table(NA) == '< table of extent 0 >'
#   table0(NA) == c('<NA>'=1)
table0 <- function(...) table(..., useNA='ifany')

# ggsave_for_ppt
# wrapper around ggsave to save plots with a good default size
# and resolution for powerpoint presentations.
#
# Warnings are supressed to avoid gratuitious output that should
# be seen when the graph was first created.
ggsave_for_ppt <- function(...) {
  suppressWarnings(ggsave(..., 
                          dpi=100, 
                          width=10.24, 
                          height=7.68, 
                          units='in'))
}


# Normalize likelihood
# Given a set of log likelihood values, l, and a distance between
# their measures, delta, ensure that the integral of the area
# under the profile curve will equal delta.
normalize_likelihood <- function(log_likelihood, delta){
  l <- log_likelihood
  l <- l - max(l)
  l <- exp(l)
  l <- l / sum(l)
  l <- l / delta
  
  l       
}


# Get a confidence interval from profile likelihood data.
#
# The method is simple, but counter-intuitive, so I will take some
# time to explain how it works and why it makes sense.
#
# To find the most probably value for the data we simply find the
# maximum likelihood and corresponding data... easy.  To find the
# 95% confidence interval we can subtract 1.96 from the log of this
# likelihood maximum then find the data points which the most 
# extreme values for o that have log likelihoods above this value 
# [1].
#
# I know this method works because it produces similar outcomes to
# BEIR VII, but why does it work?  I originally thought that a value
# that had a 20x higher likelihood than another was simply 20x more
# likely to occur and therefore we had to integrate across all
# likelihood values and then find the area that spans 95% just as
# you would do with a probability density curve.
#
# First it is important to know that likelihood is not the likelihood
# of the parameter, instead it is the likelihood of observing the
# data given the parameter [2].
#
# Also realize that there is a thing called a likelihood ratio test
# which compares how likely it is that one model describes a set of
# data better than another model [3].  The reasoning behind our 
# confidence interval determination is that we are essentially
# comparing the most likely model to each other model one at a time
# and determining which we would reject in a head to head comparison.
# Then we keep all of the models which have a likelihood higher than
# rejection.
#
# But why this approach?  How can we be confident that our parameter
# will lie within this range 95% of the time?  I still do not 
# understand this part of the problem.
#
# [1]: http://people.upei.ca/hstryhn/stryhn208.pdf
# [2]: http://stats.stackexchange.com/questions/31238/what-is-the-reason-that-a-likelihood-function-is-not-a-pdf
# [3]: https://en.wikipedia.org/wiki/Likelihood-ratio_test

confidence_interval <- function(x, likelihood, p=0.05) {
  most_likely_x <- x[which.max(likelihood)]
  minimum_acceptable_likelihood <- exp(log(max(likelihood)) - qnorm(1 - p/2))
  lowest_acceptable_x <- min(x[likelihood > minimum_acceptable_likelihood])
  highest_acceptable_x <- max(x[likelihood > minimum_acceptable_likelihood])
  
  # Round
  # to prettify
  most_likely_x <- round(most_likely_x, 1)
  lowest_acceptable_x <- round(lowest_acceptable_x, 1)
  highest_acceptable_x <- round(highest_acceptable_x, 1)
  
  print(paste0(most_likely_x,' (',
               lowest_acceptable_x,', ',
               highest_acceptable_x, ')'))
  c(lowest_acceptable_x, most_likely_x, highest_acceptable_x)
}


# Pluralize
# pluralize('Mouse') == 'Mice'
pluralize <- function(x) {
  c(Mouse='Mice',
    Rat='Rats',
    Dog='Dogs',
    Peromyscus='Peromyscus')[x]
}

# Save for ppt
# A size and resolution that fits well in a powerpoint presentation
ggsave_for_ppt <- function(...) suppressWarnings(ggsave(..., 
                                                        dpi=100, 
                                                        width=10.24, 
                                                        height=7.68, 
                                                        units='in'))

# Combine all unique values of x into a comma seperated list
# list_unique(c(2, 1, 1) == '1, 2'
list_unique <- function(x) paste(sort(unique(x)), sep=', ', collapse=', ')

# Lookup a value in mapping
# Convert x to a chraracter because these are required for vector keys
# convert(c(T), c('TRUE'='cool', 'FALSE'='not cool')) == 'cool'
convert <- function(x, map) map[as.character(x)]

# Get the likelihood of a range of o values
get_likelihoods <- function(data, 
                            modeling_function, 
                            o_range = seq(-2, 6, by=0.01),
                            likelihood_function=logLik){
  r <- ldply(o_range, function(o){
    m <- modeling_function(data, o)
    l = logLik(m)
    
    data.frame(o, l)
  })
  
  delta = o_range[2] - o_range[1]
  r$l <- normalize_likelihood(r$l, delta)
  
  r
}

# Write csv with good defaults
write.csv0 <- function(...) write.csv(..., row.names=FALSE)


# only
# Retrieve the one unique value of x,
# raise an error if x has more than one value
only <- function(x) { 
  u <- unique(x)
  n <- length(u)
  if(n > 1) stop("More than one value")
  u
}

# extract
# like sub, but retrive the matching substring
# extract('[0-9]*', '123 cool') == "123"
extract <- function(pattern, x) regmatches(x, regexpr(pattern, x))

# order levels by number
# Sometimes the name of a factor starts with a number and it should
# be ordered by the value of that number, rather than alphabetically.
# This is for those times.
# order_levels_by_number(factor(c('2 is before', '10')))
order_levels_by_number <- function(x) {
  u <- unique(as.character(x))
  number <- as.numeric(extract('^[0-9.]*', u))
  x <- factor(x, levels=u[order(number)])
  x
}

# has clusters
# Determine if a dataset contains multiple clusters
has_clusters <- function(data) "cluster" %in% names(data) & length(unique(data$cluster)) > 1

# lq model
# The basic linear quadratic model used in the BEIR VII report
lq_model <- function(data){
  
  formula <- I(1/age) ~ dose + I(dose^2 / (fractions))
  
  # Define a formula that accomidates multiple clusters
  if(has_clusters(data)) {
    formula <- I(1/age) ~ dose*cluster + I(dose^2 / (fractions))*cluster}
  
  glm(
    formula,
    data=data,
    weights=1/sd^2
  )
}

# fixed o model
# As above, the linear quadratic model used in the BEIR VII report
# But with a fixed curvature, o, so that we can test the likelihood
# of a particular curvature
fixed_o_model <- function(data, o){

  formula <- I(1/age) ~ I(dose + o*dose^2 / (fractions))
  
  # Define a formula that accomidates multiple clusters
  if(has_clusters(data)) {
    formula <- I(1/age) ~ I(dose + o*dose^2 / (fractions))*cluster}
  
  # Build the model
  glm(
    formula,
    data=data,
    weights=1/sd^2
  )
}

# Model meta
# An update to the BEIR VII model that accounts for random effects
model_meta <- function(data){
  
  formula <- ~ dose + I(dose^2 / (fractions))

  # Define a formula that accomidates multiple clusters
  if(has_clusters(data)) {
    formula <- ~ dose * cluster + I(dose^2 / (fractions)) * cluster}
  
  rma(
    yi,
    vi,
    mods = model.matrix(formula, data=data),
    data = data,
    method='ML'
  )
}
predict_meta <- function(m, newdata, clustered=FALSE){
  
  # Determine the formula
  # Stratify by cluster, or not
  formula <- ~ dose + I(dose^2 / (fractions))  
  if(clustered) {
    formula <- ~ dose * cluster + I(dose^2 / (fractions)) * cluster
  }
  
  newmods <- model.matrix(formula, data=newdata)
  # Remove the intercept
  # because rma automatically adds it, and I can't get it to stop
  # confusing
  newmods <- newmods[,2:ncol(newmods)]
  predict(m, newmods=newmods)$pred
}

# Model meta fixed o
# As before, but with a fixed curvature.
model_meta_fixed_o <- function(data, o){
  
  # Determine the formula
  # Stratify by cluster, or not
  formula <- ~ I(dose + o*dose^2 / (fractions))
  if(has_clusters(data)) {
    formula <- ~ I(dose + o*dose^2 / (fractions)) * cluster}
  
  rma(
    1/age,
    (1/age - 1/(age + sd))^2,
    mods = formula,
    data = data,
    method='ML'
  )
}
predict_meta_fixed_o <- function(m, newdata, clustered=FALSE){

  # Determine the formula
  # Stratify by cluster, or not
  formula <- ~ I(dose + o*dose^2 / (fractions))
  if(clustered) {
    formula <- ~ I(dose + o*dose^2 / (fractions)) * cluster
  }
 
  newmods <- model.matrix(formula, data=newdata)
  # Remove the intercept
  # because rma automatically adds it, and I can't get it to stop
  # confusing
  newmods <- newmods[,2:ncol(newmods)]
  predict(m, newmods=newmods)$pred
}


# Fake data to predict across
# This allows us to fit two lines across a limited
# data range for graphing
get_data_to_predict <- function(clusters=''){
  to_predict <- expand.grid(
    fractions = c(1, Inf),
    dose = seq(0, 1.5, 0.1),
    cluster = order_levels_by_number(unique(clusters))
  )
  to_predict$type <- 'A'
  to_predict$type[to_predict$fractions > 1] <- 'C'
  to_predict
}
