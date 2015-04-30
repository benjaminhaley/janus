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
library(xtable)
library(pander)
library(lme4)
library(coxme)

# pretty_table
#
# An html table, nicely formatted
pretty_table <- function(df) 
  print(xtable(df), 
      type="html",
      include.rownames = FALSE,
      html.table.attributes = getOption("xtable.html.table.attributes", "border=0"))

# table0
# Just like table, but show's NA's by default
# e.g.
#   table(NA) == '< table of extent 0 >'
#   table0(NA) == c('<NA>'=1)
table0 <- function(...) table(..., useNA='ifany')

# n_unique
# Numbe of unique items in a data frame.
n_unique <- function(...) length(unique(paste(...))) 


# ggsave_for_ppt
# wrapper around ggsave to save plots with a good default size
# and resolution for powerpoint presentations.
#
# Warnings are supressed to avoid gratuitious output that should
# be seen when the graph was first created.
ggsave_for_ppt <- function(...) {
  suppressWarnings(ggsave(..., 
                          dpi=100, 
                          width=9.36, 
                          height=7.02, 
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
# 95% confidence interval we can subtract 1.92 from the log of this
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
  minimum_acceptable_likelihood <- exp(log(max(likelihood)) - qchisq(1 - p, 1)/2)
  lowest_acceptable_x <- min(x[likelihood > minimum_acceptable_likelihood])
  highest_acceptable_x <- max(x[likelihood > minimum_acceptable_likelihood])
  
  # Show
  print(paste0(round(most_likely_x, 1),' (',
               round(lowest_acceptable_x, 1),', ',
               round(highest_acceptable_x, 1), ')'))
  
  c(low=lowest_acceptable_x, 
    middle=most_likely_x, 
    high=highest_acceptable_x)
}

# Generate cdf
# Given a set of likelihood return the corresponding cumulative 
# density function
cdf <- function(likelihoods) {
  cdf <- (1 - pchisq(log(max(likelihoods)) - log(likelihoods), 1)) / 2
  
  # After 0.5 (the most likely) cdf values should count up to
  # 1 instead of counting back down to 0
  greater_than_most_likely <- which.max(cdf):length(cdf)
  cdf[greater_than_most_likely] <- 1 - cdf[greater_than_most_likely]
  
  cdf
}

# Find a p value
# Based on a value, v, in a set x with given likelihoods,
# Find the p value of v.
# This is highly related to the previous function
significance <- function(v, x, likelihood) {
  p <- (1 - pchisq(log(max(likelihood)) - log(likelihood[which.closest(v, x)]), 1))/2
  
  p
}

# Compare pdfs

# p greater
# Given two cumulative probability density curves estimate the p value
# associated with the hypothesis
# a > b
#
# Tests
# p_greater(c(0.000, 0.001), c(0.999, 1)) 
# # ~ 0
# p_greater(pnorm((-100:100)/100), pnorm((-100:100)/100)) 
# # ~ 0.50
# p_greater(pnorm((-100:100)/100, sd=2), pnorm((-100:100)/100, sd=1)) 
# # ~ 0.50
# p_greater(pnorm((-1000:1000)/100, mean=1), pnorm((-1000:1000)/100, mean=0)) 
# # ~ 1 - sum(rnorm(1000000, mean=1) >= rnorm(1000000, mean=0)) / 1000000
# # ~ 0.24
p_greater <- function(cdf_a, cdf_b, n_samples=100000) {
  a_sample <- which.closest(runif(n_samples), cdf_a)
  b_sample <- which.closest(runif(n_samples), cdf_b)
  a_greater <- a_sample > b_sample
  a_equal <- a_sample == b_sample
  1 - (sum(a_greater) + sum(a_equal)/2) / n_samples
}

# p different
# Like p greater but a two sided test
p_different <- function(cdf_a, cdf_b, n_samples=100000) {
  p_a_greater <- p_greater(cdf_a, cdf_b, n_samples)
  p_b_greater <- p_greater(cdf_b, cdf_a, n_samples)  
  min(p_a_greater, p_b_greater) / 2
}



# Find the closest value in a list
# e.g.
#   closest(1.7, c(1, 2, 3)) == 2
#
# Default to the first item in the list in the case of ties
#   closest(1.5, c(1, 2, 3)) == 1
which.closest <- function(values, list) {
  sapply(values, function(value) which.min(abs(list - value)))
}
closest <- function(value, list) {
  list[which.closest(value, list)]
}


# Pluralize
# pluralize('Mouse') == 'Mice'
pluralize <- function(x) {
  c(Mouse='Mice',
    Rat='Rats',
    Dog='Dogs',
    Peromyscus='Peromyscus')[x]
}

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
                            o_range = seq(-2, 20, by=0.02),
                            likelihood_function=logLik,
                            other_functions = list()){
  r <- ldply(o_range, function(o){
    m <- modeling_function(data, o)
    l = likelihood_function(m)
    result <- data.frame(o, l)
    
    for(fn in names(other_functions)) {
      result[[fn]] = other_functions[[fn]](
        data=data, 
        o=o, 
        model=m, 
        likelihood=l
      )
    }
    
    result
  })
  
  delta = o_range[2] - o_range[1]
  r$l <- normalize_likelihood(r$l, delta)
  
  r
}
mse <- function(sm) { 
  mse <- mean(sm$residuals^2)
  return(mse)
}

# Write csv with good defaults
write.csv0 <- function(...) write.csv(..., row.names=FALSE)


# only
# Retrieve the one unique value of x,
# raise an error if x has more than one value
only <- function(x) { 
  u <- unique(x)
  n <- length(u)
  if(n > 1) stop(paste("More than one value", paste(head(u), collapse=', ')))
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

# get cluster number
get_cluster_number <- function(x) {
  as.numeric(extract('^[0-9.]*', x))
}



# has clusters
# Determine if a dataset contains multiple clusters
has_clusters <- function(data) "cluster" %in% names(data) & length(unique(data$cluster)) > 1

# lq model
# The basic linear quadratic model used in the BEIR VII report
#
# Notably this seems to conflict with the model used to test various
# theta values.  Specifcially this seems to be weighted by n, the
# number of mice in each group, wheras that anlaysis was not weighted,
# it seems!
lq_model <- function(data){
  
  formula <- I(1/age) ~ dose + I(dose^2 / (fractions))
  
  # Define a formula that accomidates multiple clusters
  if(has_clusters(data)) {
    formula <- I(1/age) ~ dose*cluster + I(dose^2 / (fractions))*cluster}
  
  glm(
    formula,
    data=data,
    weights=n
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
    data=data
  )
}

# fixed o model
# A weighted version of the above.  It looks like they didn't use this
# in the original longevity analysis, but they mention applying it to 
# cancer data and it appears that they did.  So I think it was there 
# intent to apply it universally.
weighted_fixed_o_model <- function(data, o){
  
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
    method='DL'
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

# get negative dose responses
#
# Models should be limited to positive dose
# responses if they are to fit to the linear
# quadratic assumptoins.
#
# This list can be used to eliminate positive
# responses from the design matrix
model_meta_get_negative_dose_responses <- function(data, o, ...) {
  m <- model_meta_fixed_o(data, o, ...)
  negative_dose_responses <- names(coefficients(m)[coefficients(m) < 0 & grepl('dose', names(coefficients(m)))])
  negative_dose_responses
}

# Model meta fixed o
# As before, but with a fixed curvature.
model_meta_fixed_o <- function(data, 
                               o,
                               negative_dose_responses = c(),
                               yi = 1/data$age,
                               vi = (1/data$age - 1/(data$age + data$sd))^2){
  
  # Determine the formula
  # Stratify by cluster, or not
  formula <- ~ I(dose + o*dose^2 / (fractions))
  if(has_clusters(data)) {
    formula <- ~ I(dose + o*dose^2 / (fractions)) * cluster - I(dose + o*dose^2 / (fractions))
  }
  
  # Determine mods
  # We determine the model matrix ourselves
  # in case any mods are to be excluded.
  mods <- model.matrix(formula, data)
  mods <- mods[,!colnames(mods) %in% negative_dose_responses]
    
  m <- rma(
    yi,
    vi,
    mods = mods,
    data = data,
    method='DL',
    control=list(maxiter=1000, threshold=10e-12)
  )
  
  m
}
predict_meta_fixed_o <- function(m, newdata, clustered=FALSE, negative_dose_responses=c()){

  # Determine the formula
  # Stratify by cluster, or not
  formula <- ~ I(dose + o*dose^2 / (fractions))
  if(clustered) {
    formula <- ~ I(dose + o*dose^2 / (fractions)) * cluster - I(dose + o*dose^2 / (fractions))  
  }
 
  # Determine mods
  # We determine the model matrix ourselves
  # in case any mods are to be excluded.
  newmods <- model.matrix(formula, newdata)
  newmods <- newmods[,!colnames(newmods) %in% negative_dose_responses]
  
  # Remove the intercept
  # because rma automatically adds it, and I can't get it to stop
  # confusing
  newmods <- newmods[,2:ncol(newmods)]
  predict(m, newmods=newmods)$pred
}
# Full model (meta, no negative coefficients)
#
# A simple wrapper function
# First find the negative coefficients
# then remove them and return a model without
# negatives.
model_meta_fixed_o_non_negative <- function(data, o, ...) {
  negative_dose_responses <- model_meta_get_negative_dose_responses(data, o, ...)
  model_meta_fixed_o(data, o, negative_dose_responses, ...)
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
