#############################################################################
#
# Utility functions
#
# General functions without dependencies that will be used in a variety of
# scripts.
#
# Last update:
# March 2014

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