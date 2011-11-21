###############################################################################
#
# US Census tracts
#
# A test to try the census data loading package
#
# inspired by 
#				http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r
#
# documentation at 
#					http://cran.r-project.org/web/packages/UScensus2000tract/UScensus2000tract.pdf
#
# bmh Nov 2011
# 
###############################################################################

# Install the package
#
source('../util/package.R') 
package$load(c("UScensus2000"))

# Load data about any state
#
data(new_york.tract)

# See population demographics by tract (about 5000 people/tract)
#
head(new_york.tract@data)

# There is also tract geospacial data that can be used for mapping
# but I can't play with it for lack of X11 support.  I really need
# to fix this one day.
