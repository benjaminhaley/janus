###############################################################################
#
# US Census tracts
#
# A test to try the census data loading package
#
# inspired by 
#   http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r
#
# documentation at 
#   http://cran.r-project.org/web/packages/UScensus2000tract/UScensus2000tract.pdf
#
# bmh Nov 2011
# 
###############################################################################

# Install the package
source('../util/package.R') 
package$load(c("UScensus2000", "ggplot2"))

# load ggplot maps
try_require("maps")

# load data
data(illinois.cdp)

# assemble coordinates and population 
# by census designated place (cdp)
d <- cbind(
	data.frame(coordinates(illinois.cdp)),
	data.frame(illinois.cdp),
	p.not.white=(1 - illinois.cdp$white / illinois.cdp$pop2000)
	)
	
# plot illinois racial breakdown
qplot(
	X1, X2, data=d, 
	geom="point", 
	size=pop2000, 
	alpha=p.not.white
	) + 
	opts(title = expression("Ethnicity by area"))



