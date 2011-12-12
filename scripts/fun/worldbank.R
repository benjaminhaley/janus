###############################################################################
#
# World Development Indicators (world bank)
#
# A test to try the census data loading package
#
# inspired by 
#   http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r
#
# documentation at 
#   http://cran.r-project.org/web/packages/WDI/index.html
#
# bmh Dec 2011
# 
###############################################################################

# Install the package
source('../util/package.R') 
package$load(c("WDI", "ggplot2", "reshape"))

# list first 10 indicators
WDIsearch('')[1:10,]

# get tractor ownership
d <- WDI(country="all", indicator="AG.AGR.TRAC.NO", start=1950, end=2012, extra=FALSE)

# regional data
r <- c(
	"1A", "1W", "8S", "CN", "BR", "EU", "ID", "IN", "IR", "RU", "US",
	"Z4", "Z7", "ZF", "ZG", "ZJ", "ZQ"
	)

# plot tractors v time v region
ggplot( 
		d[d$iso2c %in% r,], 
		aes(x=year, y=AG.AGR.TRAC.NO, color=country)
	  ) + 
	geom_smooth()


