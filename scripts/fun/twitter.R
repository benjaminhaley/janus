###############################################################################
#
# Twitter
#
# Using R to access twitter feeds
#
# inspired by 
#   http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r
#
# documentation at
#	http://cran.r-project.org/web/packages/twitteR/index.html
#
# bmh Dec 2011
# 
###############################################################################

# Install the package
source('../util/package.R') 
package$load(c("twitteR", "ggplot2", "reshape"))

# Get today's trends
getTrends('daily')

# Search
chicago = "41.89801378,-87.623762,30mi"
searchTwitter("machine learning", n=5, since="2000-12-30", until="2020-12-10", geocode=chicago )

