###############################################################################
#
# Zoo trial
#
# recommended as good source code by stack exchange
#
# original q
# 				http://stats.stackexchange.com/questions/5418/first-r-packages-source-code-to-study-in-preparation-for-writing-own-package
# 
# bmh Nov 2011
# 
###############################################################################

# Install the package
#
source('scripts/util/package.R') 
package$load(c("zoo"))

# Load data about any state
#
data(new_york.tract)

# See population demographics by tract (about 5000 people/tract)
#
head(new_york.tract@data)

# There is also tract geospacial data that can be used for mapping
# but I can't play with it for lack of X11 support.  I really need
# to fix this one day.
