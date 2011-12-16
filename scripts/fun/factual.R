###############################################################################
#
# Factual
#
# A test to try the factual data loader.
# I go through several examples because factual is so cool.
#
# inspired by 
#   http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r
#
# documentation at 
#   http://cran.r-project.org/web/packages/factualR/factualR.pdf
#
# bmh Dec 2011
# 
###############################################################################

# Install the package
source('../util/package.R') 
package$load(c("rdatamarket", "ggplot2"))

# need a devoloper key
key <- source('../keys/factual')[[1]]

# Connect to factual
factual <- createFactualConnection(key)

# Skim http://www.factual.com/topics for an interesting table.
# copy the reference from the url
# i.e.
#   http://www.factual.com/t/mGcjnw/NCES_Public_Schools
#   is the NCES_Public_Schools data
#   with reference equal to mGcjnw
NCES.table.ID <- "mGcjnw"

# Search for city names anywhere in the table
# and charter by the charter column
filters <- '{"$search":"chicago","charter":"yes"}'  # weird bug where I can only look for
													# one search term at a time if I am also
													# looking for a column restriction.
													# So:
													#   '{"$search":"chicago","charter":"yes"}'
													#   '{"$search":"chicago,san francisco"}'
													# work, but 
													#   '{"$search":"chicago,san francisco", "charter":"yes"}'
													# does not work.
													


# Get table data
NCES.data <- factualRead( factual , NCES.table.ID, filters = filters, limit=20 )
d <- NCES.data@results
n.results <- NCES.data@resultRows

# Get a sense of the data
head(d)


#################

# Get schools within 5000 meters of 303 e chicago
id <- "7c1lXr"
filters='{"$loc":{"$within":{"$center":[[41.896484, -87.619773],5000]}}}'
data <- factualRead( factual , id, filters = filters, limit=20 )
d <- data@results
n.results <- data@resultRows

# Get a sense of the data
head(d)
