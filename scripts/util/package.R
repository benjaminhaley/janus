############################################################################### 
#
# Scripts should be able to check for packages and 
# download them if they are not found locally.
# This script will support that.
#
# bmh Sept 2011
#
# Usage 
#   source("scripts/util/package.R")
#   dependencies <- "RCurl"
#   package$load(dependencies)
#
############################################################################### 

package <- list()

# Here we list all the trusted sources of repos
#
package$.__REPOS <- c(
						"http://cran.r-project.org",
						"http://www.omegahat.org/R"
					 )

package$load <- function( dependencies ){
	installed <- .packages( all=TRUE )
	missing <- dependencies[ ! dependencies %in% installed ]
	any_missing <- length(missing) > 0
	if(any_missing){
		apply( 
			as.matrix(missing), 
			1, 
			install.packages, 
			repos=package$.__REPOS, 
			dependencies=TRUE 
		 )
	}
	apply( 
		as.matrix(dependencies), 
		1, 
		library, 
		character.only=TRUE 
	 )
	return("Success!")
}


