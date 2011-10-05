# Scripts should be able to check for packages and load them if they are not
# found.  This script will do exactly that.
# bmh Sept 2011

# Usage 
#   source("../util/package.R")
#   dependencies <- "RCurl"
#   package$load(dependencies)

package <- list()
package$.__REPOS <- "http://cran.r-project.org"

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


