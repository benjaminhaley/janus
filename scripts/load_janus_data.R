# Purpose and Signature
#
	Description <- "
		To load the data from the janus studies.  \n
		Written by Ben Haley in March of 2011. \n
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/load_janus_data.R  \n
	"

# Configuration Variables
# 
	# 
	# Give ourselves the max amount of memory to play with
	# 
	rm(list = ls(all = TRUE))
	memory.limit( 4095 )
	
	# Locatation of the janus data online
	#
	janusDataUri <- "http://janus.northwestern.edu/janus2/data/"
	tableNames <- 
	c( 
		"macro_pathologies",
		"micro_pathologies",
		"macros_grouped",
		"micros_grouped",
		"demographics",
		"pathologies_key"
	)
	row.name.locations <-
	list(
		1,
		1,
		1,
		1,
		1,
		NULL
	)
	zipExtension <- ".zip"
	fileExtension <- ".csv"

# Download, extract, and create dataframes
#
	uris = paste( janusDataUri, tableNames, zipExtension, sep="" )
	downloadNames = paste( tableNames, zipExtension, sep="" )
	fileNames = paste( tableNames, fileExtension, sep="" )
	mapply( download.file, uris, downloadNames )
	mapply( unzip, downloadNames )
	mapply( unlink, downloadNames )
	data <- mapply( read.csv, fileNames, header = TRUE, skip=1, row.names = row.name.locations )
	names( data ) <- tableNames 
	mapply( unlink, fileNames )



# Convert types where needed
#
	data$demographics$necroscopy_date <- as.Date( data$demographics$necroscopy_date, format="%Y-%m-%d" )
	data$demographics$expt <- as.factor( data$demographics$expt )
	data$macro_pathologies <- data.frame( mapply( is.finite, data$macro_pathologies, SIMPLIFY = FALSE ) )
	data$micro_pathologies <- data.frame( mapply( is.finite, data$micro_pathologies, SIMPLIFY = FALSE ) )
	data$macros_grouped <- data.frame( mapply( is.finite, data$macros_grouped, SIMPLIFY = FALSE ) )
	data$micros_grouped <- data.frame( mapply( is.finite, data$micros_grouped, SIMPLIFY = FALSE ) )
	data$pathologies_key <- data.frame( mapply( as.character, data$pathologies_key ), stringsAsFactors=FALSE )

# Print table specs to help confirm download
#
	message <- paste( length( data ), "tables downloaded with the following dimensions: " )
	print( message )
	dimensions <- lapply( data, dim )
	print( dimensions )

