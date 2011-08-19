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
	janusDataUri <- "C:/Users/Ben/Downloads/"
	tableNames <- 
	c( 
		"macro_pathologies",
		"micro_pathologies"
	)
	zipExtension <- ".zip"
	fileExtension <- ".csv"

# Download, extract, and create dataframes
#
	uris = paste( janusDataUri, tableNames, zipExtension, sep="" )
	fileNames = paste( tableNames, fileExtension, sep="" )
	mapply( unzip, uris )
	data <- mapply( read.csv, fileNames, header = TRUE, row.names = 1 )
	mapply( unlink, fileNames )


# Print table specs to help confirm download
#
	message <- paste( length( data ), "tables downloaded with the following dimensions: " )
	print( message )
	dimensions <- lapply( data, dim )
	print( dimensions )

