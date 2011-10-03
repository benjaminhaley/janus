# Purpose and Signature
#
Description <- "
To load the data from the janus studies.  \n
Written by Ben Haley in March of 2011. \n
janus/scripts/load_janus_data.R
"

# A namespace for data
#
j.data <- list()

# Configuration Variables
# 
# Give ourselves the max amount of memory to play with
# 
rm(list = ls(all = TRUE))
memory.limit( 4095 )

# Locatation of the janus data online
#
	j.data$private.uri <- "http://janus.northwestern.edu/janus2/data/"
	j.data$data.dir <- "../data/"
	j.data$private.table.names <- 
	c( 
			"macro_pathologies",
			"micro_pathologies",
			"macros_grouped",
			"micros_grouped",
			"demographics",
			"pathologies_key"
	 )
	j.data$private.table.header.rows <-
	list(
			1,
			1,
			1,
			1,
			1,
			NULL
		)
	j.data$private.zip.extension <- ".zip"
	j.data$private.file.extension <- ".csv"
	j.data$local.name <- "j.data.serialized"
	j.data$local.when.downloaded.name <- "j.data.when.downloaded.serialized"

# Download, extract, and create dataframes
#
	uris = paste( 
			j.data$private.uri, 
			j.data$private.table.names, 
			j.data$private.zip.extension, 
			sep="" 
			)
	downloadNames = paste( 
			jdata$data.dir, 
			j.data$private.table.names, 
			j.data$private.zip.extension, 
			sep="" 
			)
	fileNames = paste( 
			jdata$data.dir, 
			j.data$private.table.names, 
			j.data$private.file.extension, 
			sep="" 
			)
	mapply( 
			download.file, 
			uris, 
			downloadNames 
		  )
	mapply( 
			unzip, 
			downloadNames 
		  )
	mapply( 
			unlink, 
			downloadNames 
		  )
	data <- mapply( 
			read.csv, 
			fileNames, 
			as.is = TRUE,
			header = TRUE, 
			skip=1, 
			row.names = j.data$private.table.header.rows 
			)
	names( data ) <- j.data$private.table.names 
	mapply( 
		   unlink, 
		   fileNames 
		)

# Save Data locally and add a file modified time
	save(
		data,
		file=j.data$local.path
		)
	save(
		date,
		file=j.data$local.when.downloaded.path
		)
	j.data$local.path <- "../data/j.data.serialized"
	j.data$local.when.downloaded.path <- "../data/j.data.when.downloaded.serialized"



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

