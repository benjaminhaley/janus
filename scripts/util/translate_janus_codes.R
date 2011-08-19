# Purpose and Signature
#
	Description <- "
		This is a set of utility functions to translate between pathology \n
		codes, descriptions, and other codes. \
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/load_janus_data.R  \n
	"

# Class Configuration
# 	Declare the names of data from the loaded file
#
	key <- list()
	key$code.names <- 
	c(
		janus = "micro_code",
		description = "description",
		category = "observation_category"
	)
	key$categories <- 
	c(
		micro <- "micro",
		macro <- "macro",
		grouped <- "grouped"
	)

# Be sure the data is correctly loaded with the proper data model
#
	key$new <- function( key.data = data$pathologies_key )
	{
		names.present <- key$code.names %in% names( key.data )
		if( all( names.present ) & any( names.present ) )
		{
			key$data <- key.data
		}
		else
		{
			warning( "Data could not be loaded because code names were not found in data" )
		}
		key
	}

# Some helper functions to break apart janus codes
#
	key$get.pathology.lethality <- function( pathology.name )
	{
		split <- strsplit( pathology.name, "_", TRUE )[[1]]
		lethality.symbol <- split[length(split)]
		if( lethality.symbol == "N" ){ lethality <- "non lethal" }
		else if( lethality.symbol == "L" ){ lethality <- "lethal" }
		else { stop( paste( "No lethality symbol was found for", pathology.name ) ) }
		lethality
	}

	# Test
	#
		result <- key$get.pathology.lethality( "O_NT_N" )
		expected <- "non lethal"
		stopifnot( all.equal( expected, result ) )

key$get.janus.code <- function( pathology.code )
{
	janus.code <- NA
	valid.n.char <- 6
	valid.tails <- c( "_L", "_N" )
	tail <- substr( pathology.code, nchar(pathology.code)-1, nchar(pathology.code) )
	nchar <- nchar( pathology.code )
	
	is.valid.n.char <- nchar == valid.n.char
	is.valid.tail <- tail %in% valid.tails

	if (is.valid.n.char & is.valid.tail)
	{
		janus.code <- substr( pathology.code, 1, 4 )
	}
	else
	{
		warning ( paste (pathology.code, "is not a valid pathology code"))
	}
	janus.code
}

	# Test
	#
	# turn off false warnings
	options( warn=-1 )
	codes <- c( "GOOD_L", "BAD_L", "BAD2_2" )
	results <- lapply (codes, key$get.janus.code)
	expected <- list( "GOOD", NA, NA )
	# turn warnings back on
	options( warn = 0 )
	stopifnot( all.equal( results, expected ) )


key$get.record.by.janus.code <- function( janus.code, category )
{
	# Inputs 
	#
	record <- NA
	category.column <- key$code.names["category"]
	categories <- key$data[ , category.column ]
	code.column <- key$code.names["janus"]
	codes <- key$data[ ,code.column ]
	key.data <- key$data

	# Validations
	#
	valid.category <- category %in% categories
	valid.code <- janus.code %in% codes

	
	if( valid.category & valid.code )
	{
		record <- key.data[ codes == janus.code & categories == category, ]
	}
	if( !valid.category )
	{
		warning( paste( "the category", category, "could not be found" ) )
	}
	else if( !valid.code )
	{
		warning( paste( "the code", janus.code, "could not be found" ) )
	}
	else if( nrow( record ) != 1 )
	{
		warning( paste( "the number of matching records is not 1" ) )
	}
	record
}


# Test
#
	key <- key$new()
	janus.code <- "TADN"
	category <- "macro"
	expected <- list( 
		"macro", "1", "TADN", "Lung", "",
		"Neoplasm, uncertain whether benign or malignant",
		"28000", "M80001", as.character(NA), as.character(NA), 
		as.character(NA), "TADN", ",PR_T,EP_T", ",PR_T,EP_T"
	)
	result <- key$get.record.by.janus.code( janus.code, category )
	stopifnot( all.equal( expected, result, check.attributes=FALSE ) )


key$get.description <- function (janus.code, category)
{
	record <- key$get.record.by.janus.code( janus.code, category )
	description <- record["description"][[1]]
	description
}

	# Test
	#
		janus.code <- "TADN"
		category <- "macro"
		result <- key$get.description( janus.code, category )
		expected <- "Neoplasm, uncertain whether benign or malignant"
		stopifnot( all.equal( result, expected ) )

