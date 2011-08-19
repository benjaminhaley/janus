# Purpose and Signature
#
	Description <- "
		To determine what portion of the pathological outcome is due to radiation.  \n
		Written by Ben Haley in March of 2011.
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/janus_pathological_studies.R  \n
	"

# Configuration
# 
	# Load Data
	#
		source( "load_janus_data.R" )
		source( "util/get.sublist.R" )
	#
	# Define Filters / Filter
	#
		minimum.pathology.count = 100
		data <- lapply( data, subset, !is.na( data$demographics$necroscopy_date ) )
	#
	# Define how thorough our fitting should be (speed vs precision)
	#	*note 1e-1 gets our AIC within +/-1
	#
		model.control <- list(epsilon = 1e-1, maxit = 25, trace = FALSE)
	#
	# Define the Model factors
	#
		#
		# Name of the data object created by load_janus_data.R
		#
		data.store <- "data"
		#
		# Tables which (only) contain dependent variables 
		#
		dependent.tables <-
		c( 
			"macro_pathologies",
			"micro_pathologies",
			"macros_grouped",
			"micros_grouped"
		) 
		#
		# Independent variables
		# 	*note will be run individually, as groups, and all at once
		#
		independent.table <- "demographics"
		models <- list()
		models$demographic <- 
		c(
			"necroscopy_date", 
			"necrosopy_proctor", 
			"sex", 
			"expt", 
			"species"
		) 
		models$radiation <-
		c(
			"total_dose", 
			"dose_rate", 
			"radn"
		)


# Prepare data for analysis
#
	#
	# Define the full name off each dependent/independent variable
	#
		dependent.tables.full.names <- paste( data.store, dependent.tables, sep="$" )
		dependent.variables <- mapply( 
			names, 
			mapply( get.sublist, dependent.tables.full.names ) 
		)
		dependent.variables.full.names <- unlist( 
			mapply( paste, dependent.tables.full.names, dependent.variables, sep="$" ) 
		)
		independent.variables.full.names <- mapply( paste, data.store, independent.table, models, sep="$" )
		names( independent.variables.full.names ) <- names( models )
	# 
	# Filter out dependent Variables that have less than the minimum count
	# 
		sum.sublist <- function( sublist.name )
		{
			sum <- sum( 
				get.sublist( sublist.name ) 
			)
			return( sum )
		}
		dependent.variables.counts <- mapply( sum.sublist,  dependent.variables.full.names )
		dependent.variables.full.names <- dependent.variables.full.names[ dependent.variables.counts > minimum.pathology.count ]
	

# Define the Outcome Data
# 	*note if we try to save the entire outcome for each model we will run out of memory
#
glm.result <- function( formula, family=binomial, control=model.control )
{
	result <- glm( 
		as.formula( formula ), 
		family, 
		control=control 
	)
	return( result$aic )
}



# Prepare some formulas
# 	
	#
	# One for each factor, the full model, and any partial models noted by sepperate lists
	#
	independent.formula.null <- "NULL"
	independent.formula.single <- unlist( independent.variables.full.names )
	independent.formula.full <- paste( unlist( independent.variables.full.names ), collapse=" + " )
	independent.formula.group <- mapply( paste, independent.variables.full.names, collapse=" + " )
	independent.formula <- 
	c(
		independent.formula.null,
		independent.formula.single,
		independent.formula.group,
		independent.formula.full
	)
	formula.combinations <- expand.grid( independent.formula, dependent.variables.full.names )
	full.formula <- paste( formula.combinations[[2]], formula.combinations[[1]], sep=" ~ ")


# Run some models
#
	result <- mapply( glm.result, full.formula, family="binomial" )


# A nice little report
# 
	report <- matrix( 
		result, 
		ncol=length(dependent.variables.full.names), 
	)
	report <- data.frame( report, row.names=independent.formula )
	names( report ) <- dependent.variables.full.names
	
	write.csv( report, "temp.csv" )