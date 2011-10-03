# Purpose and Signature
#
	Description <- "
		To determine if control pathologies are affected by non-treatment factors.  \n
		Written by Ben Haley in April of 2011.
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/bmh_janus_control_veracity.R  \n
	"

# Configuration
# 
	# Report location
	#
		report.file <- "../analysis/bmh_janus_control_veracity.csv"
	
		data.summary.file <- "../analysis/bmh_janus_control_summary.txt"

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
		data <- lapply( data, subset, ( data$demographics$radn == "C" ) )
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
		data$demographics$total_exposure_time <- data$demographics$fractions * data$demographics$time_min
		data$demographics$date_of_birth <- data$demographics$necroscopy_date - data$demographics$age
		data$demographics$jm2 <- data$demographics$expt == "2"
		data$demographics$jm3 <- data$demographics$expt == "3"
		data$demographics$jm4 <- data$demographics$expt == "4"
		data$demographics$jm7 <- data$demographics$expt == "7"
		data$demographics$jm8 <- data$demographics$expt == "8"
		data$demographics$jm9 <- data$demographics$expt == "9"
		data$demographics$jm10 <- data$demographics$expt == "10"
		data$demographics$jm11 <- data$demographics$expt == "11"
		data$demographics$jm12 <- data$demographics$expt == "12"
		data$demographics$jm13 <- data$demographics$expt == "13"
		data$demographics$jm14 <- data$demographics$expt == "14"
		data$demographics$pathologist_none <- data$demographics$necrosopy_proctor == "" 
		data$demographics$pathologist_AL <- data$demographics$necrosopy_proctor == "AL" 
		data$demographics$pathologist_AS <- data$demographics$necrosopy_proctor == "AS" 
		data$demographics$pathologist_BF <- data$demographics$necrosopy_proctor == "BF" 
		data$demographics$pathologist_BJ <- data$demographics$necrosopy_proctor == "BJ" 
		data$demographics$pathologist_CZ <- data$demographics$necrosopy_proctor == "CZ" 
		data$demographics$pathologist_ES <- data$demographics$necrosopy_proctor == "ES"
		data$demographics$pathologist_FG <- data$demographics$necrosopy_proctor == "FG" 
		data$demographics$pathologist_JH <- data$demographics$necrosopy_proctor == "JH" 
		data$demographics$pathologist_JP <- data$demographics$necrosopy_proctor == "JP" 
		data$demographics$pathologist_JR <- data$demographics$necrosopy_proctor == "JR" 
		data$demographics$pathologist_KA <- data$demographics$necrosopy_proctor == "KA" 
		data$demographics$pathologist_MF <- data$demographics$necrosopy_proctor == "MF" 
		data$demographics$pathologist_PD <- data$demographics$necrosopy_proctor == "PD"



		independent.table <- "demographics"
		models <- list()
		models$demographic <- 
		c(
			"necroscopy_date", 
			"pathologist_none",
			"pathologist_AL", 
			"pathologist_AS",
			"pathologist_BF",
			"pathologist_BJ",
			"pathologist_CZ",
			"pathologist_ES",
			"pathologist_FG",
			"pathologist_JH",
			"pathologist_JP",
			"pathologist_JR",
			"pathologist_KA",
			"pathologist_MF",
			"pathologist_PD",
			"sex", 
			"jm2", 
			"jm3", 
			"jm4", 
			"jm7", 
			"jm8", 
			"jm9", 
			"jm10", 
			"jm11", 
			"jm12", 
			"jm13", 
			"jm14", 
			"species",
			"total_exposure_time",
			"date_of_birth"
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

# Prepare some formulas
# 	
	#
	# One for each factor, the full model, and any partial models noted by sepperate lists
	#
	independent.formula.null <- "NULL"
	independent.formula.single <- mapply( paste, independent.variables.full.names, sep=" + " )
	independent.formula.full <- paste( unlist( independent.variables.full.names ), collapse=" + " )
	independent.formula <- 
	c(
		independent.formula.null,
		independent.formula.single,
		independent.formula.full
	)
	formula.combinations <- expand.grid( independent.formula, dependent.variables.full.names )
	full.formula <- paste( formula.combinations[[2]], formula.combinations[[1]], sep=" ~ ")


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


# Run some models
#
	result <- mapply( glm.result, full.formula, family="binomial" )

	
# Summarize the data
#
	# independent variables
	#
		input.summary <- list()
		input.summary$gender <- table( data$demographics$sex )
		input.summary$necroscopy_date <- summary( data$demographics$necroscopy_date )
		input.summary$proctor <- table( data$demographics$necrosopy_proctor )
		input.summary$lifespan.days <- summary( data$demographics$age )
		input.summary$species <- table( data$demographics$species )
		input.summary$expertiment <- table( data$demographics$expt )
	#
	# Dependent Variables
		output.summary <- list()
		output.summary$pathologies <- dependent.variables.counts
		names(output.summary$pathologies) <- dependent.variables.full.names

writeLines( 
	c(
		Description,
		paste( "Analysis results can be found in", report.file ),
		"The following is a break down of all janus control mice except those without a necropsy report",
		capture.output( print( input.summary ) ),
		"The following pathological outcomes were watched",
		capture.output( print( output.summary ) )
	),
	sep="\n", 
	data.summary.file	
)


# A nice little report
# 
	report <- matrix( 
		result, 
		ncol=length(dependent.variables.full.names), 
	)

	report <- data.frame( report, row.names=independent.formula )
	names( report ) <- dependent.variables.full.names
	dependent.variables.counts <- mapply( sum.sublist,  dependent.variables.full.names )
	report <- rbind( report, pathology.outcome.count=dependent.variables.counts )	
	
	write.csv( report, report.file, append=TRUE )
