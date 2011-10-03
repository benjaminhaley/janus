# Purpose and Signature
#
	Description <- "
		Control Comparison.  \n
		Written by Will Liu & Ben Haley in March of 2011.
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/janus_control_comparison.R  \n
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
		data$micro_pathologies <- NULL
		data$micros_grouped <- NULL
		data <- lapply( data, subset, !is.na( data$demographics$necroscopy_date ) )
		data <- lapply( data, subset, ( data$demographics$expt != 10 ) )
		data <- lapply( data, subset, ( data$demographics$expt != 14 ) ) #dropped because injected with placebo
		data <- lapply( data, subset, ( data$demographics$radn == "C" ) )
	#
	# Define the Model factors
	#
		#
		# Create new output variables
		#
		data$demographics$lifespan.post.irradiation <- data$demographics$age - data$demographics$first_irrad
		data$demographics$time_min <- as.factor( data$demographics$time_min )
		#
		# Dependent Variables
		#
		dependent.variables <-
		c( 
			"data$demographics$lifespan.post.irradiation"
		) 
		#
		# Independent Models
		#
		independent.models <- 
		c(
			"data$demographics$sex + data$demographics$expt", 
			"data$demographics$sex + data$demographics$tmt", 
			"data$demographics$sex + data$demographics$time_min", 
			"data$demographics$sex + data$demographics$necroscopy_date", 
			"data$demographics$sex + data$demographics$necrosopy_proctor" 
		)
	

# Prepare some formulas
# 	
	formula.combinations <- expand.grid( independent.models, dependent.variables )
	full.formula <- paste( formula.combinations[[2]], formula.combinations[[1]], sep=" ~ ")

# Define the Outcome Data
#
lm.result <- function( formula )
{
	result <- lm( as.formula(formula) )
	summary <- summary( result )
	return( summary$coefficients )
}


# Run some models
#
	result <- mapply( lm.result, full.formula )


# A nice little report
# 
	report <- rbind( result[[1]], result[[2]], result[[3]], result[[4]], result[[5]]  )
	data.final <- cbind( data[[1]], data[[2]], data[[3]] )
	write.csv( report, "control_comparison.csv" )
	write.csv( report, "control_comparison_data.csv" )
