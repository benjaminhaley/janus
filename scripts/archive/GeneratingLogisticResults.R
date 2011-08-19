# Purpose and Signature
#
	Description <- "
		To verify the control pathologies between janus studies.  \n
		Written by Ben Haley in February of 2011.
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/janus_pathological_validity.R  \n
	"
# A custom regression report
#
logistic.report <- NULL
logistic.report$column.headers <- "\"\",\"Estimate\",\"Std. Error\",\"z value\",\"Pr(>|z|)\",\"odds ratio\",\"dependent variable\""
logistic.report$report <- function( glm, dependent.variable )
{
	# Retrieve the results we want
	#
	# 	Odds ratios are equal to e^coefficient because ln( p / 1-p ) = c1*x1 + c2*x2...
	#	see http://www.ats.ucla.edu/stat/r/dae/logit.htm
	#
		oddsRatios = exp( result$coefficients )
		summary = summary( result )

	# Formulate the results into a pretty report
	#
		coefficients = summary$coefficients
		oddsMatrix = matrix( oddsRatios , dimnames = list( names(oddsRatios), c("odds ratio") ) )
		report = cbind( coefficients , oddsMatrix, dependent.variable )
	
	return( report )
}



# Load the macro pathology csv

	macroPathologyCsv <- "C:/Users/Will/Desktop/Dropbox/LabStuff/4-5-11 Revisions/Gamma.csv"
	macros <- read.csv( macroPathologyCsv, header = TRUE )

# Setup the output File
#
	# Set the location
	#
	outputCsv <- "C:/Users/Will/Desktop/Dropbox/LabStuff/4-5-11 Revisions/GammaResults.csv"
	
	# Initialize the file
	#
	write( Description , outputCsv )
	write( logistic.report$column.headers , outputCsv , append = TRUE )
	

# Ready the independent macro variables
#

	macros$gender <- macros$gender
	macros$study <- as.factor( macros$experiment.number )
	macros$pathologist <- as.factor(macros$necroscopy.proctor.initials)
	macros$treatment <- as.factor(macros$treatment.code)

# Create models for each Macro pathology
#
	for( dependentColumn in c( 19:82 ) )
	{
		dependentColumn = dependentColumn
		dependentName = names( macros )[[dependentColumn]]
		formula <- 
			macros[[dependentColumn]] ~ 

			macros$species + 
			relevel( macros$loworhigh, 'zero' ) + macros$medians

		result <- glm( formula, family=binomial )

		report <- logistic.report$report( result , dependentName )
		
		# Write the output to file
		#
		write.table( 
			report, 
			file=outputCsv, 
			append = TRUE, 
			sep = ",", 
			row.names = TRUE, 
			col.names = FALSE 
		)
	}