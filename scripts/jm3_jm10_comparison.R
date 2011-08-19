# Purpose and Signature
#
	Description <- "
		Add variables of interest and filter data to compare
		Peromyscus vs Mus Musculus JM 3 vs JM 10.  \n
		Written by Will Liu & Ben Haley in March of 2011.
		Updated in June of 2011 by Ben Haley.
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/jm3_jm10_comparison.R  \n
	"

# Configuration
# 
	# Load Data and Utility Scripts
	#
		source ("load_janus_data.R")
		source ("util/translate_janus_codes.R")
		key <- key$new()
	#
	# Define Filters / Filter
	#
		data$micro_pathologies <- NULL
		data$micros_grouped <- NULL
		data$pathologies_key <- NULL
		data <- lapply( data, subset, !is.na( data$demographics$necroscopy_date ) )
		data <- lapply( data, subset, ( (data$demographics$expt == 3 | data$demographics$expt == 10) ) )
		data <- lapply( data, subset, ( data$demographics$sex == "M" ) )
		data <- lapply( data, subset, ( data$demographics$tmt != "SL" ) ) # no JM-10 matching dosage
		data <- lapply( data, subset, ( data$demographics$tmt != "SH" ) ) # no JM-10 matching dosage 
		data <- lapply( data, subset, ( data$demographics$tmt != "VW" ) ) # fracitonated
		data <- lapply( data, subset, ( data$demographics$tmt != "VV" ) ) # fracitonated
		data <- lapply( data, subset, ( data$demographics$tmt != "S8" | data$demographics$radn != "G" ) ) # 
		data <- lapply( data, subset, ( data$macro_pathologies$CDU_L == FALSE  ) ) # remove undetermined cause of death
		data <- lapply( data, subset, ( data$macro_pathologies$FIT_L == FALSE  ) ) # remove fighting deaths (peromyscus loves to fight)
		data <- lapply( data, subset, ( data$macro_pathologies$FIT_N == FALSE  ) ) # remove non-lethal fighting, these probably caused early death too  
	#
	# Define the Model factors
	#
		lethal.tumor.codes <- c( "LR_T_L", "TVAS_L", "TCON_L", "TADN_L", "TGA__L", "TLIV_L", "TKID_L", "TGI__L", "TADR_L", "TPIT_L", "TTHY_L", "TTA__L", "TMAM_L", "TUTE_L", "TOVE_L", "TEPO_L", "TWEP_L" )
		lethal.non.tumor.codes <- c( "CDU_L", "MHEP_L", "MPNU_L", "MCVD_L", "MCRD_L", "MOCY_L", "O_NT_L" )
		nonlethal.tumor.codes <- c( "LR_T_N", "TVAS_N", "TCON_N", "TADN_N", "TGA__N", "TLIV_N", "TKID_N", "TGI__N", "TADR_N", "TPIT_N", "TTHY_N", "TTA__N", "TMAM_N", "TUTE_N", "TOVE_N", "TEPO_N", "TWEP_N" )
		
		sum.columns.by.name <- function( column.names )
		{
			total <- c( 0 )
			for( n in column.names )
			{
				column.number <- which( names( data$macros_grouped ) == n )
				vector <- as.integer( data$macros_grouped[[column.number]] )
				total <- vector + total
			}
			return( total )
		}

		data$lethal.tumor.sum <- sum.columns.by.name( lethal.tumor.codes )
		data$lethal.non.tumor.sum <- sum.columns.by.name( lethal.non.tumor.codes )
		data$nonlethal.tumor.sum <-  sum.columns.by.name( nonlethal.tumor.codes )
		data$all.tumor.sum <- data$lethal.tumor.sum + data$nonlethal.tumor.sum
		data$multiple.tumors <- ( as.logical( data$lethal.tumor.sum ) & data$all.tumor.sum > 1 )
		data$lifespan.from.first.dose <- data$demographics$age - data$demographics$first_irrad


		# Lump the radiation treatments
		#
			gamma <- data$demographics$radn == "G"
			neutron <- data$demographics$radn == "N"
			control <- data$demographics$radn == "C"

			gamma.ranges <- c( -1, 10, 100, 150, 200, 400 )
			neutron.ranges <- c( -1, 10, 20, 40, 100, 200 )
			dose.labels <- c("zero","low", "medium","high", "very high" )
			dose <- data$demographics$total_dose

			data$dose.description <- factor( levels=dose.labels )
			data$dose.description[ gamma ] <- cut( dose[ gamma ], gamma.ranges, dose.labels )
			data$dose.description[ neutron ] <- cut( dose[ neutron ], neutron.ranges, dose.labels )
			data$dose.description[ control ] <- "zero"

			data$loworhigh <- factor( levels=c( "high", "low", "zero" ) )
			data$loworhigh[ data$dose.description %in% c( "high", "very high" ) ] <- "high"
			data$loworhigh[ data$dose.description %in% c( "low", "medium" ) ] <- "low"
			data$loworhigh[ data$dose.description %in% c( "zero" ) ] <- "zero"

		# Lets do lifespan stuff
		#
			mus.m <- data$demographics$species == "Mus musculus"
			per.l <- data$demographics$species == "Peromyscus leucopus"
			control <- data$demographics$radn == "C"
			mus.median <- median( data$lifespan.from.first.dose[ mus.m & control ] )
			per.median <- median( data$lifespan.from.first.dose[ per.l & control ] )
			
			data$percent.lifespan[ mus.m ] <- data$lifespan.from.first.dose[ mus.m ] / 1356
			data$percent.lifespan[ per.l ] <- data$lifespan.from.first.dose[ per.l ] / 2230 

			data$medians[ mus.m ] <- data$lifespan.from.first.dose[ mus.m ] / mus.median
			data$medians[ per.l ] <- data$lifespan.from.first.dose[ per.l ] / per.median

		# Lets merge everything into one nice data frame for analysis
		#
			# We need to rename some columns to avoid
			# conflicts and ambiguity
			#
				macros_grouped <- data$macros_grouped
				names( macros_grouped ) <- paste( "macros_grouped", names( data$macros_grouped ), sep="." )
				macro_pathologies <- data$macro_pathologies
				names( macro_pathologies ) <- paste( "macros", names( data$macro_pathologies ), sep="." )

			total.data <- data.frame( 
				data$demographics, 
				macros_grouped,
				macro_pathologies,
				data$lethal.tumor.sum,
				data$lethal.non.tumor.sum,
				data$multiple.tumors,
				data$dose.description,
				data$loworhigh,
				data$percent.lifespan,
				data$nonlethal.tumor.sum,
				data$all.tumor.sum,
				data$lifespan.from.first.dose,
				data$medians
			)

		#
		# Dependent Variables
		#

		macros.of.interest <- c(
			"ANE_L", "EDA_L", "FIT_L", "HRG_L", "INF_L", "NEC_L",
			"CDU_L", "ANE_N", "EDA_N", "FIT_N", "HRG_N", "INF_N",
			"NEC_N", "CAT_N"
		)

		dependent.variables <-
		c( 
			paste( "macros_grouped", names( data$macros_grouped ), sep="."),
			paste( "macros", macros.of.interest, sep=".")
		)

		#
		# Independent Models
		#
			independent.models <- 
				c(
					"species + relevel( data.dose.description, 'zero' ) + data.medians" 
				)

# Prepare some formulas
# 	
	formula.combinations <- expand.grid( independent.models, dependent.variables )
	full.formula <- paste( formula.combinations[[2]], formula.combinations[[1]], sep=" ~ ")

# Define the datasets of interest
#
	gamma <-  total.data$radn %in% c( "G", "C" )
	neutron <- total.data$radn %in% c( "N", "C" )

# Define Functions to get information on the pathology
#
	get.pathology.name <- function( dependent.variable )
	{
		split <- strsplit( dependent.variable, ".", TRUE )[[1]]	
		pathology.name <- split[2]
		pathology.name
	}

	#Test
	#
		result <- get.pathology.name( "macros_grouped.O_NT_N" )
		expected <- "O_NT_N"
		stopifnot( all.equal( expected, result ) )		

	get.pathology.category <- function (dependent.variable)
	{
		category <- NULL
		map <- c (
			macros_grouped = "group",
			macros         = "macro"
		)

		split <- strsplit (dependent.variable, ".", TRUE) [[1]]	
		category.name <- split [1]
		valid.category <- category.name %in% names (map)

		if (valid.category)
		{
			category <- map [category.name]
		}
		else
		{
			warning( paste( dependent.variable, "could not be matched" ) )
		}

		category
	}

	#Test
	#
		dependent.variables <- c (
			"macros_grouped.GOOD",
			"macros.GOOD",
			"nonsense.BAD"
		)
		# to supress warnings (we expect warnings)
		options( warn=-1 )
		result <- lapply (dependent.variables, get.pathology.category)
		# to re-enable warnings
		options( warn = 0 )
		expected <- list ("group", "macro", NULL)
		stopifnot( all.equal( expected, result, check.attributes=FALSE ) )		

# Define the Outcome Data
#
glm.result <- function( formula, data )
{
	# Run the model
	#
		formula <- as.formula(formula)
		dependent.variable <- all.vars( formula )[1]
		result <- glm( formula, family="binomial", data=data )

	# calculate the output
	#
		summary   <- summary( result )
		estimate  <- summary$coefficients[, 1]
		std.error <- summary$coefficients[, 2]
		p.value   <- summary$coefficients[, 4]
		odds.ratios <- exp( estimate )
		point.975.z.score <- 1.96
		lci <- exp( estimate - point.975.z.score*std.error )
		uci <- exp( estimate + point.975.z.score*std.error )
		pathology.name <- get.pathology.name( dependent.variable )
		janus.code <- key$get.janus.code( pathology.name )
		category <- get.pathology.category( dependent.variable )
		pathology.description <- key$get.description( janus.code, category )
		pathology.lethality <- key$get.pathology.lethality( pathology.name )
	
	#return a report
	#		
		report <- as.vector(t(cbind( 
			odds.ratios, lci, uci, p.value
		)))
		name.combinations <- expand.grid( 
			c( "odds", "lci", "uci", "p.value" ),
			names( odds.ratios )
		)
		names <- paste( name.combinations [, 1], name.combinations [, 2], sep=" - " )
		names( report ) <- names
		report <- c( 
			code=dependent.variable, 
			description=pathology.description, 
			lethality=pathology.lethality, 
			report 
		)
		return( report )
}

	# Test
	#
		formula <- "macros_grouped.O_NT_N ~ species + relevel( data.dose.description, 'zero' ) + data.medians"
		result <- glm.result( formula , total.data[ gamma, ] )
		result.head <- head( result, 3 )
		expected.head <- c( "macros_grouped.O_NT_N", NA, "non lethal" )
		stopifnot( all.equal( result.head, expected.head, check.attributes=FALSE, tolerance=0.001 ) )

# Run some models
#
	gamma.result <- lapply(full.formula, glm.result, total.data[ gamma, ])
	neutron.result <- lapply(full.formula, glm.result, total.data[ neutron, ])



# A nice little report
# 
	gamma.report <- data.frame( gamma.result, row.names=names(gamma.result[[1]]) )
	neutron.report <- data.frame( neutron.result, row.names=names(neutron.result[[1]]) )

	write.csv( gamma.report , "../analysis/jm10_vs_jm3_gamma.csv" )
	write.csv( neutron.report , "../analysis/jm10_vs_jm3_neutron.csv" )
	

# Produce a table of grouped pathological outcomes 
# by treatment group and species
#

	outcomes <- list(
		ANE_L=data$macro_pathologies$ANE_L,
		EDA_L=data$macro_pathologies$EDA_L,
		FIT_L=data$macro_pathologies$FIT_L,
		HRG_L=data$macro_pathologies$HRG_L,
		INF_L=data$macro_pathologies$INF_L,
		NEC_L=data$macro_pathologies$NEC_L,
		CDU_L=data$macro_pathologies$CDU_L,
		ANE_N=data$macro_pathologies$ANE_N,
		EDA_N=data$macro_pathologies$EDA_N,
		FIT_N=data$macro_pathologies$FIT_N,
		HRG_N=data$macro_pathologies$HRG_N,
		INF_N=data$macro_pathologies$INF_N,
		NEC_N=data$macro_pathologies$NEC_N,
		CAT_N=data$macro_pathologies$CAT_N,
		lethal.tumors=data$lethal.tumor.sum,
		non.lethal.tumors=data$lethal.non.tumor.sum,
		multiple.tumors=data$multiple.tumors
	)
	outcomes <- cbind( data$macros_grouped, outcomes )
	
	#
	# Remove unuesed factors so they don't clutter the table
	#
	data$demographics$species <- factor( data$demographics$species )
	
	report <- lapply( 
		outcomes, 
		table, 
		species=data$demographics$species, 
		dose=data$dose.description 
	)
	write.csv( report , "../analysis/jm3_jm10_outcome_counts.csv" )



