# Purpose and Signature
#
	Description <- "
		To explore the systematic variation in lifespan in the control animals.  \n
		Written by Ben Haley in May of 2011.
	"

# Configuration
# 
	# Report location
	#
		data.summary.file <- "../analysis/bmh_janus_control_summary.txt"

	#
	# Load Data
	#
		source( "load_janus_data.R" )

# Filter the control animals
#
	control.data <- data$demographics[ 
		data$demographics$radn == "C" & data$demographics$species == "Mus musculus", 
	]

# Plot average lifespan by experiment
#
	means <- aggregate( control.data$age, by=list( control.data$expt ), FUN=mean )
	sds <- aggregate( control.data$age, by=list( control.data$expt ), FUN=sd )
	means <- aggregate( control.data$age, by=list( control.data$expt ), FUN=
			function( ages )
			{
				t.test( control.data$age, ages )$p.value
			}
		   )
	n <- aggregate( control.data$age, by=list( control.data$expt ), FUN=length )

# Now we are going to make a little sample data set to show off the effects of 
# extraneous and confouding variables in an analysis
#
	independent <- rnorm( 1000 )
	extraneous <- rnorm( 1000 )*10
	noise <- rnorm( 1000 )*10
	confounding <- independent * rnorm( 1000 )*10
	dependent <- independent + extraneous + noise + confounding

	lm.simple <- lm( dependent ~ independent )
	lm.extraneous <- lm( dependent ~ independent + extraneous )
	lm.confounding <- lm( dependent ~ independent + confounding )
	lm.full <- lm( dependent ~ independent + confounding + extraneous )


	summary ( lm.simple )
	summary ( lm.extraneous )
	summary ( lm.confounding )
	summary( lm.full )


# Lets look at total dose vs lifespan with and without extraneous factors
#
	mus.data <- data$demographics[ data$demographics$species == "Mus musculus", ]
	jiggle <- runif( nrow( mus.data ), 0, 0.1 )
	age <- mus.data$age
	dose <- mus.data$total_dose * jiggle
	plot( age, dose, cex=0.01 )

# Lets explore regression
# extraneous and confouding variables in an analysis
#
	library(scatterplot3d)


	a <- rnorm( 100 )
	b <- rnorm( 100 )
	noise <- rnorm( 100 )*1
	dependent <- a + b + noise

	plot( dependent, a )
	plot( a+noise, dependent )

	write.csv( data.frame( a, b, noise ), file="C:/Users/Ben/Desktop/temp.csv" )