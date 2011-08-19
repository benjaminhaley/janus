# Purpose and Signature
#
	Description <- "
		To verify Will Lius first analysis in analysis/WLiuManuscriptDraft.doc and analysis/WLiuManuscriptResults.xlsx.  \n
		Written by Ben Haley in March of 2011. \n
		Script at scripts/ben_replicating_wills_first_analysis.R  \n
		Base Directory at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/
		"

# Get the pathology Data from the janus website
# 
	source("C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/load_janus_pathologies.R")


# Filter the data
# 
	#
	# Remove unecessary studies
	#
	experiment <- pathologies$experiment.number
	data <- subset( pathologies, ( experiment == 3 | experiment == 10 ) )
	data$experiment.number <- factor( data$experiment.number )
	# 
	# Remove females
	#
	data <- subset( data, data$gender == "M" )
	data$gender <- factor( data$gender )
	#
	# Eliminate incompatible radiation doses and patterns
	#
		#
		# Gamma
		#
		is.gamma <- data$type.of.radiation.neutron.or.gamma == "G"
		dose <- data$total.dose.in.cGy
		data <- subset( data, ( !is.gamma | dose != 545.7 ) )	
		data$total.dose.in.cGy <- factor( data$total.dose.in.cGy )
		#
		# Neutron
		#
		is.neutron <- data$type.of.radiation.neutron.or.gamma == "N"
		dose <- data$total.dose.in.cGy
		data <- subset( data, ( !is.neutron | ( dose != 226.08 ) ) )
		data$total.dose.in.cGy <- factor( data$total.dose.in.cGy )
		# 
		# Fractionated
		#
		is.control <- data$type.of.radiation.neutron.or.gamma == "C"
		data <- subset( data, ( data$number.of.fractions == 1 | is.control ) )
		

table( data$total.dose.in.cGy, data$experiment.number, data$type.of.radiation.neutron.or.gamma )

	

# Setup independent variables
#
# note:
# 	Create bins for gamma and neutron irradiation as described in the methods
#
	independent <- NULL
	#
	# Add the gamma irradiations as factors
	#

		is.gamma <- data$type.of.radiation.neutron.or.gamma == "G"
		dose <- data$total.dose.in.cGy

		independent$gamma.low 	 <- is.gamma && dose == 90
		independent$gamma.medium <- is.gamma && dose == 143
		independent$gamma.high	 <- is.gamma && dose == 206
		independent$gamma.higest <- is.gamma && dose == 417
	#
	# Add the neutron irradiations as factors	
	#
		is.neutron <- data$type.of.radiation.neutron.or.gamma == "N"
		dose <- data$total.dose.in.cGy

		independent$gamma.low 	 <- is.neutron && dose == 20
		independent$gamma.medium <- is.neutron && dose == 40
		independent$gamma.high	 <- is.neutron && ( dose == 80 | dose == 60 )
		independent$gamma.higest <- is.neutron && ( dose == 120 | dose = 160 )


# Setup dependent variables 
#
# note:
# 	Only lethal pathologies
#	Colapse pathological observations according to table 1
#
	dependent = NULL
	dependent$lymphoma <- 
		data$NON.THYMIC.LYMPHOMA...GENERALIZE...lethal.. |
		data$NON.THYMIC.LYMPHOMA...LOCALIZED...lethal.. |
		data$THYMIC.LYMPHOMA...GENERALIZED...lethal.. |
		data$THYMIC.LYMPHOMA...LOCALIZED...lethal..
	dependent$vascular <- 
		data$VASCULAR...lethal...Neoplasm..uncertain.whether.benign.or.malignant
#
# ... and more
#	Multitumor effects were evaluated similarly, where the number of instances where multiple tumors were observed was regressed on experiment number and dose, and stratified again by dose type. 
#	Others listed in table 1


# These groups were then regressed on indicator variables for experiment number and dose. 
