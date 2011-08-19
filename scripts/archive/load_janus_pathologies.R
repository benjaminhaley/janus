# Purpose and Signature
#
	Description <- "
		To load the data from the janus studies.  \n
		Written by Ben Haley in March of 2011. \n
		Script at C:/Users/Ben/Documents/My Dropbox/ben's bigish files/woloben/Tissue_Stats/scripts/load_janus_pathologies.R  \n
	"

# Configuration Variables
# 
	# Locatation of the pathology records online
	#
	macroUri <- "http://janus.northwestern.edu/janus2/data/macro_pathologies.zip"
	macroFileName = "macro_pathologies.csv"
	microUri <- "http://janus.northwestern.edu/janus2/data/micro_pathologies.zip"
	microFileName = "micro_pathologies.csv"
	#
	# Characteristics of the online records
	#
	demographicColumnCount <- 13 
	minimumPathologyCount <- 10
	# 
	# Give ourselves the max amount of memory to play with
	# 
	memory.limit( 4095 )


# Download and extract pathologies
#
	download.file( macroUri, "temp_macros.zip" )
	download.file( microUri, "temp_micros.zip" )
	unzip( "temp_macros.zip" )
	unzip( "temp_micros.zip" )
	unlink( "temp_macros.zip" )
	unlink( "temp_micros.zip" )


# Prepare data frames from CSVs
#
	macros <- read.csv( macroFileName , header = TRUE )
	micros <- read.csv( microFileName , header = TRUE )
	unlink( macroFileName )
	unlink( microFileName )

# Clean and Merge the pathologies
#
#	1. Remove Entries without necroscopy dates
#	2. Remove Pathologies with total counts less than the minimum
#
	validMacros <- subset( macros , necroscopy.date != "" )
	validMicros <- subset( micros , necroscopy.date != "" ) #there is a weird bug in micros where there is a single blank row inserted at row 14915.  I tried to track it down but could not.  	
	LastMicroColumn <- ncol( validMicros )
      validMicrosWoDemographics <- validMicros[ as.numeric(demographicColumnCount + 1):LastMicroColumn  ]
	allPathologies <- cbind( validMacros , validMicrosWoDemographics )

# Convert types where needed
#
	allPathologies$necroscopy.date <- as.Date( allPathologies$necroscopy.date, format="%m/%d/%Y" )
	allPathologies$experiment.number <- as.factor( allPathologies$experiment.number )

# Free some memory
#
	rm( validMacros )
	rm( validMicros )
	rm( validMicrosWoDemographics )
	rm( macros )
	rm( micros )
	gc()

# Filter out un-interesting patholgies
# 
	lastPathologyColumn <- ncol( allPathologies )
	firstPathologyColumn <- demographicColumnCount + 1
	columnCounts <- colSums( !is.na( allPathologies[firstPathologyColumn : lastPathologyColumn ]) )
	noCounts <- columnCounts[ columnCounts < minimumPathologyCount ]
	pathologies <- allPathologies[ -match(names(noCounts), names(allPathologies) ) ]

# Free some memory
#
	rm( allPathologies )
	gc()

