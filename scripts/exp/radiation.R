# ###################################################################
# #
# # Data Inventory
# # 22 Jan 2013
# #
# # I recently downloaded all of the era data.  The purpose of this
# # exercise is to explore the basic values in that dataset and 
# # ensure that they all seam reasonable.

# # Libraries
	# library(ggplot2)

# # Data
	# setwd('~/janus/scripts')
	# data <- read.csv(
		# 'data/era/big.csv', 
		# sep='|', 
		# as.is=TRUE,
		# na.strings = "n/a"
	# )

# # Helpers
	# get_some <- function(col, n=6, d=data){
		# u <- unique(paste(d[,col], d[,'file']))
		# u[sample(1:length(u), n)]
	# }
	# get_counts <- function(x) data.frame(table(x))
	# one_y_per_x <- function(x, y){
		# s <- 'lkjslkdj'
		# p <- unique(paste(x, y, sep=s))
		# u <- t(data.frame(strsplit(p, s)))[,1]
		# length(u) == length(unique(u))
	# }
	# one_y_per_x(x=c(1, 2, 2), y=c(1, 2, 3))		# FALSE
	# one_y_per_x(x=c(1, 2, 2), y=c(1, 2, 2))		# TRUE
	# one_y_per_x(x=c(1, 2, 2), y=c(3, 3, 3))		# TRUE

	# table_ <- function(...) table(..., useNA='ifany')

# # Files
	# ggplot(data=get_counts(data$file),aes(Freq)) + geom_histogram()

	# # We see that many files, ~ 200 only appear a few times
	# # the rest occur up to 15,000 times
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-01-23%20at%2011.49.01%20AM.png

# # Groups ID
	# get_some('Group.ID')   # Look like 1001-5-8 or 2-15-4
	# ggplot(data=get_counts(data$Group.ID),aes(Freq)) + 
		# geom_histogram()

	# # Most groups appear just a few times, though some as often as
	# # 6000 times.  In general the long exposed ones are dumb.

# # Group Name
	# get_some('Group.Name')   # like Nb-0.62DEGy 1,n/a
	# one_y_per_x(x=data$Group.ID, y=data$Group.Name) 	# TRUE
	# one_y_per_x(x=data$Group.Name, y=data$Group.ID) 	# FALSE
	
	# # Group name is not that helpful.  Every id has a unique
	# # name, but some names are repeated and they are difficult
	# # to read.
	
# # Treatment Sequence
	# get_some('Treatment.Sequence')	 # 1
	# table_(data$Treatment.Sequence)  # 1-3, n/a, rarely 4-8
	
# # Treatment Age and Age
	# get_some('Treatment.Age')		# 188, 100, or NA
	# ggplot(data, aes(Treatment.Age)) + 
		# geom_density() + 
		# facet_wrap( ~ Species + Age, scales="free")
	# quantile(data$Treatment.Age, na.rm=TRUE)

	# # Many problems have come to my attention:
	# #
	# # 104 Rat Prenatal exposures at ages greater than 0
	# # from lab_2_study_11.csv
	
	# s <- 
		# data$Species == 'Rat' & 
		# data$Age == 'Prenatal' &
		# data$Treatment.Age > 0
		
	# s <- s & !is.na(s)
	# sum(s)
	# unique(data$file[s])
	
	
	# s <- 
		# data$file == "lab_2_study_11.csv"
		
	# s <- s & !is.na(s)
	# table(data[s,'Treatment.Age'], data[s,'Age'], data[s, 'Sex'])
	
	# # Era report
	# # https://era.bfs.de/studies_details.php?LabId=2&StudyId=11
	
	# # If we line up the table here to the report in the era
	# # we can see that the 'prenatal' label is correct and the
	# # value 92 in the age category is wrong.  It should be an 
	# # 8.
	
	# s <- 
		# data$file == "lab_2_study_11.csv" &
		# data$Age == 'Prenatal'
		
	# data[s, 'Treatment.Age'] <- -8
	
	# # I found other errors too
	
	# s <- 
		# data$file == "lab_2_study_11.csv" &
		# data$Age == 'Young Adult'
		
	# data[s, 'Treatment.Age'] <- 92


	# s <- 
		# data$file == "lab_2_study_11.csv" &
		# data$Age == 'Adult'
		
	# data[s, 'Treatment.Age'] <- 275

	# # Its also worth double checking radiation and group values
	# # to make sure they were not subjected to similar scrambling
	
	# s <- 
		# data$file == "lab_2_study_11.csv"
		
	# s <- s & !is.na(s)
	# table(data[s,'Treatment.Quantity'], data[s,'Age'], data[s, 'Sex'])

        # Adult Preconception Prenatal Young Adult
  # 3        60             0        0           0
  # 5.96      0             0       63          20
  # 14.74     0             0       56           0
  # 33        0             0        0           0

# , ,  = Male

       
        # Adult Preconception Prenatal Young Adult
  # 3         0             0        0         120
  # 5.96      0             0       69           0
  # 14.74     0             0       48           0
  # 33      120             0        0           0
	
	
	
		
	# # Reading about this study I learned about 'radon daugthers'
	# # these refer to the radioactive isotopes that result when 
	# # Radon decays and they are subject to further decay, therefore
	# # quite risky.
	# #
	# # 
	
	
	# # Rat Old Adults at ages less than 200
	# # Mouse Preconception at ages greater than 0
	# # Age defined as 'Need Def 11'
	# # Young Adult dogs older than 1000 days
	# # Old Adult dogs younger than 500 days
	# # Prenatal Dogs over 400 days old
	
	
	
	# ddply(data, .(Group.ID), function(df) length(unique(df$Group.Name)))

# expected <- grepl('[0-9]*-[0-9]*-[0-9]*', data$Group.ID)
# get_some('Group.ID', d=data[!expected,])


# some are broken early with no indicator
# lab_19_study_6.csv


# Lab 2001, Study 2 is empty!  Look for empty studies!

# some studies, like lab 1010 study 8 seem to be missing all info
# looks like this is just how it is!

# Id like to look at treatment age by species


###################################################################
#
# Which Studies?
# 4 Feb 2013
#
# Introduction:
# The data inventory is incomplete, but it has proven to me that
# there are a fair number of inconsistencies in the data.  Its 
# silly to work on cleaning up the entire dataset when I will 
# not even use the entire dataset.
#
# So here I will define those studies which are worth employing.
# I will begin by randomly sampling some studies to read carefully
# about on the ERA and try to find simple rules which sepperate
# the useful ones from thoese that aren't.
#
# Then for each of the useful ones I will need to read its 
# introduction and proof the data within it, so ensure that it
# is sane.

# Libraries
	library(ggplot2)

# Data
	setwd('~/janus/scripts')
	data <- read.csv(
		'data/era/big.csv', 
		sep='|', 
		as.is=TRUE,
		na.strings = "n/a"
	)
	
# Random Sample
	
	# 1003.12
	# This CSV is notably empty.
	
	# 17.7
	# All of these animals are missing individual level data
	
	# 10.2
	# All of these animals are missing individual level data


# Refine
# Clearly we should remove those missing individual level data.
# So lets survey this group to see what we are sacrificing.

	# Helpers
	s <- function(x) x[sample(1:length(x), length(x))]
	
	# Data
	indiv <- data$No.of.individuals.in.study
	data$n <- sub(' .*', '', indiv)
	data$n <- as.numeric(data$n)
	data$in_era <- sub('.*\\(', '', indiv)
	data$in_era <- sub('[^0-9]*$', '', data$in_era)
	data$in_era <- as.numeric(data$in_era)
	
	i = 'No information on individuals available for this study'
	to_remove <- data[
		data$Individual.ID == i |
		data$in_era == 0
	,]

	# General Stats	
	length(unique(to_remove$file))
	length(unique(data$file))        		# 178 of 302 studies
	length(unique(to_remove$Group.ID))
	length(unique(data$Group.ID))  			# 4199 of 6810 groups
	sum(to_remove$n, na.rm=T)			
	nrow(data[data$Individual.ID != i,])	# 280 K of 500 K indiv	

	# Specific examples
	head(s(unique(to_remove$file)))
	
	# 1004.7
	# missing groups 41-54
	
	# 16.3
	# Missing all of a study on pig skin damage
	
	# 2.18
	# Missing all of a study on olfactory bulb apoptosis
	

# Resample
# Now that we have a decent refinement lets continue to look
# for samples which do not fit our purposes.

	i = 'No information on individuals available for this study'
	data <- data[
		data$Individual.ID != i &
		data$in_era != 0,
	]

	head(s(unique(data$file)))
	
	# 5.4
	# An injection study
	
	# 2.2
	# An inhalation study
	
	# 1003.21
	# A janus study!
	
# Refine
# Now we realize we must remove injection and inhalation studies
# This is trickier because there are so many.
# 
# At first I was tempted to manually comb through treatments, but
# this is inefficient because I have to look at the study definition
# for each treatment which sometimes involves looking at the same
# study defition twice.
#
# Also some studies use certain chemical treatments as co-factors
# to external irradiation and I don't want to remove these out of
# hand.


external_studies <- c(
#	'lab_1001_study_1.csv',		# plutonium injection
#	'lab_1001_study_10.csv',	# einsteinum injection
#	'lab_1001_study_11.csv',	# plutonium injection
#	'lab_1001_study_12.csv',	# radium injection
#	'lab_1001_study_13.csv',	# radium injection
#	'lab_1001_study_14.csv',	# radium injection
#	'lab_1001_study_15.csv',	# tests and incomplete studies
#	'lab_1001_study_2.csv',		# radium injection
#	'lab_1001_study_3.csv',		# radium injection
#	'lab_1001_study_4.csv',		# thorium injection
#	'lab_1001_study_5.csv',		# Strontium injection
#	'lab_1001_study_6.csv',		# Americium injection
#	'lab_1001_study_7.csv',		# Californium injection
#	'lab_1001_study_8.csv',		# Californium injection
#	'lab_1001_study_9.csv',		# Plutonium injection
#	'lab_1001_study_99.csv',	# stray dogs
	'lab_1002_study_1.csv',		# external dogs
#	'lab_1002_study_2.csv',		# strontium ingestion
#	'lab_1002_study_3.csv',		# strontium injection
#	'lab_1002_study_4.csv',		# radium injection
#	'lab_1002_study_99.csv',	# undesignated control
#	'lab_1003_study_100.csv',	# radium dial workers
	'lab_1003_study_20.csv',	# Janus
	'lab_1003_study_21.csv',	# Janus
	'lab_1003_study_22.csv',	# Janus
	'lab_1003_study_23.csv',	# Janus
	'lab_1003_study_24.csv',	# Janus
	'lab_1003_study_25.csv',	# Janus
	'lab_1003_study_26.csv',	# Janus
	'lab_1003_study_27.csv',	# Janus
	'lab_1003_study_28.csv',	# Janus
	'lab_1003_study_29.csv',	# Janus
#	'lab_1003_study_3.csv',		# Cerium injection in beagles
	'lab_1003_study_30.csv',	# Janus radioprotectors
#	'lab_1003_study_4.csv',		# Cesium 137 injection
	'lab_1003_study_5.csv',		# beagle external
	'lab_1003_study_51.csv',	# pregnancy exposures
	'lab_1003_study_52.csv',	# exposure until death
#	'lab_1003_study_53.csv',	# lithium and cesium exposure
	'lab_1003_study_54.csv',	# beagle hematalogical studies
	'lab_1003_study_55.csv',	# acute beagle exposures
#	'lab_1003_study_56.csv',	# radio-iodine in beagles
	'lab_1003_study_6.csv',		# beagle continuous exposure
	'lab_1003_study_7.csv',		# beagle duration of life exposure
#	'lab_1003_study_97.csv',	# additional janus mice
#	'lab_1003_study_98.csv',	# technique development
#	'lab_1003_study_99.csv',	# stocks, breeders, etc.
#	'lab_1004_study_1.csv',		# plutonium inhalation
#	'lab_1004_study_2.csv',		# plutonium inhalation
#	'lab_1004_study_3.csv',		# plutonium inhalation
#	'lab_1004_study_4.csv',		# plutonium inhalation
#	'lab_1004_study_5.csv',		# plutonium inhalation
#	'lab_1004_study_7.csv',		# plutonium inhalation
#	'lab_1004_study_99.csv',	# non lifespan
#	'lab_1005_study_1.csv',		# strontium inhalation
#	'lab_1005_study_10.csv',	# cerium inhalation
#	'lab_1005_study_11.csv',	# cerium inhalation
#	'lab_1005_study_12.csv',	# plutonium inhalation
#	'lab_1005_study_13.csv',	# plutonium inhalation
#	'lab_1005_study_14.csv',	# plutonium inhalation
#	'lab_1005_study_15.csv',	# plutonium inhalation
#	'lab_1005_study_16.csv',	# plutonium inhalation
#	'lab_1005_study_17.csv',	# plutonium inhalation
#	'lab_1005_study_18.csv',	# plutonium inhalation
#	'lab_1005_study_19.csv',	# plutonium inhalation
#	'lab_1005_study_2.csv',		# cerium inhalation
#	'lab_1005_study_3.csv',		# yttrium inhation
#	'lab_1005_study_4.csv',		# cerium inhalation
#	'lab_1005_study_41.csv',	# antinide inhalation
#	'lab_1005_study_42.csv',	# cerium inhalation
#	'lab_1005_study_43.csv',	# cesium inhalation
#	'lab_1005_study_44.csv',	# strontium and yttrium inhalation
#	'lab_1005_study_45.csv',	# various radionuclides
#	'lab_1005_study_46.csv',	# decorporation of radionuclides
	'lab_1005_study_47.csv',	# ITRI external radiation studies
#	'lab_1005_study_48.csv',	# Lung absorbtions
#	'lab_1005_study_49.csv',	# chemical aging
#	'lab_1005_study_5.csv',		# cesium injection
#	'lab_1005_study_52.csv',	# primate inhalation
#	'lab_1005_study_6.csv',		# inhalation of yttrium
#	'lab_1005_study_7.csv',		# inhalation of yttrium
#	'lab_1005_study_8.csv',		# inhalation of strontium
#	'lab_1005_study_9.csv',		# inhalation of cerium
#	'lab_1005_study_99.csv',	# breeding beagles
#	'lab_1006_study_1.csv',		# unassigned controls
#	'lab_1006_study_2.csv',		# strontium in monkeys
#	'lab_1006_study_3.csv',		# actinedes in monkeys
	'lab_1007_study_1.csv',		# low dose gamma in mice
	'lab_1007_study_2.csv',		# low dose gamma in mice
	'lab_1007_study_3.csv',		# low dose gamma in mice
#	'lab_1007_study_99.csv',	# undefined studies
	'lab_1008_study_3.csv',		# immature beagle exposure
#	'lab_10_study_5.csv',		# plontium exposure in utero
	'lab_11_study_1.csv',		# external on mice
	'lab_11_study_2.csv',		# external on mice
#	'lab_13_study_2.csv',		# sr-90 injection
#	'lab_18_study_1.csv',		# brachytherapy (implants)
#	'lab_19_study_2.csv',		# plutnoium toxicology
#	'lab_19_study_4.csv',		# plutonium toxicology
#	'lab_19_study_5.csv',		# transuranium toxicology
#	'lab_19_study_6.csv',		# transuranium and external - 0 ind.
#	'lab_19_study_7.csv',		# plutonium and hexachlorobutadiene
#	'lab_1_study_6.csv',		# rats radon
#	'lab_2001_study_8.csv',		# mice plutonium injections
#	'lab_2001_study_9.csv',		# rat inhaled plutonium
#	'lab_23_study_1.csv',		# rats plutonium inhalation
	'lab_2_study_1.csv',		# control rats from sprague + wistar
	'lab_2_study_10.csv',		# rat gamma
	'lab_2_study_11.csv',		# rats gamma
	'lab_2_study_12.csv',		# rats neutron
	'lab_2_study_13.csv',		# rats neutron
	'lab_2_study_14.csv',		# rats gamma and neutron
#	'lab_2_study_15.csv',		# rats chemical treatments
#	'lab_2_study_2.csv',		# rats radon inhalation
#	'lab_2_study_22.csv',		# rats radon inhalation
#	'lab_2_study_3.csv',		# rats radon and smoking
#	'lab_2_study_4.csv',		# rats radon inhalation
#	'lab_2_study_5.csv',		# rats radon inhalation
#	'lab_2_study_6.csv',		# rats radon inhalation
#	'lab_2_study_7.csv',		# rat cesium inhalation or injection
#	'lab_2_study_8.csv',		# rat actinides inhalation
	'lab_3_study_1.csv',		# mice xrays
	'lab_3_study_2.csv',		# mice xray/neutron
	'lab_3_study_3.csv',		# mice xray/neutron
	'lab_3_study_4.csv',		# mice xray/neutron
	'lab_3_study_5.csv',		# mice xray/neutron
	'lab_3_study_6.csv',		# high/low LET radiation
#	'lab_4_study_1.csv',		# Thorium injection
#	'lab_4_study_4.csv',		# zircontrast agent
#	'lab_5_study_1.csv',		# radium injection
#	'lab_5_study_2.csv',		# radium/serium injection
#	'lab_5_study_3.csv',		# Lu/Np injection
#	'lab_5_study_4.csv',		# Lu/Th injection
#	'lab_5_study_5.csv',		# Thorium/Radon injection
#	'lab_5_study_6.csv',		# Thorium injection
#	'lab_5_study_7.csv',		# radionuclide injection
#	'lab_5_study_8.csv',		# Thorium injection
#	'lab_6_study_1.csv',		# radionuclide injections
#	'lab_7_study_1.csv',		# Radium injection
#	'lab_7_study_2.csv',		# Radium injection
#	'lab_7_study_3.csv',		# Radium injection
#	'lab_7_study_4.csv',		# Radium injection
#	'lab_7_study_5.csv',		# Plutonium injection
#	'lab_7_study_6.csv',		# Plutonium injection
#	'lab_7_study_7.csv',		# Thorium injection
#	'lab_8_study_1.csv',		# Radionuclide injection
#	'lab_9_study_1.csv',		# Zinc/Americium injections
#	'lab_9_study_2.csv',		# Radium contamination
#	'lab_9_study_3.csv',		# Americium injection
	'lab_9_study_4.csv',		# mice with xrays and protectors
	'lab_9_study_5.csv',		# mice with gamma/neutron
	'lab_9_study_6.csv',		# gamma / neutron mice
	'lab_9_study_7.csv',		# xray/neutron exposures
	'lab_9_study_8.csv',		# xrays and chemicals
	'lab_9_study_9.csv'			# mice xrays/chemicals
)
	
	# Filter for external studies
	subset <- data$file %in% external_studies
	to_remove <- data[!subset,]
	data <- data[subset,]
	
	# Quantify the loss
	nrow(data); nrow(to_remove)		# Removed 96K of 230K individual
	length(unique(to_remove$file))
	length(unique(data$file))		# Removed 105 of 148 studies
	length(unique(to_remove$Group.ID))
	length(unique(data$Group.ID))	# Removed 1081 of 1530 groups

	
# Check Studies
# I now have a reasonable list of 43 studies.  Its time that
# I read about them a little more carefully to check that they
# are valid and to see what I am working with.

c(
'lab_1002_study_1.csv', 'Lifespan study of 382 female dogs at ITEH exposed to xrays across a range of fractionations and intervals',
'lab_1003_study_20.csv','Lifespan study of 11590 mice at ANL exposed to gamma and neutrons testing the effects of widely spaced small fractions vs accute exposures',
'lab_1003_study_21.csv','Lifespan study of 3275 mice at ANL exposed to 20 minutes of gamma or neutron exposures of varying doses',
'lab_1003_study_22.csv','Lifespan study of 8270 mice at ANL exposed once a week to gamma or neutron irradiation',
'lab_1003_study_23.csv','Lifespan study of 1145 mice exposed almost continuously for 23 or 59 weeks to gamma or neutron irradiation',
'lab_1003_study_24.csv','Lifespan study of 2735 mice exposed once a week for 60 weeks to gamma or neutron irradiation',
'lab_1003_study_25.csv','Lifespan study of 1880 mice exposed once a week for the duration of their lives to gamma or neutron irradiation',
'lab_1003_study_26.csv','Lifespan study of 5450 mice exposed once or more to cGy quantities of gamma or neutron irradiation'
'lab_1003_study_27.csv','Lifespan study of 2645 mice or white footed feild mice exposed once or more fractions of gamma or neutron irradiation',
'lab_1003_study_28.csv','Lifespan study of 600 mice or exposed to various protractions of neutron irradiation',
'lab_1003_study_29.csv','Lifespan study of 7895 mice exposed once a week to gamma and neutron radiation at extremely low dose rates',
'lab_1003_study_30.csv','Lifespan study of 4000 mice exposed to gamma and neutron radiation after receiving saline or radioprotector injections',
'lab_1003_study_5.csv','Lifespan study of 276 beagle dogs exposed to gamma  radiation continuously until death at various dose rates, definitions lacking',
'lab_1003_study_51.csv','Lifespan study of 298 beagle dogs exposed to gamma radiation in utero, definitions lacking',
'lab_1003_study_52.csv','Lifespan study of 144 beagle dogs exposed to gamma radiation, definitions lacking',
'lab_1003_study_54.csv','Lifespan study of 131 beagle dogs exposed to gamma radiation, definitions lacking',
'lab_1003_study_55.csv','Lifespan study of 149 beagle dogs exposed to gamma radiation, definitions lacking',
'lab_1003_study_6.csv','Lifespan study of 392 beagle dogs exposed to gamma radiation until death, definitions lacking',
'lab_1003_study_7.csv','Lifespan study of 27 beagle dogs exposed to gamma radiation until death, definitions lacking'
'lab_1005_study_47.csv','Lifespan studies of 209 beagle dogs conduted at ITRI, definitions lacking',
'lab_1007_study_1.csv','Lifespan study of 4728 mice from two strains exposed once to varying total doses of gamma irradiation',
'lab_1007_study_2.csv','Lifespan study of 2506 mice from two strains exposed once to varying total doses of gamma irradiation',
'lab_1007_study_3.csv','Lifespan study of 17139 mice, mostly female, exposed once to varying total doses of gamma irradiation',
'lab_1008_study_3.csv','Lifespan study of 1680 beagle dogs exposed to gamma irradiation at various stages in utero'
'lab_11_study_1.csv','Lifespan study of 5387 rats of several strains exposed to xrays or neutrons, sometimes with ovarectomies or estrogen treatments',
'lab_11_study_2.csv','Lifespan study of 6562 rats exposed to xrays, gamma rays, and neutrons sometimes with hormone treatments across several strains',
'lab_2_study_1.csv','2632 Sprague-Dawley or Wistar rats used as controls for studies 2.02-2.16'
'lab_2_study_10.csv','Life span studies of 543 Sprague-Dawley rats exposed to gamma irradiation at a variety of dose rates and total doses in order to determine the ddref'
'lab_2_study_11.csv','Lifespan study of 1507 Sprague-Dawley rats exposed to gamma irradiation at a variety of ages and doses including low dose rates.'
'lab_2_study_12.csv','Lifespan study of 2916 rats exposed to a variety of neutron sources, alpha particles and xrays with some chemical co-carcinogens'
'lab_2_study_13.csv','Neutron exposures combined with several carcinogens'
'lab_2_study_14.csv','Neutron or gamma ray exposure combined with several carcinogen treatments.'
'lab_3_study_1.csv','Lifespan study of 1745 mice exposed to xray and neutron irradiation to develop RBE estimates.'
'lab_3_study_2.csv','Lifespan study of 2714 mice exposed to single and fractoinated exposures of gamma rays or fission neutrons in order to estimate the relative biological effectiveness of various dose rates'
'lab_3_study_3.csv','Lifespan study of 296 mice exposed to a single xray exposure at varying total doses'
'lab_3_study_4.csv','Lifespan study of 2110 male mice exposed to xrays or fission neutrons at a variety of total doses'
'lab_3_study_5.csv','Lifespan study of 2221 mice exposed to neutrons or xrays at various ages'
'lab_3_study_6.csv','Lifespan study of 1398 mice exposed to xrays or fission neutrons'
'lab_9_study_4.csv','Lifespan study of 4205 mice exposed to xrays and treated with various radio-protectors'
'lab_9_study_5.csv','Lifespan study of 2908 mice exposed to gamma and neutron irradiation at a variety of total doses'
'lab_9_study_6.csv','Lifespan study of 4469 mice exposed to gamma or neutron irradiation'
'lab_9_study_7.csv','Lifespan study of 1261 mice exposed to xrays or neutrons at various ages',
'lab_9_study_8.csv','Lifespan studies of 1035 male mice exposed to xrays and various carbontetrachloride compounds'
)	

# Save Results
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external.rds', ascii = T, compress = F)
	data <- readRDS('../data/external.rds')



# So I've gone through all the studies in my data set and I am 
# quite confident that all of them involve external irradiation
# 
# The only gotcha I saw is that some include chemical treatments
# in addition to the external exposures.  My initial inclination
# is to simply drop groups with chemical exposure because they
# will be difficult to cross validate against, I am not interested
# in chemical outcomes, and they represent a relatively small 
# portion of the data (less than 10% I think)
#
# Now the current filter is in a file called external.rds.  I 
# may need to clean it further, but its good to have a break 
# point and so far most of the work has just been selecting 
# studies, so it seems like an appropriate name/chance to
# start new with no dependencies.


###################################################################
#
# Data Checking - Groups are what?
# 7 Feb 2013
#
# Introduction:
# Now that I have a refined set of studies, its time to do some
# more data checking.  In this section I will see if the number
# of animals per group reported by the era matches with what
# is reported in the study descriptions.
#
#
	# Libraries
	library(plyr)
	
	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external.rds')
	groups <- data$Group.ID
	file <- data$file
	
	# Helpers
	clean <- function(s) gsub('[^0-9A-Za-z,._ -]', '', s)
	truncate <- function(s) substr(s, 1, 6)
	
	# Clean for printing
	data <- data.frame(llply(data, clean))
	data <- data.frame(llply(data, truncate))
	data <- data.frame(llply(data, as.character), stringsAsFactors=F)
	data$Group.ID <- groups
	data$file <- file

	# Print Treatments
	temp <- ddply(data, .(Group.ID), function(df){
		. <- df[1,]
		c(
			file=.$file,
			n=nrow(df),
			age=.$Treatment.Age,
			type=.$Treatment.Type,
			dose=.$Treatment.Quantity,
			unit=.$Treatment.Unit,
			application=.$Treatment.Application
		)
	})
	temp <- ddply(temp, .(file), function(df){
		n <- as.numeric(sub('[0-9]*-[0-9]*-','',df$Group.ID))
		df[order(n),]
	})
	temp <- temp[,!names(temp) %in% 'file']
	print(temp, row.names=F)



#  Group.ID    n  age   type  dose   unit application

# Results
# Checked n, age, lifespan or not, type, dose, unit and application
# for most studies (more below) and developed a huge list of
# discrepiances which I will be working through.
#
# Most notably, I found that some animals were repeated as multiple
# rows if they recieved more than one treatment.  I fix that below.


###################################################################
#
# Double Vision
# 27 Feb 2013
#
# Introduction:
# During my first close pass through the study groups, I discovered
# that some of treatment groups included the same mouse more than
# once, one time for each distinct treatment that they had.  I need
# to find the extent of that problem and then reorder the data so
# that each mice only has one row and each treatment is merged.
#
# Once that is done, I will resave the data so that future work will
# not have to concern itself with such confusion.
#

	# Libraries
	library(plyr)
	
	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external.rds')


	# Define Group Constants
	# all of these should be the same for every
	# mouse in a given group.
	constants <- c(
		"Group.Name", 
		"Treatment.Sequence",
		"Treatment.Age",
		"Treatment.Type",
		"Treatment.Quantity",
		"Treatment.Unit",
		"Treatment.Application",
		"Treatment.Remarks",
		"No.of.individuals.in.study",
		"Species",
		"Strain",
		"Group.Endpoints"
	)

	
	# Find constants per group
	appearances <- ddply(data, .(Group.ID), function(df){
		result <- laply(constants, function(col){
			length(unique(df[,col]))
		})
		names(result) <- constants
		result <- c(result, n=nrow(df))
		result		
	})

	# Find groups with recurrent data
	recurrent <- appearances[rowSums(appearances[2:13]) > 12,]
	dim(recurrent)  # 256

	
	# Which columns recur?
	names(appearances)[2:13][
		colSums(appearances[2:13]) > length(unique(data$Group.ID))
	]
	# Answer - Only treatment columns
	
	# Add max recurrances by id
	recurrent <- ddply(recurrent, .(Group.ID), function(df){
		df$max <- max(df[,2:13])
		df
	})
	
	# Check multiples
	# *each n should be an even multiple of the max number of
	# recurrances
	m <- recurrent$n / recurrent$max
	sum(m != round(m))		# 0 - they are all even multiples
	
	# Look at a sample group
	data[data$Group.ID == '11-1-10',]
	
	# What is the maximum number of repeats?
	max(recurrent$max)		# 3
	
	# Strategy
	# Our sample data and observations thusfar lay out a simple
	# strategy.  Each group seems to be stacked atop one another
	# animals share ids and time of death figures.  Treatment
	# columns are the only columns that vary.
	#
	# Therefore we ought to merge the data for each recurrent
	# id.  We will cut the data into X equal elements where
	# x is the max number of recurrent values.  We will check that
	# each of these divided data match on animal id and time of
	# death and if they do then we will create extra treatement
	# columns so that each animal will ultimately have three
	# treatment columns.
	
	treatments <- c(
		"Treatment.Sequence",
		"Treatment.Age",
		"Treatment.Type",
		"Treatment.Quantity",
		"Treatment.Unit",
		"Treatment.Application",
		"Treatment.Remarks"
	)
	empty_treatments <- data[1, treatments]
	empty_treatments[1,treatments] <- NA
	
	merged <- ddply(data, .(Group.ID), function(df){
		
		id <- df$Group.ID[1]
		max <- 1
		n <- nrow(df)
		
		if(id %in% recurrent$Group.ID){
			max <- recurrent$max[recurrent$Group.ID == id]
		}

		if(max == 1){
			df <- data.frame(
				df, 
				empty_treatments, 
				empty_treatments
			)
		}

		if(max == 2){
			d1 <- df[1:(n/2),]
			d2 <- df[(n/2 + 1):n,]
			if(!identical(
				d1$Age.at.death, 
				d2$Age.at.death
			)){ print(id) }
			df <- data.frame(
				d1, 
				d2[, treatments], 
				empty_treatments
			)
		}
		if(max == 3){
			d1 <- df[1:(n/3),]
			d2 <- df[(n/3 + 1):(2*n/3),]
			d3 <- df[(2*n/3 + 1):n,]
			if(!identical(
				d1$Age.at.death, 
				d2$Age.at.death,
				d3$Age.at.death
			)){ print(id) }
			df <- data.frame(
				d1, 
				d2[, treatments], 
				d3[, treatments]
			)
		}
		df
	})
	
	
	## Proof ##
	
	# Proper column count
	ncol(merged) == ncol(data) + length(treatments)*2
	
	# Proper row count
	diff = sum(recurrent$n) - sum(recurrent$n / recurrent$max)
	nrow(data) == nrow(merged) + diff
	
	# recurrances are gone
	merged_appearances <- ddply(merged, .(Group.ID), function(df){
		result <- laply(constants, function(col){
			length(unique(df[,col]))
		})
		names(result) <- constants
		result <- c(result, n=nrow(df))
		result		
	})
	all(rowSums(merged_appearances[2:13]) == 12)

	# Look at a sample group
	merged[merged$Group.ID == '11-1-10',]	
	
	# How about treatment sequences
	table(merged$Treatment.Sequence, useNA='always')	
	# First sequences has mostly 1s, 18 2s, and 217 NAs
	table(merged$Treatment.Sequence.1)		# All 2s
	table(merged$Treatment.Sequence.2)		# All 3s
		
	# 2 sequences in first column
	merged[
		merged$Treatment.Sequence == 2 & 
		!is.na(merged$Treatment.Sequence)
	,]
	data[data$Group.ID == '11-1-143',]
	# The 2s are all from 11.1.143
	# Their neutron treatment appears to be missing
	# I've added this to problems
	
	# NAs in first sequence column
	unique(merged[
		is.na(merged$Treatment.Sequence)
	,'Group.ID'])	
	head(data[data$Group.ID == '1002-1-99',])
	head(data[data$Group.ID == '1003-6-99',])
	head(data[data$Group.ID == '1005-47-8',])
	head(data[data$Group.ID == '3-6-24',])
	# NAs are the beagle experiments with no treatment listed
	# I noticed them before, no worries.
	
	# Treatment Sequences have twos but no ones?
	table(
		is.na(merged$Treatment.Sequence),
		merged$Treatment.Sequence.1 == 2
	)
	# never

	# Treatment Sequences have three but no twos?
	table(
		is.na(merged$Treatment.Sequence.1),
		merged$Treatment.Sequence.2 == 3
	)
	# never
	
	# Write to file
	# I am reasonbly confident that the treatment data is
	# correctly widened now, I will write to file.
	setwd('~/janus/scripts')
	saveRDS(merged, '../data/external.rds')
	data <- readRDS('../data/external.rds')
	
#
# Results:
# We successfully widened the treatment columns so that some
# animals include more than one column of treatments, three at
# most.
#
# This reduced the number of rows in the dataset from 130424 to
# 118636, (about 10,000 mice).
	

###################################################################
#
# A rose by any other name
# 28 Feb 2013
#
# Introduction:
# With 39 columns and long names the data is painful to deal with
# here I will make the column names simpler.  Mostly underlined and
# a single word.

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external.rds')

	# to drop
	to_drop <- c(
		"Treatment.Sequence",
		"No.of.individuals.in.study",
		"Group.ID.1",
		"X",
		"Treatment.Sequence.1",
		"Treatment.Sequence.2"
	)

	# new names
	new_names <- c(
		"file",
		"group.id",
		"group.name",
		"age.at.treatment.1",
		"treatment.1",
		"dose.1",
		"unit.1",
		"application.1",
		"remarks.1",
		"species",
		"strain",
		"sex",
		"lifestage.at.treatment",
		"endpoints",
		"id",
		"lifespan",
		"pathology",
		"location",
		"pathology.description",
		"n",
		"in_era",
		"age.at.treatment.2",
		"treatment.2",
		"dose.2",
		"unit.2",
		"application.2",
		"remarks.2",
		"age.at.treatment.3",
		"treatment.3",
		"dose.3",
		"unit.3",
		"application.3",
		"remarks.3"
	)
	
	new_order <- c(
		"file",
		"group.id",
		"group.name",
		"n",
		"in_era",
		"species",
		"strain",
		"endpoints",
		"id",
		"sex",
		"lifespan",
		"pathology",
		"location",
		"pathology.description",
		"lifestage.at.treatment",
		"age.at.treatment.1",
		"treatment.1",
		"dose.1",
		"unit.1",
		"application.1",
		"remarks.1",
		"age.at.treatment.2",
		"treatment.2",
		"dose.2",
		"unit.2",
		"application.2",
		"remarks.2",
		"age.at.treatment.3",
		"treatment.3",
		"dose.3",
		"unit.3",
		"application.3",
		"remarks.3"	
	)


	data <- data[,!names(data) %in% to_drop]
	names(data) <- new_names
	data <- data[,new_order]

	saveRDS(data, '../data/external2.rds')



###################################################################
#
# Here we go again
# 28 Feb 2013
#
# Introduction:
# Time now to return to checking the basic facts from before

	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')

	# Helpers
	clean <- function(s) gsub('[^0-9A-Za-z,._ -]', '', s)
	truncate <- function(s) substr(s, 1, 6)

	# cache some
	groups <- data$group.id
	file <- data$file
	
	# Clean for printing
	data <- data.frame(llply(data, clean))
	data <- data.frame(llply(data, truncate))
	data <- data.frame(llply(data, as.character), stringsAsFactors=F)
	data$group.id <- groups
	data$file <- file

	# Print Treatments
	temp <- ddply(data, .(group.id), function(df){
		. <- df[1,]
		c(
			file=.$file,
			n=nrow(df),
			age=.$age.at.treatment.1,
			type=.$treatment.1,
			dose=.$dose.1,
			unit=.$unit.1,
			application=.$application.1
		)
	})
	temp <- ddply(temp, .(file), function(df){
		n <- as.numeric(sub('[0-9]*-[0-9]*-','',df$group.id))
		df[order(n),]
	})
	temp <- temp[,!names(temp) %in% 'file']
	print(temp, row.names=F)

  
###################################################################
#
# Expected Constants
# 4 March 2013
#
# I expect that most of the values by experimental group will
# be the same, but after seeing that multiple treatments were 
# treated as multiple rows, its best that I check that assumption.

	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')
	
	# Find Uniques
	result <- ddply(data, .(group.id), function(df){
		laply(df, function(col) length(unique(col)))
	})
	names(result) <- c('group', names(data))
	
	# Filter
	ok <- c(
		'id',
		'lifespan',
		'pathology',
		'location',
		'pathology.description'
	)
	filtered <- dlply(result, .(group), function(row){
		row <- row[,row > 1]
		row <- row[!names(row) %in% ok]
		row
	})
	unusual <- laply(filtered, function(cell){length(cell) > 1})
	
	# Results
	filtered[unusual]		# empty


#
# Results:
# Nothing to see here.  It appears that groups have the same
# number of every variable you would expect for them to have.


 
###################################################################
#
# Expected Variables
# 4 March 2013
#
# Elaborating on that last test, we do expect some variables to
# change within a group, notably id, lifespan, pathology, location,
# and pathology description.  Do they?

	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')
	
	# Find Uniques
	result <- ddply(data, .(group.id), function(df){
		laply(df, function(col) length(unique(col)))
	})
	names(result) <- c('group', names(data))
	
	# Helper
	did_not_change <- function(should_change){
		# Filter
		filtered <- dlply(result, .(group), function(row){
			row <- row[,row < 2]
			row <- row[names(row) %in% should_change]
			row
		})
		unusual <- laply(filtered, function(cell){length(cell) > 0})
		
		# Results
		names(filtered[unusual])
	}
	first_value <- function(groups, column){
		subset <- data[data$group.id %in% groups,]
		ddply(subset, .(group.id), function(df){
			df[1, column]
		})
	}
	
	# Results
	id <- did_not_change('id')  			# 8 studies with n=1
	life <- did_not_change('lifespan')	 	
	life[!life %in% id]						# 9 studies usually the
											# same lifespans
	
#
# Results:
# A few studies were noted as having no change in lifespan.  More
# appeared with only one pathology result, but I stopped processing 
# these because this is not the data we are going for in this pass.


	
	
###################################################################
#
# Second Pass - species, strain, gender, exposure, age
# 5 March 2013
#
# I checked dose, n, and other factors on a first pass through the
# groups, but was not able to check gender, species, strain, and
# age at treatment.  Now it is time to do those things.


	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')
	
	# Helpers
	clean <- function(s) gsub('[^0-9A-Za-z,._ -]', '', s)
	truncate <- function(s) substr(s, 1, 12)

	# cache some
	groups <- data$group.id
	file <- data$file
	
	# Clean for printing
	data$strain <- sub("^[^,]*, ", "", data$strain)
	data <- data.frame(llply(data, clean))
	data <- data.frame(llply(data, truncate))
	data <- data.frame(llply(data, as.character), stringsAsFactors=F)
	data$group.id <- groups
	data$file <- file
	
	
	# Group Summary
	summary <- ddply(data, .(group.id), function(df){
		c(
			file=df$file[1],
			males=sum(df$sex == 'Male', na.rm=T),
			females=sum(df$sex== 'Female', na.rm=T),
			other=sum(
				is.na(df$sex) | 
				!(df$sex %in% c('Male', 'Female'))
			),
			species = df$species[1],
			strain = df$strain[1],
			age = median(df$age.at.treatment.1, na.rm=T)
		)
	})
	
	# Sort Groups
	summary <- ddply(summary, .(file), function(df){
		n <- as.numeric(sub('[0-9]*-[0-9]*-','',df$group.id))
		df[order(n),]
	})
	summary <- summary[,!names(summary) %in% c('file')]
		
	
#
# Results:
# Found a few small descrepiancies.  Was not able to check age
# because it was wrong so often.

	
###################################################################
#
# Treatment Age
# 6 March 2013
#
# My last analysis found many NA values in treatment age.  I need
# to see why this is true.


	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')	
	
	# Group Summary
	summary <- ddply(data, .(group.id), function(df){
		age <- df$age.at.treatment.1
		
		c(
			age=age[1],
			same=all(age[1] == age[1]),
			na=sum(is.na(age))
		)
	})
	
	summary[is.na(summary$same) | summary$same != 1 | summary$na > 0,]


	summary[
		!is.na(summary$same) & 
		!summary$same != 1 &
		!summary$na > 0,
	]

	
#
# Results:
# There are about 100 groups without information about treatment age
# these will generally need to be provided from other sources.  A few
# small extra discrepiances were noticed.  I will need to know when
# animals were garunteed alive, for my analysis, that is could they 
# die during the investigation?


###################################################################
#
# Is Treatment ever Pre Death?
# 6 March 2013
#
# A quick sanity check.

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')	
	
	# Group Summary
	diff <- data$lifespan - data$age.at.treatment.1
	greater <- diff < 0
	table(greater, useNA = 'ifany')   	# 722 are, 3788 NAs
	length(unique(data$group.id[greater & !is.na(greater)]))
	quantile(diff[greater & !is.na(greater)], na.rm=T)
	
	# 722 animals from 150 groups
	# 416 to 1 day too soon, usually 41
	
#
# Results:
# This is clearly a problem, affecting 722 animals from 150
# groups.  I should revisit the issue after proofing and see
# if it continues to persist.



###################################################################
#
# Multiple Treatments
# 6 March 2013
#
# Prevoius analysis did not allow me to carefully check animals
# that had recieved multiple treatments.  I will do that now.


	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')
	
	# Helpers
	clean <- function(s) gsub('[^0-9A-Za-z,._ -]', '', s)
	truncate <- function(s) substr(s, 1, 15)

	# cache some
	groups <- data$group.id
	file <- data$file
	
	# Clean for printing
	data$strain <- sub("^[^,]*, ", "", data$strain)
	data <- data.frame(llply(data, clean))
	data <- data.frame(llply(data, truncate))
	data <- data.frame(llply(data, as.character), stringsAsFactors=F)
	data$group.id <- groups
	data$file <- file
	
	
	# Group Summary
	subset <- data[!is.na(data$treatment.2),]
	summary <- ddply(subset, .(group.id), function(df){
		c(
			file=df$file[1],
			t1=df$treatment.1[1],
			t2=df$treatment.2[1],
			t3=df$treatment.3[1]
		)
	})
	
	# Sort Groups
	summary <- ddply(summary, .(file), function(df){
		n <- as.numeric(sub('[0-9]*-[0-9]*-','',df$group.id))
		df[order(n),]
	})
	summary <- summary[,!names(summary) %in% c('file')]
		
	
#
# Results:
# Found a few small descrepiancies.  Was not able to check age
# because it was wrong so often.



###########################################################
# 
# Restructuring
#
# The current data structure is less than ideal, too much
# information is given to chemical treatments, its not clear
# what is controlling for what, and not enough information
# is given about the details of radiation dosing.
#
# I will add columns as documented in the code.

	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external2.rds')
	
	# Columns to add 
	to_add <- c(
		'study.id',	# like group, but for study
		'cluster',	# a group of groups wherein only radiation 
					# treatment varies
		'quality',	# Like treatment.1 now, but only radiation
		'unit',		# like unit.1 now, but only radiation
		'dose',		# like dose.1 now, but only radiation
		'dose_rate',# unit / minute
		'fractions',# exposure count
		'fraction_time', # minutes / fraction
		'age.at.treatment', # like age.at.treatment.1 now
		'assignment_age', # age that an animal entered the study
					# generally just age.at.treatment
		'fraction_interval', # days between fractions
		'remarks',  # like remarks.1 now, but only radiation
		'other_treatments',	# a combination of descriptors about any
					# secondary treatments which should be consistent
					# within a cluster
		'is_vetted' # a binary indicating if the data has been 
					# checked thoroughly
	)
	
	# Define Treatment columns
	treatment_cols <- c(
		'age.at.treatment',
		'treatment',
		'dose',
		'unit',
		'application',
		'remarks'
	)
	
	# Radiation treatments
	rad_treatments <- c(
		'X-rays whole body',
		'gamma-rays Co-60',
		'neutrons fission',
		'gamma-rays Cs-137',
		'neutrons 1-10 MeV',
		'neutrons>10 MeV',
		'accel. neutrons 0.1-10 MeV',
		'X-rays local',
		'ﬂ-rays',
		'gamma-rays whole body',
		'accel. alpha whole body',
		'accel. alpha local',
		'Bremsstrahlung > 3MeV.',
		'neutrons C-252',
		'gamma-rays local'	
	)
	
	# Control treatments
	control_treatments <- c(
		'none (controls)',
		'shamexposed'
	)
	
	# Add columns
	for(col in to_add){
		data[,col] <- NA
	}
	
	# Populate function
	# This should replace when a value is NA, but concatenate
	# with a seperator when it is not NA.  The purpose of this
	# is to ensure that we never overwrite data.  For example
	# if an animal is exposed to two different types of radiation
	# we will want to know that
	populate <- function(.in, .out){
		na <- is.na(.out)
		.out[na] <- as.character(.in[na])
		.out[!na] <- paste(.out[!na], .in[!na], sep=', ')		
		.out
	}
	
	# Populate Radiation Treatment
	# Radiation treatments are those defined by rad_treatments
	# this function will select for those
	populate_radiation_treatment <- function(.in, .out, treatment){
		w <- which(
			treatment %in% c(rad_treatments, control_treatments)
		)
		.out[w] <- populate(.in[w], .out[w])
		.out
	}
	
	# Populate all radiation treatments
	# Radiation treatments might occur in any of three columns
	# this will look for any of those
	populate_all_radiation_treatments <- function(.in, .out){

		range <- 1:3

		.out <- data[, .out]
		treatment_cols <- paste0('treatment.', range)
		.in_cols <- paste0(.in, '.', range)
		
		for(i in range){
			.in  <- data[,.in_cols[i]]
			treatment <- data[,treatment_cols[i]]
			
			.out <- populate_radiation_treatment(
				.in,
				.out,
				treatment
			)
		}
		
		.out	
	}
	
	# Populate radiation variables
	data$quality <- populate_all_radiation_treatments(
		'treatment',
		'quality'
	)
	data$unit <- populate_all_radiation_treatments(
		'unit',
		'unit'
	)	
	data$dose <- populate_all_radiation_treatments(
		'dose',
		'dose'
	)	
	data$remarks <- populate_all_radiation_treatments(
		'remarks',
		'remarks'
	)
	
	# Populate other treatments
	relevant_treatments <- c(rad_treatments, control_treatments)
	for(i in 1:3){
		treatment = data[,paste0('treatment', '.', i)]
		for(col in treatment_cols){
			col <- paste0(col, '.', i)
			subset <- !treatment %in% relevant_treatments &
					  !is.na(treatment)
			data[subset,'other_treatments'] <- populate(
				data[subset, col],
				data[subset, 'other_treatments']
			)
		}
	}
	
	# None are vetted yet
	data$is_vetted <- FALSE
		
	# Compose study id
	data$study.id <- sub('\\-[0-9]*$', '', data$group.id)
	
	# Age
	data$age.at.treatment <- data$age.at.treatment.1
	data$assignment.age <- data$age.at.treatment
	
	# Cluster 
	# by group, other treatments, and species
	# *note some of these will be wrong, they can be
	#  corrected during the close pass
	data <- ddply(data, .(file), function(df){
		n <- as.numeric(as.factor(paste(
			df$other_treatments,
			df$species
		)))
		df$cluster <- paste0(df$study.id, '-', n)
		df
	})

	saveRDS(data, '../data/external3.rds')

# Results
# 
# We have a new file, better than ever (hopefully)

###################################################################
#
# No Multiple Treatments
# 8 March 2013
#
# I checked to be sure that multiple treatments actually recieved
# multiple treatments, but I didn't check to be sure that single
# treatments didn't actually recieve multiple treatments.  I will
# do that now.

	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external3.rds')
	
	# Helpers
	clean <- function(s) gsub('[^0-9A-Za-z,._ -]', '', s)
	truncate <- function(s) substr(s, 1, 15)

	# cache some
	groups <- data$group.id
	file <- data$file
	
	# Clean for printing
	data$strain <- sub("^[^,]*, ", "", data$strain)
	data <- data.frame(llply(data, clean))
	data <- data.frame(llply(data, truncate))
	data <- data.frame(llply(data, as.character), stringsAsFactors=F)
	data$group.id <- groups
	data$file <- file
	
	
	# Group Summary
	data <- ddply(data, .(file), function(df){
		df$none <- all(is.na(df$treatment.2))
		df
	})
	single_treatments <- unique(data$file[data$none])
	
	cat(paste(single_treatments, '\n'))
		
	
# Results
# Looking good, no problems to report.





###################################################################
#
# Merge in Janus
# 8 March 2013
#
# The Janus mouse records are more complete than the ones available
# from the ERA.  I will drop their records and add our own.

	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external3.rds')
	janus <- read.csv('~/Downloads/demographics.csv', skip=1)
	
	# Constants
	era_janus_study_id <- '1003'
		
	# Make names compatiable
	names(janus) <- c(
		'id',
		'necroscopy_date',
		'necrosopy_proctor',
		'lifespan',
		'cause_of_death',
		'autopsy_type',
		'has_micro',
		'sex',
		'species',
		'study.id',	
		'tmt',
		'remarks',
		'age.at.treatment',
		'dose',
		'quality',
		'was_control_mock_treated',
		'fractions',
		'fraction_time',
		'dose_rate'
	)
	
	# Remove some
	to_remove <- c(
		'necroscopy_date',
		'necrosopy_proctor',
		'autopsy_type',
		'has_micro',
		'tmt',
		'was_control_mock_treated'	
	)
	janus <- janus[,! names(janus) %in% to_remove]
	
	# Helper
	recode_factor <- function(x, before, after){
		x <- as.character(x)
		x[x == before] <- after
		as.factor(x)
	}
	
	# Recode sex
	janus$sex <- recode_factor(janus$sex, 'F', 'Female')
	janus$sex <- recode_factor(janus$sex, 'M', 'Male')

	# Recode species
	janus$species <- recode_factor(
		janus$species, 
		'Mus musculus', 
		'Mouse'
	)
	janus$species <- recode_factor(
		janus$species, 
		'Peromyscus leucopus', 
		'Peromyscus'
	)
		
	# Recode studies
	cached_study.id <- janus$study.id
	janus$study.id <- recode_factor(janus$study.id, '2', '1003-20')
	janus$study.id <- recode_factor(janus$study.id, '3', '1003-21')
	janus$study.id <- recode_factor(janus$study.id, '4', '1003-22')
	janus$study.id <- recode_factor(janus$study.id, '7', '1003-24')
	janus$study.id <- recode_factor(janus$study.id, '8', '1003-25')
	janus$study.id <- recode_factor(janus$study.id, '9', '1003-26')
	janus$study.id <- recode_factor(janus$study.id, '10','1003-27')
	janus$study.id <- recode_factor(janus$study.id, '11','1003-xx')
	janus$study.id <- recode_factor(janus$study.id, '12','1003-28')
	janus$study.id <- recode_factor(janus$study.id, '13','1003-29')
	janus$study.id <- recode_factor(janus$study.id, '14','1003-30')
	
	# recode quality
	janus$quality <- recode_factor(
		janus$quality, 
		'G', 
		'gamma-rays whole body'
	)
	janus$quality <- recode_factor(
		janus$quality, 
		'N', 
		'neutrons fission'
	)
	janus$quality <- recode_factor(
		janus$quality, 
		'C', 
		NA
	)

  # cGy -> Gy
  janus$dose <- janus$dose / 100
  janus$dose_rate <- janus$dose_rate / 100
	    		
	# add
	janus$is_vetted <- F
	janus$unit <- 'grays'
	janus$assignment_age <- janus$age.at.treatment
	janus$group.id <- paste0(janus$study.id, '-', cached_study.id)
	janus$cluster <- paste0(janus$study.id, '-', 1)
	janus$strain <- 'Mouse, B6CF1'
	janus$strain[janus$species == 'Peromyscus'] <- 'leucopus'
	
	#   strain (based on species)
	# 	group.id <- study.id + cached_study.id 

  # Remove animals that died of un-natural causes
  natural_deaths <- c('Died',
                      'Sacrifice, moribund')
  janus <- janus %.% filter(cause_of_death %in% natural_deaths)
	
	# Remove Janus from era
	jdata <- grepl('1003.2', data$study.id)
	data <- data[!jdata,]
	
	# Add Real Janus
	data <- merge(data, janus, all=T)

	# Save
	saveRDS(data, '../data/external3.1.rds')

	
# Results
# Janus is merged (fingers crossed)



###########################################################
#
# Merge in Beagles
# April 2015
#
# The Janus Beagle records are more complete than those 
# available through the ERA.  I will use janus.

# Libraries
library(plyr)
library(dplyr)

# Data
setwd('~/janus/scripts')
data <- readRDS('../data/external3.1.rds')
basics <- read.csv('http://s3.amazonaws.com/janus-cloud2/www/dog_tissues/data/basics.csv')
chronic <- read.csv('http://s3.amazonaws.com/janus-cloud2/www/dog_tissues/data/cobalt_chronic.csv')
fractionated <- read.csv('http://s3.amazonaws.com/janus-cloud2/www/dog_tissues/data/cobalt_fractionated.csv')

# Helper
recode_factor <- function(x, before, after){
  x <- as.character(x)
  x[x == before] <- after
  as.factor(x)
}

# Constants
era_beagle_study_id <- '1003'

# Limit to those in documented studies that only involved
# external radiation exposure (5, and 6) see [1]
# [1]: http://www.ustur.wsu.edu/nra/pdf/ira.pdf
basics <- basics %.% 
  filter(study %in% 5:6)

# Remove unnecessary columns
to_remove <- c(
  'birth_date',
  'tissues',
  'experiment',
  'era_GroupID',
  'era_studyID',
  'dam',
  'sire',
  'interrupts',
  'dose_per_fraction'
)

basics <- basics[,!names(basics) %in% to_remove]
chronic <- chronic[,!names(chronic) %in% to_remove]
fractionated <- fractionated[,!names(fractionated) %in% to_remove]

# Make names compatiable
names(basics) <- c(
  'id',
  'cause_of_death',
  'lifespan',
  'sex',
  'group.id',
  'remarks',
  'study.id')
names(chronic) <- c(
  'id',
  'dose',
  'dose_rate',
  'fractions',
  'age.at.treatment')
names(fractionated) <- c(
  'id',
  'fractions',
  'fraction_time',
  'age.at.treatment',
  'dose_rate',
  'dose')

# Make values compatiable
basics$sex <- recode_factor(basics$sex, 'F', 'Female')
basics$sex <- recode_factor(basics$sex, 'M', 'Male')
basics$sex <- recode_factor(basics$sex, 'U', 'Both')

chronic <- chronic %.%
  mutate(dose = dose / 100, 
         dose_rate = dose_rate / (100 * 22*60 ))

fractionated <- fractionated %.%
  mutate(dose = dose / 100,
         dose_rate = dose_rate / (100))

# Merge tables
beagle <- merge(chronic, 
              fractionated,
              on='id',
              all=TRUE)
beagle <- merge(basics,
              beagle,
              on='id',
              all.x=TRUE)

# Normalize ID values
beagle <- beagle %.%
  mutate(study.id = paste0(era_beagle_study_id, '-', study.id),
         group.id = paste0(study.id, '-', group.id),
         id = paste0(group.id, '-', id))

# Restrict to animals which died a natural death
natural_causes_of_death <- c('Age', 
                             'Radiation',
                             'Sacrificed Moribund')


# add
beagle$is_vetted <- F
beagle$unit <- 'grays'
beagle$assignment_age <- beagle$age.at.treatment
beagle$cluster <- paste0(beagle$study.id, '-', 1)
beagle$strain <- 'Dog, Beagle'
beagle$species <- 'Dog'
beagle$quality <- 'gamma-rays whole body'

# Remove animals that died of un-natural causes
natural_deaths <- c('Radiation',
                    'Sacrificed Moribund')
beagle <- beagle %.% filter(cause_of_death %in% natural_deaths)

# Remove Beagles from era
bdata <- 
  grepl('1003', data$study.id) &
  data$strain == 'Dog, Beagle'
data <- data[!bdata,]

# Add Real Beagles
data <- merge(data, beagle, all=T)

# Save
saveRDS(data, '../data/external4.rds')

# Results
# Argonne Beagles are merged (fingers crossed)



###########################################################
# 
# Remove non-ASCII
# April 2014
#
# Revisiting these scripts I am getting coding errors that
# seemed to be cause by non-ascii characters like gamma.
#
#   > grepl('', study_data$other_treatments[1028])
#   [1] FALSE
#   Warning message:
#     In grepl("", study_data$other_treatments[1028]) :
#     input string 1 is invalid in this locale
#
# One quick / dirty way to fix that is to remove all non
# ASCII characters.
# 

# Libraries
library(plyr)

astype <- function(obj, type) {
  FUN <- switch(type,
                character = as.character, 
                numeric = as.numeric, 
                factor = as.factor,
                logical = as.logical,
                integer = as.integer,
                complex = as.compex, 
                double = as.double)
  FUN(obj)
} 

# Data
setwd('~/janus/scripts')
data <- readRDS('../data/external4.rds')

# Precheck
str(data)    # 127919 x 49

not_ascii = "[^0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&\\(\\)\\*\\+,-\\./:;<=>?@[^_`\\{|\\}~ \t\n\r']"
column_names = names(data)
for(column in column_names) {
  data[,column] = 
    astype(sub(not_ascii, "", data[,column]), class(data[,column]))
}

# Postcheck
str(data)    # 127919 x 49

saveRDS(data, '../data/external4.rds')



###########################################################
# 
# Outside Literature
#
# The time is ripe to solve the problems I have found, 
# consult the outside literature to arbitrate, fill in 
# missing values.
#
# Its a complex process.  I will list the steps I can 
# anticipate with the expectation that I will expand this
# list.
#
# For each file:
#	Check for noted problems
# 	Check for NAs
# 	Resolve these problems and NAs using the cited literature
#	check cluster assignments
# 	check that the age of assignment to control conditions is correct

	# Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.rds')
	
	
	# Helpers
	show_groups <- function(group.id, verbose=F){
		
		of_interest <- c(
			'study.id',
			'group.id',
			'id',
			'lifespan',
			'species',
			'strain',
			'sex',
			'cluster',
			'application.1',
			'quality',
			'unit',
			'dose',
			'dose_rate',
			'fractions',
			'fraction_time',
			'age.at.treatment',
			'fraction_interval',
			'assignment.age',
			'lifestage.at.treatment',
			'is_vetted',
			'other_treatments',
			'remarks',
			'group.name',
			'endpoints'
		)
		
		# Verbose Variables
		verbose_vars <- c('id', 'lifespan')
		
		# clean a little
		subset <- data[data$study.id == group.id,of_interest]
		
		if(verbose){
			subset$id <- sub('^[0-9]*.[0-9]*.[0-9]*.', '', subset$id)
			subset$id <- as.numeric(subset$id)			
		} else {
			subset <- subset[!names(subset) %in% verbose_vars]
		}
		
		temp <- dlply(subset, .(group.id), function(df){
			cat('\n', df$group.id[1], '\n')
			spacer = rep('\t', 7)
			n = nrow(df)
			cat('\t', 'n', spacer, n, '\n', sep='', collapse='')
			for(i in 1:ncol(df)){
				col <- df[,i]
				u <- unique(paste(col))
				name <- names(df)[i]
				tabs <- max(ceiling(7 - (nchar(name)+1)/3), 1)
				spacer <- rep('\t', tabs)
				cat('\t', name, spacer, u, '\n', sep='', collapse='')
			}
		})
		
	}
	

	# Start with one
	show_groups('9-6')
	
	### Develop concordance ###
	
	## concordance 9-6 C57bl/Cnb mice are a substrain of C57bl.  I cannot find much information on this particular substrain, but it is interesting to note that C57bl mice are the most common mice in use (specifically C57bl/6 mice) and are a little odd as mice go.  They like alcohol, don't like climbing stairs, and are quicker to go after food than other mice.  They've been called the alcoholic couch potatoe of mice. http://www.informatics.jax.org/external/festing/mouse/docs/C57BL.shtml
	## concordance 9-6 SCK/CEN is the studiecentrum voor kernerergie (dutch) or the Center d'Etude l'energie Nucleair, a belgian facility with 600 employees that studies many aspects of radiation especially relevant to the construction and maintainance of power facilities.
	## ERAD is the european radiobiology archive database, is a disk that was used to populate the ERA, originally held by GERBER. http://bit.ly/X0SqRU
	## thorax, the part of the body between the neck and the abdomen.  From a greek work for chest
	## deuteron, one proton and one neutron, a heavy hydrogen stripped of its electrons.  Notably it is believed that all deuterium is a product of the big bang as opposed to a star product.
	## berilium can be used as a target shooting off neutrons when hit with gamma or alpha radiation.  http://en.wikipedia.org/wiki/Beryllium
	## EULEP is the European Late Effects Project, presumabably to study the long term effects of radiation exposure.  They seem to have run out of funding round about 1995.  http://www.euradnews.org/storyfiles/130125.0.eulep_news_may_2005.pdf
	## Late Effect, as supposed a late effect is one which arrives long after the acute effect, could be weeks, could be decades.  http://en.wikipedia.org/wiki/Late_effect
	## An ionization chamber is a simple device used to measure radiation doses.  It consists of two opposingly charged plates or a wire surrounded by a cylindrical plate.  Ionizing events caused by incident radiation cause ionized particles to move to the charged elements inducing a current opposite to the charge of the plate which can be used to effectively measure the number of ioninzing events of the incident radiation.  However, it is not possible to understand the energetic characteristics of these events, the speed and concentration of the resulting particles.  Interestingly ionization chambers are also the principal that smoke detectors use, they have a constant radaition source which creates an ionization current but when smoke enters the chamber, it absorbs these ions and the alarm is signaled.
	## Shonka tissue equiviliant ion chambers are specially designed with plastic and gas that mimics the atomic composition of living tissues.  This makes them ideal insturments to measure the absorbed doses in living tissues.  They are used for neutron and proton radiation, though I'm not sure why the same concerns do not apply to gamma irradaition?  http://www.orau.org/ptp/collection/ionchamber/shonkatissueequivalent.htm
	## The thymus is a specialized imune organ located in the middle of the lungs that produces immune cells.
	## Deterministic effects of radiation exposure occur only after some threshold exposure has been reached.  This would include sepsis which depends on a high enough dose to inactive the immune system which will recover if the dose is sufficiently low.  Or late effects of extensive lung damage.  http://www.imagewisely.org/Imaging-Professionals/Imaging-Physicians/Articles/Ionizing-Radiation-Effects-and-Their-Risk-to-Humans
	## Sarcomas are tumors that arrise in connective tissues, like bone or cartilige, as opposed to epithelial tissues like the breast or skin.  These are rare in humans, presumbably because connective tissue multiplies much slower than epithelial tissues.
	## Fast Neutrons, as opposed to thermal neutrons, have high individual energies.  Their energy is typically above 1 MeV.  By contrast fission neutrons which are released from atomic decay have mean energies less than 2 MeV.
	## Penumbra is the space around a radiation target that recieves partial irraidaiton (20-80% ? whatever that means) it derives from the penumbra of an eclipse, the area of light that shines around the eclipsing body, it is defined as being fully sheilded from parts of the source and fully exposed to other parts giving it a partial total exposure.  
	## A perspex cage is a transparent cage made of perspex plastic, an alternative to glass.
	## Collimated refers to the process of making light parallel as is done for example with a laser.  
	## isodose, the areas of an exposure which recieve equal doses
	## fluence, the number of particles that pass through an area in a fixed unit of time.  For examples if 15 particles pass through 1 square meter over the time interval, fluence is 15 / m^2.  Fluence can also be considered in a volume where particles have a path length.  If their are 100 particples and the average path length of a given particle is 0.1 m in a meter cubed box over a given interval then the fluence is 100*0.1 m / 1 m^3 = 10 / m^2 that is, in any given square meter within the box we would expect 10 particles to pass through us. (this may only work at the limit of small, not sure).  http://en.wikipedia.org/wiki/Fluence
	## Specific energy is the energy per unit mass.  For example, radiation doses are usually measured in Grays which represent one Joule absorbed per kilogram of mass.  However, as the mass is reduced (or volume equivilantly) the specific energy may become non-uniform.  For example neutrons produce a specific energy that is bi-modal at small scales (the size of cells for instance) such that some cell masses have high specific energy and others have essentially no specific energy.  By contrast gamma irradiation is more distributed.  http://en.wikipedia.org/wiki/Specific_energy
	## Gamma vs Neutron, gamma radiation produces a more evenly distributed dose even when the total dose and energy per particle is the same as a neutron dose.  The reason is because gamma irradiation kicks off electrons which have a long path length, where Neutron irradiation kicks off protons which have a short path length. http://link.springer.com/article/10.1007%2FBF01323118?LI=true#page-6
	## A secondary electron or delta electron is simply the electrons farther down the chain of some primary, high energy electron which caused a domino effect.  Some make the cutoff as those electrons with less than < 50 eV.  For reference, the ionization energy for the first electron of most biological molecules is somewhere between 5 and 10 eV.  So at minimum a primary electron should cause 5 ionization events, usually many more.
	## Saturation effects.  It is possible that extremely high energy Neutrons will cause so much localized damage that they saturate the system, meaning that additional dose has no effect.  This is a good reason to include specific indicators of modal energy in these studies.
	## Microdosimetry, the study of the distribution of dose in very small volumes of tissues.  This study is what has lead to the theory of dual action.  A good review is here http://link.springer.com/article/10.1007%2FBF01323118?LI=true#page-11
	## Theory of Dual action, the belief that two sublesions must combine to form a lesion capable of damaging the cell, leading to death or promoting cancer.  The main evidence for this is the study of microdosimitry which shows that as events that might cause the smallest possible damage (a sublesion) are spread in space, the chance for biologically measurable damage (a lesion) gets smaller.  For instance comparing Neutron and Gamma irradiation.  http://link.springer.com/article/10.1007%2FBF01323118?LI=true#page-11
	## Supposedly C57Bl Cnb mice have a 'lower spontaneous cancer incidence, particularly with respect to thymic lymphomas and lung cancer'.  I never found a good source that attests to this outside of the paper.  I suppose they are right, but I'm not sure to what degree.  Also I would guess this is not important for lifespan effects, but who knows?
	## (F58) in a species name refers to the 'filial generations' that is how many inbred matings have been performed since the original two purebred strains were mated.
	## Louvain-la Neuve holds the cyclotron used to bombard a beryillium target with deuterons in order to emit Neutron irradiation.  The city is a planned one, it was where the french speaking portion of the Catholic University of Leuven moved after disputes between french and flemish speakers came to a head in the 1960s.  The area is very much a university town and has an annual 24 hour bike ride in which students decorate bikes, get drunk and have a good old time.  I should like to attend some day.  http://en.wikipedia.org/wiki/Louvain-la-Neuve
	## Specific pathogen free mice are those which are garunteed to be free of some given pathogen.  http://en.wikipedia.org/wiki/Specific_Pathogen_Free.  Interestingly these mice are more susceptiable to disease when exposed which is noted in this paper http://www.jstor.org/stable/pdfplus/3577205.pdf?acceptTC=true
	## Corrected for competing risks is a technique that would be used when we are looking at the lifespan effects of radiation on a given disease.  For example if you were looking at the effects of radiation on Leukemia it is necessary to say that animals that left the study by dying of something other than leukemia 'left the study' so we would only focus on the surviving population and the incidence of leukemia within it.  This idea has important implications when we think about the effects of radiation exposure.  If an animal is in a high risk environment, say a war zone then its risk from radiation might be limited because it will be masked by the danger inherent in a war zone.  
	## UNSCEAR the united nations scientific comittee on the effects of atomic radiation is a group established in the 1950s to measure the level of exposure to radaition of human populations.  They now also attempt to estimate the effects of that exposure.
	## The most important point of the ??? studies to me is that these Berillium produced Neutrons have an RBE about equivilant to gamma irradiation.  I assume this is because their damage is so concentrated that its redundant (~10x too concentrated) the equivilant of launching a gernade at a sqirrel when a bb gun would do.
	
	
	### Fix problems ###
	
	## 9-6-2 to 16 are listed as X-ray exposures in the data but as Gamma ray exposures in the descriptions.  RESOLVED - description is accurate, data is wrong.  I know because Maisin et al. 1988 refers to gamma rays.  http://www.jstor.org/stable/pdfplus/3577205.pdf?acceptTC=true.
	data[
		data$study.id == '9-6' & 
		data$quality == 'X-rays whole body',
		'quality'
	] <- 'gamma-rays Co-60'
	table(data[data$study.id == '9-6', c('dose', 'quality')])
	
	## 9-6-19 is listed as 0.18 Gy in the data and 0.54 Gy in the table. I'd be inclined to trust the table.  RESOLVED - description is accurate, data is wrong, I am confident because 9-6-19 has a n of 210 and Maisin et al. 1988 pg 305 Table IV lists a group with 210 animals that recieved a dose of 0.54 Gy.  http://www.jstor.org/stable/pdfplus/3577205.pdf?acceptTC=true.  Also the group name for 9-6-19 in the data indicates that it recieved 0.54 Gy.
	data[
		data$group.id == '9-6-19',
		'dose'
	] <- 0.54	
	table(data[data$study.id == '9-6', c('dose', 'group.id')])
	
	## 9-6-22 is listed as having 96 mice in the description, but has 196 mice in the data.  RESOLVED data is correct, description is wrong.  I am confident because the group listed in Table V of Maisin et al 1988 had 196 mice as the data lists.  http://www.jstor.org/stable/pdfplus/3577205.pdf?acceptTC=true
	# no action needed

	## 9-6-25 to 27 seem to be exposures just to the thorax, these are indicated as external exposures in the data.  RESOLVED the description is correct the data is wrong.  I am confident because Table VI of Maisin et al 1988 includes groups with the same number of mice as groups 25 to 27 and is a table of thorax exposures.  http://www.jstor.org/stable/pdfplus/3577205.pdf?acceptTC=true
	data[
		data$group.id %in% c('9-6-25','9-6-26','9-6-27'),
		'application.1'
	] <- 'External exposure local'
	table(data[data$study.id == '9-6', c('group.id', 'application.1')])
	

	### Resolve NAs ###

	# 9-6-1 is a control, has no dose, and has a dose rate of NA
	# I can update for all cases like this
	data[
		data$dose == 0 &
		!is.na(data$dose) &
		is.na(data$dose_rate),
		'dose_rate'
	] <- 0
	
	# 9-6-1 has NA values for fractions, but this can safely be 
	# called 0 because the animal was not irradiated or mock 
	# irradiated.  The other fractional values, fraction_time and
	# fraction_interval may remain NA since they don't make sense
	# when no fractions were delivered.
	data[
		data$group.id == '9-6-1',
		'fractions'
	] <- 0
	table(data[data$study.id == '9-6', c('fractions', 'group.id')], useNA='ifany')

	# 9-6-1 has NA values for other treatments.  This can be
	# updated in all cases to None
	
	data[
		is.na(data$other_treatments),
		'other_treatments'
	] <- 'none'
	sum(is.na(data$other_treatments))
	
	# 9-6 dose rates were consistent across treatment mode
	# Maisin et al 1988 lists a dose rate of 0.3 Gy/min for Gamma
	# treatment, but I cannot find the dose rate of neutron exposure
	# nor can I access the original sources which list these values
	# so I am afraid I must leave them as they are.
	data[
		data$study.id == '9-6' & 
		data$quality == 'gamma-rays Co-60',
		'dose_rate'
	] <- 0.3
	table(data[data$study.id == '9-6', c('quality', 'dose_rate')], useNA='ifany')	
	
	
	# 9-6 fractions differ by several study groups
	# I define them here and use them to fill in fractions
	# fraction_time, and fraction_interval
	gamma_1 <- c('9-6-2', '9-6-3', '9-6-4', '9-6-5', 
				 '9-6-6', '9-6-7')
	gamma_8 <- c('9-6-14', '9-6-15', '9-6-16')
	gamma_10 <- c('9-6-8', '9-6-9', '9-6-10', '9-6-11',
				  '9-6-12', '9-6-13')
	neutron_1 <- c('9-6-17', '9-6-18', '9-6-19', '9-6-20',
				   '9-6-21')
	neutron_8 <- c('9-6-22', '9-6-23', '9-6-24')
	neutron_thorax <- c('9-6-25', '9-6-26', '9-6-27')
	
	temp <- function(data, groups, fractions, fraction_interval){
		subset <- data$group.id %in% groups
		data[subset, 'fractions'] <- fractions
		data[subset, 'fraction_interval'] <- fraction_interval
		data
	}
	
	data <- temp(data, gamma_1, 1, NA)
	data <- temp(data, gamma_8, 8, 60*3)
	data <- temp(data, gamma_10, 10, 60*24)
	data <- temp(data, neutron_1, 1, NA)
	data <- temp(data, neutron_8, 8, 60*8)
	data <- temp(data, neutron_thorax, 1, NA)

	table(data[data$study.id == '9-6', c('fractions', 'group.id')], useNA='ifany')	
	table(data[data$study.id == '9-6', c('fraction_interval', 'group.id')], useNA='ifany')	
	
	# 9-6 checked mean lifespans, these check out, standard error
	# in lifespan, however, I cannot reproduced!
	
	
	### Add more data ###
	
	# source 
	# (from http://www.jstor.org/stable/pdfplus/3577205.pdf)
	data$source <- NA
	data[
		data$study.id == '9-6' &
		data$quality == 'gamma-rays Co-60',
		'source'
	] <- 'Cs137'
	data[
		data$study.id == '9-6' &
		data$quality == 'neutrons>10 MeV',
		'source'
	] <- 'd(50)+Be'
	
	table(data[data$study.id == '9-6', c('group.id', 'source')], useNA='ifany')	
	
	# modal energy (MeV)
	# from (https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=6)
	data$modal_energy <- NA
	data[
		data$study.id == '9-6' &
		data$quality == 'neutrons>10 MeV',
		'modal_energy'
	] <- 23
	table(data[data$study.id == '9-6', c('group.id', 'modal_energy')], useNA='ifany')	
	
	# gamma contamination (fraction)
	# from http://www.jstor.org/stable/pdfplus/3577205.pdf?acceptTC=true
	data$gamma_contamination <- NA
	data[
		data$study.id == '9-6' &
		data$quality == 'neutrons>10 MeV',
		'gamma_contamination'
	] <- 0.07
	table(data[data$study.id == '9-6', c('group.id', 'gamma_contamination')], useNA='ifany')	
	# gamma contamination (fraction)
	# from http://www.jstor.org/stable/pdfplus/3577205.pdf?acceptTC=true
	data$gamma_contamination <- NA
	data[
		data$study.id == '9-6' &
		data$quality == 'neutrons>10 MeV',
		'gamma_contamination'
	] <- 0.07
	table(data[data$study.id == '9-6', c('group.id', 'gamma_contamination')], useNA='ifany')

	# radiation protocol
	# from https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=6
	data$radiation_protocol <- NA
	data[
		data$study.id == '9-6' &
		data$quality == 'gamma-rays Co-60',
		'radiation_protocol'
	] <- 'EULEP'
	table(data[data$study.id == '9-6', c('group.id', 'radiation_protocol')], useNA='ifany')

	# radiation assessment
	# from https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=6
	data$radiation_assessment <- NA
	data[
		data$study.id == '9-6' &
		data$quality == 'gamma-rays Co-60',
		'radiation_assessment'
	] <- 'ionization chamber'
	table(data[data$study.id == '9-6', c('group.id', 'radiation_assessment')], useNA='ifany')
	data[
		data$study.id == '9-6' &
		data$quality == 'neutrons>10 MeV',
		'radiation_assessment'
	] <- 'Shonka plastic A150 Te'
	table(data[data$study.id == '9-6', c('group.id', 'radiation_assessment')], useNA='ifany')
	
	# radiation cage 
	# from http://www.jstor.org/stable/pdfplus/3577205.pdf
	data$radiation_cage <- NA
	data[
		data$study.id == '9-6',
		'radiation_cage'
	] <- 'perspex cage'
	table(data[data$study.id == '9-6', c('group.id', 'radiation_cage')], useNA='ifany')

	# specific pathogen free? 
	# from http://www.jstor.org/stable/pdfplus/3577205.pdf
	data$specific_pathogen_free <- NA
	data[
		data$study.id == '9-6',
		'specific_pathogen_free'
	] <- TRUE
	table(data[data$study.id == '9-6', c('group.id', 'specific_pathogen_free')], useNA='ifany')
	
	# Mock irraited?
	# extrapolated from lack of mention in 
	# http://www.jstor.org/stable/pdfplus/3577205.pdf
	data$mock_irradiated <- NA
	data[
		data$study.id == '9-6' &
		data$quality == 'none (controls)',
		'mock_irradiated'
	] <- FALSE
	table(data[data$study.id == '9-6', c('group.id', 'mock_irradiated')], useNA='ifany')
	
	# Animals per cage
	# from http://www.jstor.org/stable/pdfplus/3577205.pdf
	data$animal_per_cage <- NA
	data[
		data$study.id == '9-6',
		'animal_per_cage'
	] <- 2
	table(data[data$study.id == '9-6', c('group.id', 'animal_per_cage')], useNA='ifany')

	### Vet it ###
	data[
		data$study.id == '9-6',
		'is_vetted'
	] <- TRUE
	table(data[data$study.id == '9-6', c('group.id', 'is_vetted')], useNA='ifany')
		




	#### Start with one ####
	study = '9-8'
	show_groups(study)
	
	# *note this study involved only abdomen exposures, which I
	# discovered while working on it.  I will cease efforst and
	# set to dump to true for the rest of the study
	
	data$to_dump <- NA
	data$to_dump[data$study.id == study] <- TRUE
	
	### Develop concordance ###

## UCL Brussels is the University Catholique de Louvain, belgian's largest French speaking University.
## Leuven a Belgian city only 25 km from Brussels, notable for its university and as the headquarters of Anheuser-Busch InBev and production center of Stella Artois.
		
				
		
		
		
		
	 #### Start with one ####
	 study = '9-5'
	show_groups(study)
	
	### Develop concordance ###

## BALB/c/CNB.  I can't actually find anything on the CNB substrain. I am going to assume that a paper will mention if the CNB denotes something particularly meaningful from a radiation resistance angle.  But BALB/c is an interesting mouse.  For one it produces lots of plasmacytomas, that is a malignant response, to mineral oil injection which somehow helps in the production of mono-clonal antibodies.  They are also rather resistant to mammary tumours but have extra trouble with lung tumors and renal tumors.
## Positron is an electron anti-particle.  It has the same mass an an electron, but the opposite charge.  It was discovered in cloud chambers when observing particles that behaved like electrons but moved the opposite direction under the influence of a magnetic feild.  When an electron collides with a positron they may annihilate one anotherand produce at least two gamma rays.  However, they might simple bounce off one another in an elastic collision.  http://en.wikipedia.org/wiki/Electron%E2%80%93positron_annihilation
## Beta-decay involved the emision of a positron or electron is emitted from an atomic nucleus.  In the case of an electron emission, B-, some neutron in the atomic center becomes a proton and the atomic number increases (though the atomic mass remains the same).  In the case of a positron emmission a proton becomes a neutron and the atomic number decreases.
## CS-137 is a fission product of the nuclear fission of uranium-235.  It has a have life of about 30 years.  It decays by B- emission, releasing an electron and moving it up the atomic scale to Berillium 137 which is highly unstable and releases gamma radiation.  The most interesting factoid about CS-137 is that it has not existed on earth until the launch of the first atomic bombs which sprayed it into the atmosphere.  Now trace amount of CS-137 contaminate the atmosphere making it possible detect if the contents of containers were sealed from the external environment since the bomb blast.  For example we can see if supposed Jeffersonian wine was actually bottled in Jeffersons time.  source: http://en.wikipedia.org/wiki/Caesium-137
## Gy - a unit, notably the LD50 to humans is about 3-6 Gy of ionizing radiation.  With medical intervention LD50 increases to 5-10 Gy.  http://en.wikipedia.org/wiki/Median_lethal_dose
## Paraffin Wax, a long chain hydrocarbon with a flexible melting point either a little above or below room temperature.  Useful because it is resistant to chemical reactions, but can be burned readily.  Its produced when refining oils and is notable amoung other things for its use as a heat sink.  It captures heat by melting (without changing temperature) and releases it on solidifying.  This has been employed in drywall and cooling the electronic of the Lunar Rover.  The wax expansion on melting can also be used to build functional thermostats.  http://en.wikipedia.org/wiki/Wax_thermostatic_element
## Deterministic effects are those that kill above a certain dose threshold, for example the threshold necessary to induce immune system death.  
## Lucite is another name for plexiglas
## Sarcoma is a tumor of the connective tissues, it shares an etymological origin with sarcasm which means literally to tear away flesh.
## Adenoma is a benign tumor originating in a gland, if it becomes maligant it is termed an adenocarcinoma
## Edema refers to swelling due to liquid accumulation.  The origins of the word come from the same as Oedipus, which refers to a swollen foot.
## Glomerulosclerosis is a disease of in which the glomeruli of the kidneys harden, detectable by increased levels of protein in the blood, it is often fatal.  
## Intersticial nephritis is the swelling of areas around the tubules in the kidneys, it has many causes, usually diseases or infection. 
## beam homogeneity is an important concept, apparently a beam of radiation just like a beam of light can vary in intensity and the researchers have to be wary of this.  That can be an important source of study to study variation.
## TE - tissue equivilant
## TE gas - a tissue equivilant gas contains the same portions of hydrogen, nitrogen, carbon, and oxygen as living tissues do.  This can be achieved with N2, O2, H2, and some cabon based gas like methane, ethane, or propane. 
## Telecobalt beam seems to refer to external radiation from a cobalt source (as opposed to internal radiation).  It is generally replaced by accelerated particles in the developed world which achieve a higher beam energy, but is still widely used in developing countries because cobalt beams are cheaper than their alternative.
## CEA Fontenay-aux roses refers to France's Commission for Atomic Energy (and energy alternatives) which is the equivilant of the Department of Energy in America and funds research into energy production.  Fontenay-aux-roses is a suburb 5km south of Paris, site of a fort pre world war II that was converted into a nuclear energy research center.
		
	### Fix problems ###
	
	## They say that mice surviving more than 30 days were utalized.  Does this mean the mice that died earlier were simply discareded?  I should check for deaths within 30 days of irradiation.  http://www.jstor.org/stable/pdfplus/3575971.pdf.  RESOLVED I think all mice are in the data I have.  I see mice that die as early as 30 days old in the dataset and do not see significant differences between groups.  
	quantile(data$lifespan[data$study.id == study], probs=(0:100)/100)
	library(ggplot2)
  ggplot(data[data$study.id == study,],
		aes(x=group.id, y=lifespan, color=group.id)
	) + 
	geom_boxplot()
	

	## 9-5 are listed as strain C57/BL6Bd in the data and as BALB/c/Cnb in the description. RESOLVED.  Description is correct, data is wrong.  source http://www.jstor.org/stable/pdfplus/3575970.pdf?acceptTC=true
	data[
		data$study.id == study,
		'strain'
	] <- 'BALB/c/Cnb'
	table(data[data$study.id == study, c('strain')])

	## 9-5-1 to 13 claim are listed as X-rays in the data and Gamma rays in the study description.  RESOLVED, description is accurate, data is wrong.  See http://www.jstor.org/stable/pdfplus/3575970.pdf?acceptTC=true
	of_interest <- 'quality'
	table(data[,of_interest])
	data[
		data$study.id == study &
		data$quality == 'X-rays whole body',
		of_interest
	] <- 'gamma-rays Co-60'
	table(data[data$study.id == study, c(of_interest)])

	## 9-5-14 is missing its group id in the description table (small detail).  RESOLVED, self evident.
	## 9-5-17 is missing from the description.  RESOLVED, self evident.


	### Check mean lifespans ### 
	
	subset <- data[data$study.id == study,]
	result <- ddply(subset, .(group.id), function(df){
		n <- nrow(df)
		c(
			n=n,
			u=round(mean(df$lifespan)),
			se=round(sd(df$lifespan)/n^0.5)
		)
	})
	result[order(as.numeric(sub(".*-.*-", "", result$group.id))),]

	## 9-5-2 has a different mean lifespan in my data (738) than in table 1 of Maisin (743).  http://www.jstor.org/stable/pdfplus/3575970.pdf?acceptTC=true
	## 9-5-10 has a different mean lifespan in my data (739) than in table 1. of Maisin (747).  http://www.jstor.org/stable/pdfplus/3575970.pdf?acceptTC=true
	## 9-5-19 has a different mean lifespan in my data (598) than in table 1. of Maisin II (605).  http://www.jstor.org/stable/pdfplus/3575971.pdf
	
	# I seem small discrepiancies of less than 10 days in 3 of the
	# 19 groups.  The rest check out completely.  I am inclined to 
	# let these discrepiances slide.
	
	# Notably, I could not reproduce the standard error calculation
	# again!
	
	### Define Treatment Groups ###
	control  <- paste0(study, '-', 1)
	gamma_1  <- paste0(study, '-', 2:7)
	gamma_10 <- paste0(study, '-', 8:13)
	neutron_1<- paste0(study, '-', 14:19)

	### Resolve NAs ###
	
 	# 9-5-1 is a control, has a dose of NA
	# I can update for all cases like this
	data[
		!is.na(data$quality) &
		data$quality == 'none (controls)' &
		is.na(data$dose),
		'dose'
	] <- 0
	
	# study dose rates were consistent across treatment mode
	# Maisin et al 1983 lists the dose rates as 4 Gy/min for
	# gamma radiation.  http://www.jstor.org/stable/pdfplus/3575970.pdf?acceptTC=true
	# Maisin et al 1983 II lists the dose rates as 3 Gy/min
	# for neutron irraddiation.  http://www.jstor.org/stable/pdfplus/3575971.pdf
	
	data[
		data$study.id == study & 
		data$quality == 'gamma-rays Co-60',
		'dose_rate'
	] <- 4
	data[
		data$study.id == study & 
		data$quality == 'neutrons>10 MeV',
		'dose_rate'
	] <- 3
	table(data[data$study.id == study, c('quality', 'dose_rate')], useNA='ifany')	
	
	# study fractions differ by several study groups
	# I define them here and use them to fill in fractions
	# fraction_time, and fraction_interval	
	temp <- function(data, groups, fractions, fraction_interval){
		subset <- data$group.id %in% groups
		data[subset, 'fractions'] <- fractions
		data[subset, 'fraction_interval'] <- fraction_interval
		data
	}
	data <- temp(data, gamma_1, 1, NA)
	data <- temp(data, gamma_10, 10, 60*24)
	data <- temp(data, neutron_1, 1, NA)

	table(data[data$study.id == study, c('fractions', 'group.id')], useNA='ifany')	
	table(data[data$study.id == study, c('fraction_interval', 'group.id')], useNA='ifany')	
		
	
	### Add more data ###
	
	# source 
	# from http://www.jstor.org/stable/pdfplus/3575970.pdf
	# and http://www.jstor.org/stable/pdfplus/3575971.pdf
	data[
		data$study.id == study &
		data$quality == 'gamma-rays Co-60',
		'source'
	] <- 'Cs137'
	data[
		data$study.id == study &
		data$quality == 'neutrons>10 MeV',
		'source'
	] <- 'd(50)+Be'
	
	table(data[data$study.id == study, c('group.id', 'source')], useNA='ifany')	
	
	# modal energy (MeV)
	# from http://www.jstor.org/stable/pdfplus/3575971.pdf
	data[
		data$study.id == study &
		data$quality == 'neutrons>10 MeV',
		'modal_energy'
	] <- 23
	table(data[data$study.id == study, c('group.id', 'modal_energy')], useNA='ifany')	
	
	# gamma contamination (fraction)
	# from https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=5
	data[
		data$study.id == study &
		data$quality == 'neutrons>10 MeV',
		'gamma_contamination'
	] <- 0.05
	table(data[data$study.id == study, c('group.id', 'gamma_contamination')], useNA='ifany')	
	# radiation protocol
	# from https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=5
	data[
		data$study.id == study &
		data$quality == 'gamma-rays Co-60',
		'radiation_protocol'
	] <- 'EULEP'
	table(data[data$study.id == study, c('group.id', 'radiation_protocol')], useNA='ifany')

	# radiation assessment
	# from https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=5
	data[
		data$study.id == study &
		data$quality == 'gamma-rays Co-60',
		'radiation_assessment'
	] <- 'ionization chamber'
	table(data[data$study.id == study, c('group.id', 'radiation_assessment')], useNA='ifany')
	data[
		data$study.id == study &
		data$quality == 'neutrons>10 MeV',
		'radiation_assessment'
	] <- 'Shonka plastic A150 Te'
	table(data[data$study.id == study, c('group.id', 'radiation_assessment')], useNA='ifany')
	
	# radiation cage 
	# from http://www.jstor.org/stable/pdfplus/3575971.pdf
	# and http://www.jstor.org/stable/pdfplus/3575970.pdf
	data[
		data$study.id == study &
		data$quality == 'neutrons>10 MeV',
		'radiation_cage'
	] <- 'perspex cage'
	data[
		data$study.id == study &
		data$quality == 'gamma-rays Co-60',
		'radiation_cage'
	] <- 'lucite cage'
	table(data[data$study.id == study, c('group.id', 'radiation_cage')], useNA='ifany')

	# specific pathogen free? 
	# from http://www.jstor.org/stable/pdfplus/3577205.pdf
	# in the study cited, which came after this one, they note
	# the switch to pathogen free mice.  Therefore its safe to
	# assume they were not using pathogen free mice at this point
	#
	# I also find no mention of the term in the other sources.
	# from http://www.jstor.org/stable/pdfplus/3575971.pdf
	# and http://www.jstor.org/stable/pdfplus/3575970.pdf
	data[
		data$study.id == study,
		'specific_pathogen_free'
	] <- FALSE
	table(data[data$study.id == study, c('group.id', 'specific_pathogen_free')], useNA='ifany')
	
	# Mock irraited?
	# we can assume they weren't, its not mentioned in either
	# of the sources and the same controls were used for both
	# neutron irradiated and gamma irradiated mice which were
	# subject to different radiation protocols.
	#
	# from http://www.jstor.org/stable/pdfplus/3575971.pdf
	# and http://www.jstor.org/stable/pdfplus/3575970.pdf
	data[
		data$study.id == study &
		data$quality == 'none (controls)',
		'mock_irradiated'
	] <- FALSE
	table(data[data$study.id == study, c('group.id', 'mock_irradiated')], useNA='ifany')
	
	# Animals per cage
	# from http://www.jstor.org/stable/pdfplus/3575971.pdf
	# and http://www.jstor.org/stable/pdfplus/3575970.pdf
	data[
		data$study.id == study,
		'animal_per_cage'
	] <- 2
	table(data[data$study.id == study, c('group.id', 'animal_per_cage')], useNA='ifany')

	## Vet it ###
	data[
		data$study.id == study,
		'is_vetted'
	] <- TRUE
	table(data[data$study.id == study, c('group.id', 'is_vetted')], useNA='ifany')


		
		


	 #### Start with one ####
	 study = '9-7'
	show_groups(study)
	
	### Develop concordance ###
## Crookes tube a precursur to the vacuum tube was used in the discovery of cathode rays (streams of electrons) and xrays.  The device consists of glass, nearly evacuated, but with a small amount of air sufficient to contribute an even smaller number of positivively charged ions which always exist in air.  A high voltage differntial is created (somehow) between the two nodes at opposite ends of the glass.  The positive ions go to the negative pole and strike it hard enough to release electrons which fly towards the positive pole creating a visible cathode ray.  This ray is visible because some of the electrons collide violently with the residual air and cause it to florecese.  Other electrons miss all the air entirely, and the positive node and instead strike the back of the glass causing it to florese.  Under the influence of a magnet this beam will be bent which is what led JJ Thompson to the relazation that the cathode ray was composed of small negatively charged elements.  http://en.wikipedia.org/wiki/Crookes_tube
## Xrays were discovered using the Crooke's tube.  Under high enough voltage, electrons are accelerated to sufficient speeds that they cause the release of xrays when they collide with the back glass, this happens when they raise electrons out of inner shell orbitals (xray florescence) or because they curve so sharply when passing close to an atomic nucleus.  These rays were detected by Roentgen who saw a florscent glow on teh wall behind his crooke tube which was capable of passing through objects like paper.  Investigating these rays he discovered the xray which won him the first ever nobel prize for physics.  http://en.wikipedia.org/wiki/Crookes_tube#The_discovery_of_X-rays
## Thermoinic emission is the release of electrons from a solid (metal) into air due to high temperatures.  The temperature when very high (1000K) exceeds the binding energy of the metal to the electrons and they 'boil' away.  This was observed when it was noted that a positivley charged metal sphere lost its charge when heated sufficiently. By contrast a negatively charged sphere will retain its charge when heated to the same levels.  This property can be employed in vacuum tubes to provide electrons for a current in place of small quantities of air that were employed in the Crookes tube.
## Why vacuums?  Vacuum tubes are necessary to generate high energy emmissions because they give electrons a long distance to accelerate un-obstructed.  A tube filled with air would have too many collisions to allow electrons to accelerate to the speeds necessary to cause high energy particles to emerge. 
## Xray tubes.  Very similar to Crooke's tubes but differ because they have a more complete vacuum and electrons are released by thermoinic emissions (heated metals).  The xrays are produced when the electrons strike the positive node and the quality of the xrays is determined by the distance that they travel and the material that composes the positive anode. http://en.wikipedia.org/wiki/X-ray_tube
## kPv, or Peak Kilovoltage is the maximum voltage applied across and xray tube.  Along with the length of the tube and the quality of the positive node, it will determine the energy spectrum of the resultant xrays.  http://en.wikipedia.org/wiki/Peak_kilovoltage
## Kerma, or kinetic energy released in material, is the sum of all of the kinetic energies of charged particles that are released by an uncharged radioactive particle interacting with a substance.  It differe from absorbed dose because it does not account for energy re-emmited from the body that absorbed.  However, somewhat annoyingly, it is measured in grays (J/Kg) just like absorbed dose.  It will be useful to watch out for situations where kerma might be mixed with absorbed dose.  I assume that most of our quality measurements are on absorbed dose, but I don't know for certain.  http://en.wikipedia.org/wiki/Kerma_(physics)
## Roentgen is an outdated unit that measures radiation using kerma as opposed to absorbed dose.  The trouble with this approach is that absorbed dose may vary substantially even with the same dose in roentgens based on the characteristics of the incident beam.  For example in bone, one roentgen (air keram) of xrays may deposit between 0.01 and 0.04 Gy of absorbed dose.
## Homegeneity Factor, or HF, refers to how many different wavelenghts are included in a beam.  It is measured by the ratio of the width of the first half value layer divided by the width of the second half value layer.  The first half value layer will intrinsically be smaller than the second one because it is biased in the radiation it stops, essentially stopping the easy to stop radiation.  However, if the source is monochromatic, then there is not difference in how easy each photon is too stop and the size of the second half value layer will be exactly the same as the first.  http://en.wikipedia.org/wiki/Half-value_layer
## Beam Hardening refers to the fact that the average wavelength of photons in a beam of radiation increases as the beam passes through blocking substances.  The reason for this is that low energy photons are more easily disrupted by the stopping material.
## Cupping artifacts occur because of beam hardening.  Essentially thick/dense portions of a sample attenuate a beam less than would be expected by the thinner portions because they are hardened.  Basically somethign 2x as thick will not cause a 2x decline in transmission unless the transmission rate is already very high. http://radiopaedia.org/articles/beam-hardening-phenomenon
## HVL the half value layer needed to attenuate the intesity of radiation (or air kerma rate) by 1/2
## non-neoplastic refers to diseases which do not involve the growth of new tissues, e.g. not tumors
## Hepato refers to the liver, its greek origin is hepat or hepar which is related to the word for repairing or mending because the liver is good at regenerating itself
## Sarcoma, Leukemias, Carcinoma, vs Lymphoma.  These are the basic types of cancer.  Carcinoma is most common and results in spreading tumors nearly anyhwere.  Leukemias are in the blood and do not usually produce solid tuomrs.  Sarcomas are in connective tissues and usually develop as spherical tumors and spread through bones.  Lymphomas infect and spread using the lymph system.  In a sense we can think of these different types of cancer as those that spread through different sets of tissues.  http://www.your-cancer-prevention-guide.com/cancer-classifications.html
## Actinides are elements in the bottom row of the periodic table from Actenide (89) to lawrencium (103).  They are usually set off below the table for formatting reasons.  Of them only Thorium, Uranium, and Plutonium exist naturally.  These are highly toxic materials.  For example a nickle size portion of plutonium (5000 mg) could kill more than 10 people (22 mg is LD50 for a 70kg person).  http://en.wikipedia.org/wiki/Actinide
## European Commission is the executive branch of the European Union, unlike the american executive branch it consits of a comittee with one representative of each of the 27 nations that make up the EU.  http://en.wikipedia.org/wiki/European_Commission
## Directorate General is the name of a body that works for the European Comission on some specific task like aggriculture or buisness.  I belive the equivilant in America are things like the NIH, DOE, etc. http://en.wikipedia.org/wiki/European_Civil_Service#Directorates-General
## Joint research center is a Directorate General of the European Union involved in various research concerns, most of them related to energy production and safety.  http://en.wikipedia.org/wiki/Joint_Research_Centre
## The Institute for Reference Materials and Measurements (IRMM) works on reliable measurement tools and techniques as a subdivision of the Joint Research Center.  They seem to be similar to NIST in America.  Before 1993 they were known as the central bureau for nuclear measurements, reflecting a mission more singularly isolated on nuclear energy concerns.  http://en.wikipedia.org/wiki/Institute_for_Reference_Materials_and_Measurements
## The 'thimble' ionization chamber, seems to be a very small ionization chamber with the trademark name thimble http://bit.ly/Xcr7JX
## Lymphoblast is an immature lymphoctye (T cells or B cells).  It can be thought of as a t-cell stem cell.  http://en.wikipedia.org/wiki/Lymphoblast
## Angioma a benign growth of blood cells
## Parenchyma is the bulk of an organ, its functional parts as opposed to its structural parts.  It comes from the word parenkhein, which means to pour in.  http://en.wikipedia.org/wiki/Parenchyma  
## Septum is a wall that devides two things.  It shares a false origin with the word septic which might refer to a system running between walls and is a helpful mneumonic.  Aveolar septum (the reason I looked the word up) refer to the spaces between aveoli.  http://en.wikipedia.org/wiki/Septum
## Mononuclear leukocytes aka agranualocytes are white blood cells characterized by a one lobed nucleus.  They are composed of monocysetes and lymophocytes and comprise about 35% of all white blood cells. http://en.wikipedia.org/wiki/Agranulocyte
## Monocytes ar composed of macrophages and dendritic cells.  Collectively they consume cells and pathogens and present antigens for identification by other immune cells.  
## Atelectasis means incomplete extension and refers to the symptom where aveoli are collapsed.  This can be an acute or chronic condition.  http://en.wikipedia.org/wiki/Atelectasis
## Emphasyma involves the break down of lung tissues so that aveoli essentially merge and lose the elastic property necessary to support exhalation. http://en.wikipedia.org/wiki/Emphysema
## Inclusion bodies are aggregated protien masses surrounded by a lipid membrane in the cytoplasm of a cell.  They can be induced by the expression of foreign DNA which creates foreign proteins that might aggregate in an inclusion body.  In the paper I am reading they mention inclusion bodies in cancer cells, I don't see any mention of that in wikipedia.  Perhaps this is an indication of viral infection in these cells?  http://en.wikipedia.org/wiki/Inclusion_body
## Cox-Mantel or the log rank test is a non-parametric test that asks whether two survival curves are different.  In essence it looks at any given time point and compares the % of animals that died in one group (of those that were still alive) vs the % of animals that died both groups.  It estimates the reliability of these values using the binomial distribution.  Then the sum of the discrepiancy between one group and the whole sample is divided by the sum of the variance (summed across time points that is) to produce a Z value that indicates if the group deviates significantly from the whole population. http://en.wikipedia.org/wiki/Logrank_test
## Breslows method is a test of the likelihood that a coefficient is significant in the proportional hazards model.  http://en.wikipedia.org/wiki/Proportional_hazards_models  
## Peto test - ???
## Hormesis of non-neoplastic lung disease?  In 9-7 non-neoplastic lung disease was the primary cause of death, but seemed to be reduced in animals that were exposed to low doses of radiation.  The authors note that cancer incidence still increased, but its interseting that other effects might have diminished.  Quite odd.  http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
		
	### Fix problems ##

	# No problems found

	### Check mean lifespans ### 
	
	subset <- data[data$study.id == study,]
	result <- ddply(subset, .(group.id), function(df){
		n <- nrow(df)
		c(
			n=n,
			u=round(mean(df$lifespan)),
			se=round(sd(df$lifespan)/n^0.5)
		)
	})
	result[order(as.numeric(sub(".*-.*-", "", result$group.id))),]

	# These checked out, I saw a few small discrepiancies, nothing
	# greater than 5 days.  Notably the standard error actually 
	# was correct this time.  Also when they reported standard errors
	# from previous experiments, they listed numbers much lower than
	# what were seen in the tables of 9-6, 9-5.  I can only assume
	# they have fixed their damnable ways.
	
	
	### Define Treatment Groups ###
	control   <- paste0(study, '-', 1)
	neutron_7 <- paste0(study, '-', 2:5)
	neutron_21<- paste0(study, '-', 6:9)
	xray_7    <- paste0(study, '-', 10:12)
	xray_21   <- paste0(study, '-', 13:15)


	### Helper Functions ###
	tableNA <- function(...) table(..., useNA='ifany')
	group_table <- function(data, study, outcome){
		tableNA(data[data$study.id == study, c('group.id', outcome)])
	}


	### Resolve NAs ###
	
	# study dose rates were consistent across treatment qulity
	# 1 Gy/min for xrays and 0.04 Gy/min
	# http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true	
	data[
		data$study.id == study & 
		data$quality == 'neutrons 1-10 MeV',
		'dose_rate'
	] <- 0.04
	data[
		data$study.id == study & 
		data$quality == 'X-rays whole body',
		'dose_rate'
	] <- 1
	group_table(data, study, c('quality'))
	
	# radiation fractions were always acute	
	data[
		data$study.id == study &
		data$group.id != control,
		'fractions'
	] <- 1
	group_table(data, study, c('fractions'))
			
	
	### Add more data ###
	
	# source 
	# from http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data[
		data$study.id == study &
		data$group.id %in% c(neutron_7, neutron_21),
		'source'
	] <- 'd(50)+Be'
	group_table(data, study, c('source'))
	
		
	# modal energy (MeV)
	# from http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data[
		data$study.id == study &
		data$group.id %in% c(neutron_7, neutron_21),
		'modal_energy'
	] <- 3.1
	group_table(data, study, c('modal_energy'))
	
	# gamma_contamination (fraction)
	# unspecified
	# data[
		# data$study.id == study &
		# data$quality == 'neutrons>10 MeV',
		# 'gamma_contamination'
	# ] <- 0.05
	# group_table(data, study, c('gamma_contamination'))
	
	# radiation protocol
	# from http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data[
		data$study.id == study &
		data$group.id %in% c(xray_7, xray_21),
		'radiation_protocol'
	] <- 'EULEP'
	group_table(data, study, c('radiation_protocol'))

	# radiation assessment
	# from http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data[
		data$study.id == study &
		data$group.id %in% c(xray_7, xray_21),
		'radiation_assessment'
	] <- 'philips ionization chamber'
	data[
		data$study.id == study &
		data$group.id %in% c(neutron_7, neutron_21),
		'radiation_assessment'
	] <- '0.53 cm^3 thimble ionization chamber'
	group_table(data, study, c('radiation_assessment'))
	
	# radiation cage 
	# from http://www.jstor.org/stable/pdfplus/3575971.pdf
	# and http://www.jstor.org/stable/pdfplus/3575970.pdf
	data[
		data$study.id == study &
		data$group.id %in% c(xray_7, xray_21, neutron_7, neutron_21),
		'radiation_cage'
	] <- 'perspex cage'
	group_table(data, study, c('radiation_cage'))

	# specific pathogen free? 
	# http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data[
		data$study.id == study,
		'specific_pathogen_free'
	] <- TRUE
	group_table(data, study, c('specific_pathogen_free'))
	
	# Mock irraited?
	# http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	# They say that they were indeed.
	data[
		data$study.id == study &
		data$group.id %in% c(control),
		'mock_irradiated'
	] <- TRUE
	group_table(data, study, c('mock_irradiated'))
	
	# Animals per cage
	# http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data[
		data$study.id == study,
		'animal_per_cage'
	] <- 5
	group_table(data, study, c('animal_per_cage'))

	# controls_added_continually
	data$controls_added_continually[
		data$study.id == study &
		data$group.id %in% control
	] <- TRUE
	group_table(data, study, c('controls_added_continually'))

	# decomposed or cannibalized
	# they say less than 1% 
	# I am being conservative and setting the value to 1
	# http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data$decomposed_or_cannibalized[
		data$study.id == study
	] <- 0.01
	group_table(data, study, 'decomposed_or_cannibalized')

	# dose undertainty (fraction)
	data$dose_uncertainty[
		data$study.id == study &
		data$group.id %in% c(neutron_7, neutron_21)
	] <- 0.07
	group_table(data, study, 'dose_uncertainty')

	# kPv (peak kilovoltage)
	# this is valid for xray exposures
	# http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data$kpv[
		data$study.id == study &
		data$group.id %in% c(xray_7, xray_21)
	] <- 250
	group_table(data, study, 'kpv')


	# HVL (half voltage layer)
	# this is valid for xray exposures
	# http://www.jstor.org/stable/pdfplus/3579307.pdf?acceptTC=true
	data$hvl[
		data$study.id == study &
		data$group.id %in% c(xray_7, xray_21)
	] <- '2mm Cu'
	group_table(data, study, 'hvl')
	
	
	## Vet it ###
	data[
		data$study.id == study,
		'is_vetted'
	] <- TRUE
	group_table(data, study, 'is_vetted')




	 #### Start with one ####
	 study = '9-4'
	
	### Develop concordance ###
## Radioprotection works by reducing oxygen levels in cells through some mechanism which then induces the well known hypoxic resistance response.  In practice, such pretreatments can increase LD50 by a little less than 2 fold.  This is a theoretically interesting result, but not large enough to be all that useful in, for instance, a combat zone.  Also the compounds employed have bad side effects, which would be expected when creating a hypoxic shock.  A better solution in most cases would be to increase sheilding.  As we know, a couple mm of copper can provide 50% sheilding and thus have the same effect as these radioprotective agents.  http://www.informatics.jax.org/greenbook/chapters/chapter22.shtml (see pretreatment with chemical agents).
## Peritoneum refers to the body cavity, the word itself is derived from the greek word for the abdominal membrane which can be decomposed into around (peri) stretched (teinein).  You might say the part inside the strechedaround membrane (or some nonsense).  A pertoneum injection refers to an injection direcly into the abdomen.  Its nice and easy and can be considered in contrast to an intravaenous injection. http://www.etymonline.com/index.php?search=peritonaeum
## Isologous - having the same genotype.  http://medical-dictionary.thefreedictionary.com/isologous
## They note in this paper that an injection of isologous bone marrow post irradiation is almost as good as administering a radioprotector up to 500R, amazing!  Turns out that this is the basic principal justifying the need for bone marrow transplanets, i.e. they allow for more aggressive radiotherapy. http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
## Reticular fibers are collagen fibers that serve as connective tissues within organs, between cells.  http://en.wikipedia.org/wiki/Reticular_fiber
## Myeloid cell has several meetings, but amount them it is any blood cell that is not a lymphocyte (precursurs to b cells, t cells, and natural killer cells.)
## Why do kidneys respond to radiation?  I can't find a clear explanation, but it appears that they do respond by hardening, fractionating the dose does not seem to help.  I wonder why?  http://books.google.com/books?id=6HhjwRyqBzgC&q=nephrosclerosis#v=onepage&q=kidney&f=false
## Papilloma is a small (i.e. human papilloma virus).  It derives from the word papilla which means nipple and is related to the word for swelling. http://www.etymonline.com/index.php?term=papilla&allowed_in_frame=0
## Why do BALB/c mice have a higher incidence of tumors than c57bl mice?  Its not clear to me why.  But they seem to be substantially different, ~18% leukemia and 35% carcinoma in BALB/c and only 12% leukemia and 1.5% carcinoma in c57bl mice.  http://www.jstor.org/stable/pdfplus/3574859.pdf?acceptTC=true
## Full width at half maximum - the width of a function between the two points on either side of the function maxiumum that have examactly 1/2 the value of the function maximum. http://en.wikipedia.org/wiki/Full_width_at_half_maximum
## A tungsten target is commonly used in xray tubes because it produces a nice quality xray for reasons I don't understand.  The tungsten also has a tendency to melt and must be rotated to remain fresh.  http://uk.answers.yahoo.com/question/index?qid=20100111070206AAqb4T7
## Inherent filtration is that part of radiation beam filtration which in inevetiable as the beam must escape from the xray tube and pass through some medium which will filter it.  http://www.ndt-ed.org/EducationResources/CommunityCollege/Radiography/Physics/filters.htm
## Ripple (electricity) refers to the fact that AC to DC conversion ineviatibly leaves behind a variability in voltage which moves pendulously around the mean voltage.  

	### Check mean lifespans ### 
	
	subset <- data[data$study.id == study,]
	result <- ddply(subset, .(group.id), function(df){
		n <- nrow(df)
		c(
			n=n,
			u=round(mean(df$lifespan)),
			se=round(sd(df$lifespan)/n^0.5),
			dose=df$dose[1],
			strain=df$strain[1]
		)
	})
	result[order(as.numeric(sub(".*-.*-", "", result$group.id))),]	
	
	### Define Treatment Groups ###
	# first batch defined from
	# http://www.jstor.org/stable/pdfplus/3574859.pdf?acceptTC=true
	balb_control	<- paste0(study, '-', 1)
	balb_xray 	<- paste0(study, '-', 2:8)
	balb_x_aet 	<- paste0(study, '-', 10:13)
	balb_x_mix 	<- paste0(study, '-', c(14:16, 18:21))
	#
	# second batch defined from
	# http://www.jstor.org/stable/pdfplus/3575315.pdf?acceptTC=true
	c57_control	<- paste0(study, '-', 24)
	c57_xray	<- paste0(study, '-', 25:26)
	c57_x_mix	<- paste0(study, '-', 27:28)
	c57_xray_4<- paste0(study, '-', c(29:31, 33:35))
	c57_x4_mix<- paste0(study, '-', c(36:39, 41:42))

	# Be sure there are just the right amount
	all <- c(
		balb_control, balb_xray, balb_x_aet, balb_x_mix, 
		c57_control, c57_xray, c57_x_mix, c57_xray_4, c57_x4_mix
	)
	groups <- unique(data$group.id[data$study.id == study])
	groups[!groups %in% all]
	all[!all %in% groups]

	### Helper Functions ###
	tableNA <- function(...) table(..., useNA='ifany')
	group_table <- function(data, study, outcome){
		tableNA(data[data$study.id == study, c('group.id', outcome)])
	}	
	
	### Fix problems ##
	
	## 9-4-1 to 23 are listed as C57BL/Cnb in the data and BALB/c/Cnb in the description.  Notably these have different LD50/30s (whatever that means).  RESOLVED description is accurate and data is wrong.  These mice should be listed as BALB/c/Cnb.  Source http://www.jstor.org/stable/pdfplus/3574859.pdf?acceptTC=true
	balbs <- c(balb_control, balb_xray, balb_x_aet, balb_x_mix)
	data[
		data$study.id == study &
		data$group.id %in% 	balbs,
		'strain'
	] <- 'BALB/c/Cnb'
	group_table(data, study, 'strain')
	 
	## 9-4-22 does not appear in the two treatment data, though the description claims it should have recieved xrays and 5ht.  RESOLVED 9-4-22 is not the in the data.  Take no action.

	## 9-4-9 should be listed as reciving both xrays and AET according to the description, but doesn't appear in the two treatment data.  RESOLVED 9-4-9 does not expist in the data.
	
	## 9-4-9, 17, 22, 23, and 32 are missing from study 9-4.  In the description this is only labeled for 23 and 32.  RESOLVED the table needs correcting.
	
	
	## 9-4 radiation is expressed in Roengens.  RESOLVED I am converting expression to Gy using an F factor of 0.0094.  Values close to this are widely cited on the internet.  I found a study that measured f-factors for various tissues.  Referencing figure 3 we can see that 0.0094 is a good estimate for muscle deposites when the HVL is 0.7mm Cu (as in this study).  The error in this measure is likely to be smaller than the uncertainty in the beam quality listed.  However, it is probably worth mentioning this change explicitly in the methods.  http://iopscience.iop.org.turing.library.northwestern.edu/0031-9155/32/5/005/pdf/0031-9155_32_5_005.pdf
	f_factor <- 0.0094
	data$dose <- as.numeric(data$dose)
  # ERROR - NA's caused by conversion
	data$dose[data$study.id == study] <- f_factor * data$dose[data$study.id == study]
	group_table(data, study, 'dose')
	
	# Note the conversion
	data[
		data$study.id == study & 
		data$quality != 'none (controls)',
		'converted_to_grays'
	] <- TRUE
	group_table(data, study, 'converted_to_grays')

	
	### Check Cluster Assignments ##
	data$cluster[
		data$group.id %in% c(balb_control, balb_xray)
	] <- '9-4-1'
	data$cluster[
		data$group.id %in% c(balb_x_aet)
	] <- '9-4-2'
	data$cluster[
		data$group.id %in% c(balb_x_mix)
	] <- '9-4-3'
	
	data$cluster[
		data$group.id %in% c(c57_control, c57_xray, c57_xray_4)
	] <- '9-4-4'
	data$cluster[
		data$group.id %in% c(c57_x_mix, c57_x4_mix)
	] <- '9-4-5'
	group_table(data, study, 'cluster')


	### Resolve NAs ###
	
	show_groups(study)
	
	# study dose rates were consistent across treatment qulity
	# 100 R/min
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	data[
		data$study.id == study & 
		data$quality != 'none (controls)',
		'dose_rate'
	] <- 100 * f_factor
	group_table(data, study, c('dose_rate'))
	
	# radiation fractions were either acute or 4
	# http://www.jstor.org/stable/pdfplus/3575315.pdf?acceptTC=true
	data[
		data$study.id == study &
		data$group.id %in% c(
			balb_xray, balb_x_aet, balb_x_mix, 
			c57_xray, c57_x_mix
		),
		'fractions'
	] <- 1
	data[
		data$study.id == study &
		data$group.id %in% c(c57_xray_4, c57_x4_mix),
		'fractions'
	] <- 4
	group_table(data, study, c('fractions'))
			
			
	# fraction_interval
	# (weekly)
	# http://www.jstor.org/stable/pdfplus/3575315.pdf?acceptTC=true
	data[
		data$study.id == study &
		data$group.id %in% c(c57_xray_4, c57_x4_mix),
		'fraction_interval'
	] <- 1*7*24*60 # weeks * days/week * hours/day * minues/hours	
	group_table(data, study, c('fraction_interval'))


	### Add more data ###


	## Add age at assignment should be, 30 days post irradiation.  Because they removed mice that died of acute effects.  Source http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	# good general default
	data$assignment_age <- data$age.at.treatment
	################ NOOOTE ######################
	# remove above on copy #
	s <- data$study.id == study
	data$assignment_age[s] <- 30 + data$assignment_age[s]
	group_table(data, study, c('assignment_age'))
		
	# source 
	# from http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	data[
		data$study.id == study &
		! data$group.id %in% c(c57_control, balb_control),
		'source'
	] <- 'xray'
	group_table(data, study, c('source'))

		
	# modal energy (MeV)
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	# data[
		# data$study.id == study &
		# data$group.id %in% c(neutron_7, neutron_21),
		# 'modal_energy'
	# ] <- 3.1
	# group_table(data, study, c('modal_energy')) 


	# gamma_contamination (fraction)
	# unspecified
	# data[
		# data$study.id == study &
		# data$quality == 'neutrons>10 MeV',
		# 'gamma_contamination'
	# ] <- 0.05
	# group_table(data, study, c('gamma_contamination'))
	
	# radiation protocol
	# https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=4
	data[
		data$study.id == study &
		! data$group.id %in% c(c57_control, balb_control),
		'radiation_protocol'
	] <- 'EULEP'
	group_table(data, study, c('radiation_protocol'))


	# radiation assessment
	# https://era.bfs.de/studies_description_from_db.php?LabId=9&StudyId=4
	data[
		data$study.id == study &
		! data$group.id %in% c(c57_control, balb_control),
		'radiation_assessment'
	] <- 'philips ionization chamber'
	group_table(data, study, c('radiation_assessment'))

	
	# radiation cage 
	# from http://www.jstor.org/stable/pdfplus/3575971.pdf
	# and http://www.jstor.org/stable/pdfplus/3575970.pdf
	data[
		data$study.id == study &
		! data$group.id %in% c(c57_control, balb_control),
		'radiation_cage'
	] <- 'lucite cage'
	group_table(data, study, c('radiation_cage'))

	# specific pathogen free? 
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	# deduced by omission (and later reference to switching)
	data[
		data$study.id == study,
		'specific_pathogen_free'
	] <- FALSE
	group_table(data, study, c('specific_pathogen_free'))

	# Mock irraited?
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	# Deduced by omission and reference in a later paper
	data[
		data$study.id == study &
		data$group.id %in% c(c57_control, balb_control),
		'mock_irradiated'
	] <- FALSE
	group_table(data, study, c('mock_irradiated'))
	
	# Animals per cage
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	data[
		data$study.id == study,
		'animal_per_cage'
	] <- 2
	group_table(data, study, c('animal_per_cage'))

	# controls_added_continually
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	# deduced through omission (and an explicit reference in a later
	# paper)
	data[
		data$study.id == study &
		data$group.id %in% c(c57_control, balb_control),
		'controls_added_continually'
	] <- FALSE
	group_table(data, study, c('controls_added_continually'))

	# decomposed or cannibalized
	# they say less than 1% 
	# I am being conservative and setting the value to 1
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	data[
		data$study.id == study,
		'decomposed_or_cannibalized'
	] <- 0.01
	group_table(data, study, 'decomposed_or_cannibalized')

	# dose undertainty (fraction)
	# data$dose_uncertainty[
		# data$study.id == study &
		# data$group.id %in% c(neutron_7, neutron_21)
	# ] <- 0.07
	# group_table(data, study, 'dose_uncertainty')

	# kPv (peak kilovoltage)
	# this is valid for xray exposures
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	data[
		data$study.id == study &
		!data$group.id %in% c(c57_control, balb_control),
		'kpv'
	] <- 250
	group_table(data, study, 'kpv')

	# HVL (half voltage layer)
	# this is valid for xray exposures
	# http://www.jstor.org/stable/pdfplus/3574717.pdf?acceptTC=true
	data[
		data$study.id == study &
		!data$group.id %in% c(c57_control, balb_control),
		'hvl'
	] <- '0.7mm Cu'
	group_table(data, study, 'hvl')
	
	## Double check that other treatments within clusters are identical
	table(data[
		data$study.id == study,
		c('cluster', 'other_treatments') 
	])
	group_table(data, study, c('cluster', 'other_treatments'))
	
	## All, add method of conversion from Roentgens to Gray to my general methods.  Read the following comments in 9-4 for details


	## Vet it ###
	data[
		data$study.id == study,
		'is_vetted'
	] <- TRUE
	group_table(data, study, 'is_vetted')


	# Save
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.5.rds')


# Results
#
# I went through several studies using the above technique, but 
# found it too slow and cumbersome.  I will move forward in a more
# targeted way, focused on cleanup relevant to DDREF estimates,
# that is, with a focus on low-LET radiation and lifespan outcomes.


#############################################################
#
# Simpler Cleanup
#
# The above cleanup technique was too slow, now I will hone
# it specifically for the problem of estimating ddref which
# requires a focus on low-LET studies and lifespan outcomes
# I will ignore other factors for the time being.


  # Libraries
	library(plyr)

	# Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.5.rds')
	
	# Extend Data
	data$exclude <- FALSE
	data$reason <- ""
	data$warning <- FALSE
	data$warning_reason <- ""
		
	# Configuration
	relevant_columns <- c(
		"id",
		"group.id",
		"study.id",
		"cluster",
		"species",
		"strain",
		"sex",
		"lifespan",
		"quality",
		"unit",
		"dose",
		"dose_rate",
		"fractions",
		"fraction_time",
		"age.at.treatment",
		"assignment_age",
		"remarks",
		"is_vetted",
		"group.name",
		"n",
		"in_era",
		"endpoints",
		"lifestage.at.treatment",
		"fraction_interval",
		"other_treatments",
		"cause_of_death",
		"source",
		"exclude",
		"reason",
		"warning",
		"warning_reason"
	)
	other_treatment_columns <- c(
		"age.at.treatment.1",
		"treatment.1",
		"dose.1",
		"unit.1",
		"application.1",
		"remarks.1",
		"age.at.treatment.2",
		"treatment.2",
		"dose.2",
		"unit.2",
		"application.2",
		"remarks.2",
		"age.at.treatment.3",
		"treatment.3",
		"dose.3",
		"unit.3",
		"application.3",
		"remarks.3"
	)
	numeric_columns <- c(
		"lifespan",
		"dose",
		"dose_rate",
		"age.at.treatment",
		"assignment_age",
		"MAS"
	)
	## 9-4 radiation is expressed in Roengens.  Convert expression to Gy using an F factor of 0.0094.  Values close to this are widely cited on the internet.  Also figure 3 of http://bit.ly/11vzNe8 shows that the F factor does not deviate substantially from  0.0094.  
	f_factor <- 0.0094
	
	# Helpers
	trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	find_in_file <- function(pattern, file='exp/radiation.R'){
		lines <- trim(readLines(file))
		cat(paste0(lines[grepl(pattern, lines)], collapse='\n'))
	}
	get_study_data <- function(study_id){
		d <- data[data$study.id == study,relevant_columns]
		d$MAS <- d$lifespan - d$age.at.treatment
		d
	}
	get_study_treatments <- function(study_id){
		data[data$study == study,other_treatment_columns]
	}	
	paste_unique <- function(x) paste(unique(x), collapse=' ')
	summarize_numeric <- function(x){
		n <- length(x)
		na_message <- ''
		if(any(is.na(x))){
			na_message <- paste0(' missing ', sum(is.na(x)))
		}
		paste0(
			round(mean(x, na.rm=T), 1),
			' +/- ',
			round(sd(x, na.rm=T), 1),		# /n^0.5
			na_message
		)
	}
	id_order <- function(id) order(as.numeric(gsub('^.*-', '', id)))
	summarize_study_data <- function(study_data){
		summary <- ddply(study_data, .(group.id), function(df){
			numeric <- names(df) %in% numeric_columns
			data.frame(
				n=nrow(df),
				llply(df[,!numeric], paste_unique),
				llply(df[,numeric], summarize_numeric)
			)
		})	
		summary <- summary[id_order(summary$group.id),]	
		rownames(summary) <- summary$group.id
		summary
	}
	get_numeric_factor <- function(...){
		as.numeric(as.factor(paste(...)))
	}
	print_for_copy <- function(x) cat(paste0(x, collapse='",\n"'))
	add_exclusions <- function(exclusions, data){
		for(ex in exclusions){
			who_new <- ex$who & !data$exclude
			data$exclude[ex$who] <- TRUE
			data$reason[who_new] <- ex$why
		}
		data
	}
	add_warnings <- function(warnings, data){
		for(w in warnings){
			who_new <- w$who & !data$warning
			data$warning[w$who] <- TRUE
			data$warning_reason[who_new] <- w$why
		}
		data
	}
	table0 <- function(...) table(..., useNA='ifany')
	collapse_paste <- function(...) paste(..., collapse=' ')
	paste_columns <- function(df) unlist(alply(df, 1, collapse_paste))


	###
	# Pick a study
	study <- '1003-21'
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Read title/basic description
	# List sources
	# anl-95-3.pdf JM-3
	# Check for lifespan data
	
	# Filter
	filter <- with(study_data,		
		(quality == 'gamma-rays whole body' | is.na(quality)) &
		dose < 1.5
	)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
	
	# Check for existing problems
	find_in_file(study)
	
	# Fix problems
  study_data$quality[is.na(study_data$quality)] <- 'none (controls)'
	groups <- with(study_data, paste(quality, dose, sex))
	print_for_copy(sort(unique(groups)))
	ordered_groups <- c(
		"none (controls) 0 Male",
		"none (controls) 0 Female",
		"gamma-rays whole body 0.8631 Male",
		"gamma-rays whole body 0.8631 Female",
		"gamma-rays whole body 1.3714 Male",
		"gamma-rays whole body 1.3714 Female"
	)
	ids <- match(groups, ordered_groups)
	study_data$group.id <- paste0(study_data$study.id, '-', ids)

	# Source
	study_data$source[
		study_data$quality == 'gamma-rays whole body'
	] <- 'Co-60'
		
  # Check data
  s <- summarize_study_data(study_data[,])
  # MAS/lifespan
  s[,c('n', 'group.id', 'sex', 'MAS')]	
  # dose
  s[,c('n', 'group.id', 'sex', 'dose')]
  # fractions
  s[,c('n', 'group.id', 'sex', 'fractions')]
  # dose_rate
  s[,c('n', 'group.id', 'sex', 'dose_rate')]	
  # other treatments
  unique(study_treatments)
  # quality
  s[,c('n', 'group.id', 'sex', 'quality')]	
  # fraction_interval
  s[,c('n', 'group.id', 'sex', 'fraction_interval')]	
  # fraction_time
  s[,c('n', 'group.id', 'sex', 'fraction_time')]	
  # cluster
  s[,c('n', 'group.id', 'sex', 'cluster')]	


	# Exclude
	exclusions <- list(
		list(
			who=study_data$group.id %in% c(
				'1003-21-6'
			),
			why="see exclusion-1 in radiation.R"
		),
		list(
			who=study_data$cause_of_death %in% c(
				"Removal to another experiment", 
				"Discard"
			),
			why="see exclusion-2 in radiation.R"
		)
	)
			
	study_data <- add_exclusions(exclusions, study_data)
	
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]

	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.6.rds')

	
	###
	# Pick a study
	study <- '1003-27'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.6.rds')

	# Read title/basic description
	# List sources
	# anl-95-3.pdf JM-10
	# Check for lifespan data
	
	# Get Data
	
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		(is.na(quality) | quality == 'gamma-rays whole body') &
		dose < 150
	)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	# 
	# regroup
	groups <- with(study_data, paste(quality, dose, fractions))
	print_for_copy(sort(unique(groups)))
	ordered_groups <- c(
		"NA 0 24",
		"NA 0 1",
		"gamma-rays whole body 86.31 1",
		"gamma-rays whole body 137.14 1"
	)
	ids <- match(groups, ordered_groups)
	study_data$group.id <- paste0(study_data$study.id, '-', ids)
	# cGy -> Gy
	study_data$dose <- study_data$dose / 100
	study_data$dose_rate <- study_data$dose_rate / 100
	# control quality
	study_data$quality[study_data$dose == 0] <- 'none (controls)'
	study_data$source[
		study_data$quality == 'gamma-rays whole body'
	] <- 'Co-60'	
	
	# Check data
	s <- summarize_study_data(study_data[
		study_data$cause_of_death %in% c('Died', 'Sacrifice, moribund')
	,])
	# MAS/lifespan
	s[,c('n', 'group.id', 'sex', 'MAS')]	
	# dose
	s[,c('n', 'group.id', 'sex', 'dose')]
	# fractions
	s[,c('n', 'group.id', 'sex', 'fractions')]
	# dose_rate
	s[,c('n', 'group.id', 'sex', 'dose_rate')]	
	# other treatments
	unique(study_treatments)
	# quality
	s[,c('n', 'group.id', 'sex', 'quality')]	
	# fraction_interval
	s[,c('n', 'group.id', 'sex', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'group.id', 'sex', 'fraction_time')]	
	# cluster
	s[,c('n', 'group.id', 'sex', 'cluster')]	

	# Add exclusions
	exclusions <- list(
		list(
			who=!study_data$cause_of_death %in% c(
				'Died', 
				'Sacrifice, moribund'
			),
			why="see exclusion-3 in radiation.R"
		)
	)
	study_data <- add_exclusions(exclusions, study_data)
	
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.7.rds')
	
	# Followup
	# full clean/save (below)
	# remove references in ddref.R
	# update focal study in ddref.R
	# rerun funnel in ddref.R
	# rerun 'more beir' in ddref.R
	# check the lifespan graph in ddref.R

	###
	# Pick a study
	study <- '1007-2'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.7.rds')

	# Read title/basic description
	# List sources
	# 3577229.pdf
	# Check for lifespan data
		
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		TRUE
	)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	# 
	# missing dose rate
	study_data$dose_rate <- 0.4
	# missing fractions
	study_data$fractions <- 1
	# missing fraction_time
	study_data$fraction_time <- study_data$dose / study_data$dose_rate
	# flawed clusters
	study_data$cluster[study_data$strain == 'Mouse, C3Hf/Bd'] <- '1007-2-2'
	study_data$source[
		study_data$quality == 'gamma-rays Cs-137'
	] <- 'Cs-137'
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'strain', 'sex', 'dose', 'lifespan')]	
	# dose
	s[,c('n', 'strain', 'sex', 'dose')]
	# fractions
	s[,c('n', 'strain', 'sex', 'fractions')]
	# dose_rate
	s[,c('n', 'strain', 'sex', 'dose_rate')]	
	# other treatments
	unique(study_treatments)
	# quality
	s[,c('n', 'strain', 'sex', 'quality')]	
	# fraction_interval
	s[,c('n', 'strain', 'sex', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'strain', 'sex', 'fraction_time')]	
	# cluster
	s[,c('n', 'strain', 'sex', 'cluster')]	
	# treatment age
	s[,c('n', 'strain', 'sex', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.8.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)


	###
	# Pick a study
	study <- '1007-3'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.8.rds')

	# Read title/basic description
	# List sources
	# 3575061.pdf
	# Check for lifespan data
		
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		TRUE
	)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	# source: Ullrich 1979 jstor.org/stable/pdfplus/3575012.pdf
	study_data$strain <- 'Mouse, RFM/Un'
	study_data$unit <- 'grays'
	study_data$dose[study_data$group.id == '1007-3-7'] <- 3
	study_data$fractions <- 1  
	study_data$dose_rate <- 0.45
	study_data$fraction_time <- with(study_data, dose / dose_rate) 
	study_data$source[
		study_data$quality == 'gamma-rays Cs-137'
	] <- 'Cs-137'	  
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'sex', 'dose', 'lifespan')]		
	# dose
	s[,c('n', 'sex', 'dose')]
	# fractions
	s[,c('n', 'group.id', 'sex', 'fractions')]
	# dose_rate
	s[,c('n', 'dose', 'sex', 'lifespan')]	
	# other treatments
	unique(study_treatments)
	# quality
	s[,c('n', 'group.id', 'sex', 'quality')]	
	# fraction_interval
	s[,c('n', 'group.id', 'sex', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'group.id', 'sex', 'fraction_time')]	
	# cluster
	s[,c('n', 'group.id', 'sex', 'cluster')]	
	# treatment age
	s[,c('n', 'strain', 'sex', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
		list(
			who=study_data$group.id %in% c('1007-3-8', '1007-3-16'),
			why="see exclusion-7 in radiation.R"
		)
	)
	study_data <- add_exclusions(exclusions, study_data)
	
	# Add warnings
	warnings <- list(
		list(
			who=study_data$id == study_data$id,
			why="see warning-2 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
	
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.9.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	

	###
	# Pick a study
	study <- '11-1'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.9.rds')

	# Read title/basic description
	# List sources
	# era description
	# Check for lifespan data
		
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		!study_data$quality %in% c(
			'accel. neutrons 0.1-10 MeV',
			'neutrons>10 MeV',
			'neutrons 1-10 MeV'
		) &
		!study_data$group.id == '11-1-143'
	)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	# source: era
	study_data$quality[is.na(study_data$quality)] <- 'none (controls)'
	study_data$dose[is.na(study_data$dose)] <- 0
	study_data$fractions <- 1
	study_data$dose_rate <- 0.06
	study_data$fraction_time <- study_data$dose / study_data$dose_rate
	t <- study_data$other_treatments
	study_data$other_treatments[grepl('ovarectomy',t)] <- 'ovarectomy'
	study_data$other_treatments[grepl('estrogen',t)] <- 'estrogen'
	study_data$other_treatments[
		grepl('estrogen',t) &
		grepl('ovarectomy',t)
	] <- 'ovarectomy & estrogen'
	distinctions <- paste_columns(study_data[,c(
		'strain', 'other_treatments'
	)])
	print_for_copy(unique(distinctions))
	cluster_order <- c(
		"Rat, WAG/RIJ none",
		"Rat, WAG/RIJ estrogen",
		"Rat, WAG/RIJ ovarectomy",
		"Rat, WAG/RIJ ovarectomy & estrogen",
		"Rat, Sprague Dawley (SD/RIJ) none",
		"Rat, Sprague Dawley (SD/RIJ) estrogen",
		"Rat, Sprague Dawley (SD/RIJ) ovarectomy",
		"Rat, Sprague Dawley (SD/RIJ) ovarectomy & estrogen",
		"Rat Brown Norway (BN/BRIJ) none",
		"Rat Brown Norway (BN/BRIJ) estrogen",
		"Rat Brown Norway (BN/BRIJ) ovarectomy",
		"Rat Brown Norway (BN/BRIJ) ovarectomy & estrogen"
	)
	study_data$cluster <- paste0(study, '-', 
		match(distinctions, cluster_order)
	)
	study_data$source[
		study_data$quality == 'X-rays whole body'
	] <- 'x-ray'
	study_data$age.at.treatment[
		study_data$dose == 0
	] <- NA

	
	# Check data
	s <- summarize_study_data(study_data)
	# MAS/lifespan
	s[,c('n', 'strain', 'dose', 'lifespan')]		# fail
	# dose
	s[,c('n', 'strain', 'dose')]
	# fractions
	s[,c('n', 'strain', 'fractions')]
	# dose_rate
	s[,c('n', 'strain', 'dose_rate')]	
	# other treatments
	s[,c('n', 'strain', 'other_treatments')]	
	# quality
	s[,c('n', 'strain', 'quality')]	
	# fraction_interval
	s[,c('n', 'strain', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'strain', 'fraction_time')]	
	# cluster
	s[,c('n', 'strain', 'cluster')]	
	# treatment age
	s[,c('n', 'cluster', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	
	# Add warnings
	warnings <- list(
		list(
			who=study_data$id == study_data$id,
			why="see warning-3 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.10.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	
	###
	# Pick a study
	study <- '11-2'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.10.rds')

	# Read title/basic description
	# List sources
	# era description
	# Check for lifespan data
		
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		!study_data$quality %in% c(
			"accel. neutrons 0.1-10 MeV",
			"neutrons 1-10 MeV",
			"neutrons>10 MeV",
			"X-rays local"
		)
	)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	study_data$fractions <- 1
	corrections <- list(
		list(
			fractions=5, fraction_interval=4*7/5,
			groups=c(3)
		),list(
			fractions=10, fraction_interval=1*30.5,
			groups=c(8,9,11,12,17)
		),list(
			fractions=20, fraction_interval=7/2,
			groups=c(13,18)
		),list(
			fractions=10, fraction_interval=7/5,
			groups=c(14,19,29,30)
		),list(
			fractions=120, fraction_interval=12/120,
			groups=c(38,39,44,45)
		),list(
			fractions=5, fraction_interval=7,
			groups=c(58)
		),list(
			fractions=10, fraction_interval=7,
			groups=c(71:74,77,78)
		),list(
			fractions=5*4, fraction_interval=7/5,
			groups=c(110,114,118,121)
		),list(
			dose=2.0,
			groups=c(47)
		),list(
			strain='Rat, WAG/RIJ', 
			groups=c(67)
		),list(
			quality='X-rays whole body', 
			groups=c(78)
		),list(
			dose=0.2, 
			groups=c(100)
		),list(
			dose=0.4, 
			groups=c(110, 112, 114)
		),list(
			dose=0.4, 
			groups=c(124,125)
		),list(
			dose=0.6, 
			groups=c(126)
		),list(
			dose=1.6, 
			groups=c(127,128,129)
		),list(
			quality='gamma-rays local', 
			other_treatments='none',
			groups=c(124:127,129)
		)	
	)
	for(c in corrections){
		feilds <- names(c)[!names(c) %in% 'groups']
		
		for(f in feilds){
			s <- study_data$group.id %in% paste0(study, '-', c$groups)
			study_data[s, f] <- c[[f]]
		}
	}
	study_data$dose[is.na(study_data$dose)] <- 0
	study_data$quality[is.na(study_data$quality)] <- 'none (controls)'
	study_data$dose_rate[is.na(study_data$dose_rate)] <- 0
	study_data$unit <- 'grays'
	study_data$dose_rate[
		study_data$quality == 'gamma-rays whole body' &
		study_data$fractions == 1
	] <- 0.9
	study_data$dose_rate[
		study_data$quality == 'gamma-rays whole body' &
		study_data$fractions > 1
	] <- 0.001
	study_data$dose_rate[
		study_data$quality == 'X-rays whole body'
	] <- 0.06
	study_data$fraction_time = with(study_data, 
		dose / (dose_rate*fractions)
	)
	study_data$other_treatments[
		grepl('estrogen', study_data$other_treatments)
	] <- 'estrogen'
	study_data$cluster <- ""
	clusters <- list(
		c(1:3),
		c(7:13,15,						# 7-15 from B
		  98,102,108,110,116,118		# 98-118 from F
		),
		c(7, 16:19),
		c(23,29),
		c(24,30),
		c(31,34,38,40),
		c(31,36,42,44),
		c(32,35,37,39,41,43,45),
		c(31,46:47),
		c(49,61,71,77),
		c(49,79,81),
		c(50,62,72,78),
		c(51,53:55,57:59,75, 		   	# 51-75 from E
		  99,100,104,112,114,119,121), 	# 99-121 from F
		c(52,56,60,76)
	)
	for(i in 1:length(clusters)){
		groups <- paste0(study, '-', clusters[[i]])
		s <- study_data$group.id %in% groups
		study_data[s,'cluster'] <- paste0(
			study_data[s,'cluster'], " ",
			study, '-', i
		)
	}
	study_data$cluster <- gsub('(^ *| *$)', '', study_data$cluster)
	study_data$source[
		study_data$quality == 'gamma-rays whole body'
	] <- 'Cs-137'
	study_data$source[
		study_data$quality == 'gamma-rays local'
	] <- 'Cs-137'
	study_data$source[
		study_data$quality == 'X-rays whole body'
	] <- 'x-ray'
	study_data$age.at.treatment[
		study_data$dose == 0
	] <- NA

	# Refilter
	filter <- with(study_data,		
		!study_data$quality %in% c(
			"gamma-rays local"
		)
	)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]

		
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'strain', 'lifespan')]		# fail	
	# dose
	s[,c('n', 'strain', 'dose')]
	# unit
	s[,c('n', 'strain', 'unit')]
	# fractions
	s[,c('n', 'strain', 'fractions')]
	# fraction_interval
	s[,c('n', 'strain', 'fraction_interval')]	
	# quality
	s[,c('n', 'strain', 'quality')]	
	# dose_rate
	s[,c('n', 'quality', 'dose_rate')]	
	# fraction_time
	s[,c('n', 'strain', 'fraction_time')]	
	# other treatments
	s[,c('n', 'other_treatments')]	
	# cluster
	s[,c('n', 'strain', 'cluster')]	
	# treatment age
	s[,c('n', 'strain', 'sex', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
		list(
			who=study_data$group.id %in% paste0(study,'-',c(
				12, 15, 33, 48, 65:70, 73:74, 80
			)),
			why="see exclusion-4 in radiation.R"
		),
		list(
			who=study_data$group.id %in% paste0(study,'-',c(
				79:81
			)) | study_data$id %in% c(
				'11-2-98-100',
				'11-2-99-83'
			),
			why="see exclusion-5 in radiation.R"
		),
		list(
			who=study_data$group.id %in% paste0(study,'-',c(
				39,45,46
			)) &
				study_data$lifespan == 0,
			why="see exclusion-6 in radiation.R"
		)
	)
	study_data <- add_exclusions(exclusions, study_data)
	
	# Add warnings
	warnings <- list(
		list(
			who=study_data$id == study_data$id,
			why="see warning-4 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.11.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	

	###
	# Pick a study
	study <- '3-4'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.11.rds')

	# Read title/basic description
	# List sources
	# 3577526.pdf
	# Check for lifespan data
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,
		! quality %in% c('neutrons fission')
	)
	table0(filter)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	study_data$fractions <- 1
	study_data$dose_rate <- 0.133
	study_data$fraction_time <- with(study_data, dose / dose_rate)
	study_data$source[
		study_data$quality == 'X-rays whole body'
	] <- 'x-ray'
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n',  'lifespan')]	
	# dose
	s[,c('n',  'dose')]
	# fractions
	s[,c('n',  'fractions')]
	# dose_rate
	s[,c('n',  'dose_rate')]	
	# other treatments
	s[,c('n',  'other_treatments')]	
	# quality
	s[,c('n',  'quality')]	
	# fraction_interval
	s[,c('n',  'fraction_interval')]	
	# fraction_time
	s[,c('n',  'fraction_time')]	
	# cluster
	s[,c('n',  'cluster')]	
	# treatment age
	s[,c('n',  'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
		list(
			who=with(study_data, id == id),
			why="see exclusion-8 in radiation.R"
		)
	)
	study_data <- add_exclusions(exclusions, study_data)
	
	# Add warnings
	warnings <- list(
	)
	study_data <- add_warnings(warnings, study_data)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.12.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	
	
	###
	# Pick a study
	study <- '3-5'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.12.rds')

	# Read title/basic description
	# List sources
	# 3576356.pdf
	# Check for lifespan data in source
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		! quality %in% c('neutrons fission')
	)
	table0(filter)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	study_data$fractions <- 1
	study_data$dose_rate <- 0.133
	study_data$fraction_time <- with(study_data, dose / dose_rate)
	study_data$cluster <- with(study_data, paste0(
		study.id, '-', as.numeric(as.factor(age.at.treatment))
	))
	study_data$source[
		study_data$quality == 'X-rays whole body'
	] <- 'x-ray'
	
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'sex', 'MAS')]
	# n, sex, age.at.treatment, dose, and lifespan
	s[,c('n', 'sex', 'age.at.treatment', 'dose', 'lifespan')]	
	# fractions
	s[,c('n', 'sex', 'fractions')]
	# dose_rate
	s[,c('n', 'sex', 'dose_rate')]	
	# other treatments
	s[,c('n', 'sex', 'other_treatments')]	
	# quality
	s[,c('n', 'sex', 'quality')]	
	# fraction_interval
	s[,c('n', 'sex', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'sex', 'fraction_time')]	
	# cluster
	s[,c('n', 'age.at.treatment', 'cluster')]	
	# treatment age
	s[,c('n', 'sex', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	
	# Add warnings
	warnings <- list(
		list(
			who=study_data$group.id == '3-5-19',
			why="see warning-5 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
	table0(study_data$warning)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.13.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	
	
	###
	# Pick a study
	study <- '9-5'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.13.rds')

	# Read title/basic description
	# List sources
	# 3575970.pdf
	# Check for lifespan data in source
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		!quality %in% c('neutrons>10 MeV')
	)
	table0(filter)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	study_data$fraction_interval <- study_data$fraction_interval/1440
	study_data$fraction_time <- with(study_data, 
		dose / (fractions * dose_rate)
	)
	study_data$quality[
		study_data$quality == 'gamma-rays Co-60'
	] <- 'gamma-rays whole body'
	study_data$source[
		study_data$quality == 'gamma-rays whole body'
	] <- 'Cs-137'
	study_data$dose_rate <- 4
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'warning', 'dose', 'lifespan')]	
	# dose
	s[,c('n', 'strain', 'sex', 'dose')]
	# fractions
	s[,c('n', 'strain', 'sex', 'fractions')]
	# dose_rate
	s[,c('n', 'strain', 'sex', 'dose_rate')]	
	# other treatments
	s[,c('n', 'strain', 'sex', 'other_treatments')]	
	# quality
	s[,c('n', 'strain', 'sex', 'quality')]	
	# fraction_interval
	s[,c('n', 'strain', 'sex', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'strain', 'sex', 'fraction_time')]	
	# cluster
	s[,c('n', 'strain', 'sex', 'cluster')]	
	# treatment age
	s[,c('n', 'strain', 'sex', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	table0(study_data$exclusion)
	
	# Add warnings
	warnings <- list(
		list(
			who=study_data$group.id %in% c('9-5-2', '9-5-10'),
			why="see warning-6 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
	table0(study_data$warning)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.14.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	
	
	###
	# Pick a study
	study <- '9-6'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.14.rds')

	# Read title/basic description
	# List sources
	# 3577205.pdf
	# Check for lifespan data in source
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		! quality %in% c('neutrons>10 MeV')
	)
	table0(filter)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	study_data$fractions[study_data$group.id == '9-6-1'] <- 1
	study_data$fraction_interval <- study_data$fraction_interval/1440
	study_data$fraction_time <- with(study_data, 
		dose / (dose_rate * fractions)
	)
	study_data$quality[
		study_data$quality == 'gamma-rays Co-60'
	] <- 'gamma-rays whole body'
	study_data$source[
		study_data$quality == 'gamma-rays whole body'
	] <- 'Cs-137'	
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'dose', 'fractions', 'lifespan')]	
	# dose
	s[,c('n', 'dose')]
	# fractions
	s[,c('n', 'fractions')]
	# dose_rate
	s[,c('n', 'dose_rate')]	
	# other treatments
	s[,c('n', 'other_treatments')]	
	# quality
	s[,c('n', 'quality')]	
	# fraction_interval
	s[,c('n', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'fraction_time')]	
	# cluster
	s[,c('n', 'cluster')]	
	# treatment age
	s[,c('n', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	table0(study_data$exclusion)
	
	# Add warnings
	warnings <- list(
	)
	study_data <- add_warnings(warnings, study_data)
	table0(study_data$warning)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.15.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	
	
	###
	# Pick a study
	study <- '9-7'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.15.rds')

	# Read title/basic description
	# List sources
	# 3579307.pdf
	# Check for lifespan data in source
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		! quality %in% 'neutrons 1-10 MeV'
	)
	table0(filter)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	study_data$dose_rate <- 1
	study_data$fractions <- 1
	study_data$fraction_time <- with(study_data, dose / dose_rate)
	study_data$cluster <- with(study_data, paste0(
		study.id, '-', as.numeric(as.factor(age.at.treatment))
	))
	study_data$cluster[study_data$group.id == '9-7-1'] <- 
		'9-7-1 9-7-2'	
	study_data$source[
		study_data$quality == 'X-rays whole body'
	] <- 'x-ray'
	study_data$age.at.treatment[
		study_data$dose == 0
	] <- NA
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'age.at.treatment', 'dose', 'lifespan')]	
	# dose
	s[,c('n', 'strain', 'sex', 'dose')]
	# fractions
	s[,c('n', 'fractions')]
	# dose_rate
	s[,c('n', 'dose_rate')]	
	# other treatments
	s[,c('n', 'other_treatments')]	
	# quality
	s[,c('n', 'quality')]	
	# fraction_interval
	s[,c('n', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'fraction_time')]	
	# cluster
	s[,c('n', 'cluster')]	
	# treatment age
	s[,c('n', 'age.at.treatment')]	


	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	table0(study_data$exclusion)
	
	# Add warnings
	warnings <- list(
		list(
			who=with(study_data, id == id),
			why="see warning-7 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
	table0(study_data$warning)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.16.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	
	
	###
	# Pick a study
	study <- '3-1'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.16.rds')

	# Read title/basic description
	# List sources
	# 3577210.pdf
	# Check for lifespan data in source
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		! quality %in% 'neutrons 1-10 MeV' &
		! group.id %in% '3-1-9'
	)
	table0(filter)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	doses <- data.frame(
		group= c('3-1-2', '3-1-7', '3-1-8'),
		dose = c(   0.4,    12.8,    25.6 )
	)
	for(g in doses$group){
		study_data$dose[study_data$group.id == g] <- 
			doses$dose[doses$group == g]
	}
	study_data$fractions <- 1
	study_data$dose_rate <- 0.6
	study_data$fraction_time <- with(study_data, dose / dose_rate)
	study_data$age.at.treatment <- 6 * 7
	study_data$source[
		study_data$quality == 'X-rays whole body'
	] <- 'x-ray'	
	
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'dose', 'lifespan')]
	# strain
	s[,c('n', 'dose', 'strain')]
	# sex
	s[,c('n', 'dose', 'sex')]
	# dose
	s[,c('n', 'dose')]
	# fractions
	s[,c('n', 'fractions')]
	# dose_rate
	s[,c('n', 'dose_rate')]	
	# other treatments
	s[,c('n', 'other_treatments')]	
	# quality
	s[,c('n', 'quality')]	
	# fraction_interval
	s[,c('n', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'fraction_time')]	
	# cluster
	s[,c('n', 'cluster')]	
	# treatment age
	s[,c('n', 'age.at.treatment')]	
	
	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	table0(study_data$exclusion)

	# Add warnings
	warnings <- list(
		list(
			who=study_data$group.id %in% c('3-1-1'),
			why="see warning-8 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
	table0(study_data$warning)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.17.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)
	
	
	###
	# Pick a study
	study <- '3-2'

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.17.rds')

	# Read title/basic description
	# List sources
	# Could not find a source for Gamma, only neutron, 3578595.pdf
	# Check for lifespan data in source
	
	# Get Data
	study_data <- get_study_data(study)
	study_treatments <- get_study_treatments(study)

	# Check for existing problems
	find_in_file(study)
	
	# Filter
	# so we only have the groups needed to estimate ddref
	filter <- with(study_data,		
		! quality %in% c('neutrons fission')
	)
	table0(filter)
	study_data <- study_data[filter,]
	study_treatments <- study_treatments[filter,]
		
	# Fix problems
	# but only the ones that might affect ddref estimates
	study_data$quality[study_data$group.id %in% c('3-2-1')] <- 
		'none (controls)'
	study_data$fractions <- 1
	study_data$dose_rate <- 0.318
	study_data$fraction_time <- with(study_data, dose / dose_rate)
	study_data$source[
		study_data$quality == 'X-rays whole body'
	] <- 'x-ray'
	
	# Check data
	s <- summarize_study_data(study_data[
	,])
	# MAS/lifespan
	s[,c('n', 'dose', 'quality', 'lifespan')]	
	# dose
	s[,c('n', 'dose')]
	# fractions
	s[,c('n', 'fractions')]
	# dose_rate
	s[,c('n', 'dose_rate')]	
	# other treatments
	s[,c('n', 'other_treatments')]	
	# quality
	s[,c('n', 'quality')]	
	# fraction_interval
	s[,c('n', 'fraction_interval')]	
	# fraction_time
	s[,c('n', 'fraction_time')]	
	# cluster
	s[,c('n', 'cluster')]	
	# treatment age
	s[,c('n', 'age.at.treatment')]	

	# Add exclusions
	exclusions <- list(
	)
	study_data <- add_exclusions(exclusions, study_data)
	table0(study_data$exclusion)
	
	# Add warnings
	warnings <- list(
		list(
			who=with(study_data, id == id),
			why="see warning-9 in radiation.R"
		)
	)
	study_data <- add_warnings(warnings, study_data)
	table0(study_data$warning)
		
	# Vet
	study_data$is_vetted <- TRUE
	
	# Merge
	rows <- match(study_data$id, data$id)
	cols <- relevant_columns
	data[rows,cols] <- study_data[,cols]
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.18.rds')
	
	# Followup
	# update template (below)
	# full clean/save (below)
	# follow ddref.R instructions (on paper)


  ### 
  # Prepare for finishing steps
  #
  # Just for simplicity its nice to save as a new name for
  # the last steps.  This makes it easy to add new cleanup
  # steps above.
  
  data <- readRDS('../data/external4.18.rds')
  saveRDS(data, '../data/external4.study_fixes.rds')


	###
	# Stray Fixes
	#
	# These groups stick around despite having the wrong stats
	# because their clusters are poorly defined.  I could go through
	# a full fix for them, but a quick fix fits the bill.

	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.study_fixes.rds')

	clusters <- list(
		# Cluster : Group Ids
		list('1007-1-1', c('1007-1-1', '1007-1-2', '1007-1-3')),
		list('1007-1-2', c('1007-1-4', '1007-1-5', '1007-1-6')),
		
		list('1008-3-1', c('1008-3-1')),
		list('1008-3-2', c('1008-3-2')),
		list('1008-3-3', c('1008-3-3')),
		list('1008-3-4', c('1008-3-4')),
		list('1008-3-5', c('1008-3-5')),
		list('1008-3-6', c('1008-3-6')),
		list('1008-3-7', c('1008-3-7')),
		list('1008-3-8', c('1008-3-8')),
		list('1008-3-9', c('1008-3-9')),
		list('1008-3-10', c('1008-3-10')),
		list('1008-3-11', c('1008-3-11')),
		list('1008-3-12', c('1008-3-12')),
		
		list('2-1-1', c('2-1-1')),
		list('2-1-2', c('2-1-2')),
		list('2-1-3', c('2-1-3')),
		list('2-1-4', c('2-1-4')),
		list('2-1-5', c('2-1-5')),
		
		list('3-6-1', paste0('3-6-', c(1:5))),
		list('3-6-2', paste0('3-6-', c(6:10))),
		list('3-6-3', paste0('3-6-', c(11:16))),
		list('3-6-4', paste0('3-6-', c(17:23))),
		list('3-6-5', paste0('3-6-', c(24))),
		
		list('3-1-2', paste0('3-1-', c(9:15)))
	)
	for(c in clusters){
		cluster = c[[1]]
		groups = c[[2]]
		data$cluster[data$group.id %in% groups] <- cluster
	}
	
	
	# Save checkpoint
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external4.stray_fixes.rds')
	
	
	
	###
	# Clean
	
	# Restore from checkpoint
	setwd('~/janus/scripts')
	data <- readRDS('../data/external4.stray_fixes.rds')
	
	# constants
	strains <- data.frame(
		old = c(
			"Dog, Beagle",
			"Mouse, B6CF1",
			"leucopus",
			"Mouse, BALB/c Bd",
			"Mouse, BFM",
			"Mouse, C57BL/6Bd",
			"Mouse, C3Hf/Bd",
			"Mouse, RFM/Un",
			"Rat, WAG/RIJ",
			"Rat, Sprague Dawley (SD/RIJ)",
			"Rat Brown Norway (BN/BRIJ)",
			"Rat, Sprague-Dawley SPF",
			"Rat Wistar",
			"Mouse, BC3F1 (C57Bl/CnexC3H/Cne)F",
			"Mouse, CBA/H/Cne",
			"BALB/c/Cnb",
			"Mouse, C57BL/Cnb"
		),
		new = c(
			"beagle",
			"B6CF1",
			"leucopus",
			"BALB/c/Bd",
			"BFM",
			"C57BL/6Bd",
			"C3Hf/Bd",
			"RFM/Un",
			"WAG/RIJ",
			"SD/RIJ",
			"BN/BRIJ",
			"SPF",
			"Wistar",
			"BC3F1",
			"CBA/H/Cne",
			"BALB/c/Cnb",
			"C57BL/Cnb"
		)
	)
	
	# Adopt new naming style
	data$group_id <- data$group.id
	data$study_id <- data$study.id
	data$group_name <- data$group.name
	data$lifestage_at_treatment <- data$lifestage.at.treatment
	data$age_at_treatment <- data$age.at.treatment
	data$pathology_description <- data$pathology.description
	
	# Dump old names
	data <- data[,!grepl('\\.', names(data))]
	
	# simplify strain names
	for(strain in unique(data$strain)){
		data$strain[data$strain == strain] <- 
			as.character(strains$new[strains$old == strain])
	}	
	
	# Save
	setwd('~/janus/scripts')
	saveRDS(data, '../data/external5.rds')


# Results
#
# This was a good approach.  Data is clean and saved.  If we want to
# widen the search, there will be more work to do.
#
# Warnings and exclusions listed below should be kept for posterity.
	
# Exclusions
# The reasons for some exclusions are too cumbersome to fill in
# a dataframe, so I will list them in detail here.
#
# exclusion-1: These groups (6/S5 and 8/S6) appear to have been abandoned.  They have lifespans hundreds of days shorter than equivilant male groups, unlike other gender matched treatments which have much smaller differences. There MAS is not listed in the era summary tables.  see:https://era.bfs.de/studies_details.php?LabId=1003&StudyId=21.  Finally the cause of death is almost always listed as 'discard' or 'remove to another experiment'.
# exclusion-2: The cause of death for these animals is listed as 'discard' or 'removal to another experiment' meaning they were not lifespan studies.  Removal of these animals from groups 2 and 3 causes their MAS values to match what is seen in the era descriptions.  https://era.bfs.de/studies_details.php?LabId=1003&StudyId=21
# exclusion-3: Some animals were discarded (24), missing (25), moved to other experiments (6), or suffered accidental deaths (14).  These are not included in published summary tables e.g. http://janus.northwestern.edu/janus2/reports/complete.pdf table 14 and should not be included in my analysis because they did not live complete lifespans.
# exclusion-4: These groups recieved some form of estrogen exposure, but given this exposure, they do not have a sufficient number of different treatments to be included (need 3 or more).  We could simply put them in different clusters and they would be eliminated during the funnel, but easier to eliminate them here.
# exclusion-5: These groups all include animals with an age of death of 6993 days, clearly impossible.
# exclusion-6: These animals have impossible lifespans, 0.  11-2-39 has exactly 8 animals with a lifespan of 0 and there are 8 more in the data than in the description.  The remaining animals from groups 11-2-45 and 46 have simply been excluded.
# exclusion-7: These groups have mean lifespans that differ substantially (more than one standard deviation) from those reported in the literature (table 1 of Ullrich 1979 - jstor.org/stable/pdfplus/3575012.pdf).  This seems to be because of a number of exclusions due to lost or sacrificed animals which can no longer be detected in this version of the dataset.  Concretely, 1007-3-8 is off by 21 days (614 vs 635).  1007-3-16 is off by 122 days (294 vs 417).  Other groups in this study are affected as well, but by less than one standard deviation.
# exclusion-8: These groups are identical to those from 3-2.

# Warnings
# Some data might be excluded from analysis or might be kept in, this 
# contains descriptions of why.
#
# warning-2: Mean lifespan in the groups in this study varied from those reported in the formal literature (table 1 of Ullrich 1979 - jstor.org/stable/pdfplus/3575012.pdf).  Two groups varied substantially as noted in exclusion-7.  The others varied by 0-8 days, always less than one standard deviation.
# warning-3: I could not find an external source to verify lifespan or even the animals per treatment group for this study.  The only source that had a table (Solleveld, Leukemia Research Vol 10 No 7 pp 755-759) had animal number and lifespand different than what is reported here.
# warning-4: As with 11-1, I could not find an external source to verify lifespan or even the animals per treatment group for this study.  See validation_of_11-1_and_11-2.txt for my work on finding sources.
# warning-5: Group 3-5-19 has 430 animals with a mean lifespan of 824 days in the data but should only have 203 animals with a mean lifespan of 827 days according to 3576356.pdf table II.  The difference in mean lifespan is small and this is the only group with such a disparity, which makes me think its probably worth including regardless.
# warning-6 9-5-2 and 9-5-10 the mean lifespans in the data (738 and 739) are not the same at those in 3575970.pdf table 1 (743 and 747).  These are the only two discrepiances and are both less than 10 days.  Probably ok.
## warning-7 mean lifespans in 9-7 are consistently off by 2 (both up and down) from those in 3579307.pdf table 1.  The disparity is small, much less than the standard deviation, but still a bit worrisome.
## warning-8 3-1-1 has a lifespan two days higher in the data (889) than in 3577210.pdf table 1 (887).  This is the only problem in this dataset which makes me think that the data is correct and the original table is wrong.
# warning-9 none of this could be checked with a published reference because the only published reference, 3578595.pdf, deals with neutron exposure.


###########################################################
# 
# Study and Group problems
#
# A list of problems with individual groups or studies that
# ought to be addressed.


## 1002-1 Age of exposure in study 1002-1 is consistently labled as 400 days yet the study protocol says that exposure was given between 8 and 15 months of age.  I should resolve this
## 1002-1 has something called fractionation interval which is a bit mysterious to me.  But I think it needs to be encoded
## 1002-1 notes that some dogs were bred after exposure.  It would be useful to know which ones were.
## 1002-1 There is no dose rate in study 1002-1.
## 1002-1 What is the total dose rate?
## 1002-1-99 has 6 animals with an 'other' gender.  What are these?
## 1002-1-99 I should remove this group 1002-1-99 which were 'not assigned'
## 1003-20 I'm not sure the age at exposure is actually always 100.  I imagine that our data has more/better detail on the actual age of exposure
## 1003-20-20 has only 5 animals, it should have 200
## 1003-20-20-41 are missing or almost missing from the era.  Perhaps they are in janus?
## 1003-20-42 has only 187 animals, it should have 200
## 1003-20-47 to 48 claim they are 0.8 neutrons here, but they are listed as 0.2 neutrons in the era description.  Double check these.
## 1003-22-1 to 14 have doses that are 10x lower in the description than in the data I have RESOLVED: the database is correct and the description is wrong as can be seen when comparing to the Janus documentation table 9, http://janus.northwestern.edu/janus2/reports/complete.pdf
## 1003-22-22 has a dose of 12 in the table, but only 1.2 in the database.  Which is right? 
## 1003-23-7 to 8 are listed as gamma in the data but neutron in the descriptions
## 1003-27 is listed as species mouse, but this is inacurate these are peromyscus, strain leucopus.  They are more distant from mice than rats are.  RESOLVED I used janus data
## 1003-27-1 says its type sham exposure in the data but is listed as none in the description.  This is as opposed to 1003.27.2 which is listed as a sham exposure in both sources.
## 1003-27-3 has 200 mice in the data but claims it should have 455 in the description RESOLVED the era table is wrong.  http://janus.northwestern.edu/janus2/reports/complete.pdf Table 14 shows that only 200 mice are expected.
## 1003-24-4 is listed as 4.17 in the data and 417 in the era description.  I am inclined to trust the data because it is consistent with treatment 1003.24.3
## 1003-29-23 is listed as 4 Gy in the description, but as 0.4 Gy in the raw data.  I am inclined to believe the data because it is consistent with 1003.29.24, but I need to be sure.
## 1003-30-1 to 4 dose is listed as <NA> but it should be zero as it is in other control cases.
## 1003-30-3 to 4 and 7 to 10 and 13 to 20 are all injected with radioprotectors and should be removed from this analysis.  Also, the fact that these are listed as neutron and gamma exposures while the controls are listed as saline control and WR-2721 is decieving, this should be made consistent in the database.  The same problem occurs with the units which are sometimes grays and sometimes units of solution.  And the Application which is 'not applied' rather than 'external' 
## 1003-5 doses in the data are in rads while the database is in mGy.  I should simply convert everything in rads over to mGy.
## 1003-5-1, 7, and 8 are missing.  One is especially important because it is a control condition.
## 1003-5-2 notes that '18 are terminated' this is rather important as these 18 will surely affect survival estiamtes.
## 1003-51 to 55, 5, 6, 7, and 47  details missing from the dog studies as we already knew
## 1003-51-5 has no description.  I will need to cross check it using an alternate source.  Also doses, units, and application are clearly wrong, so I imagine I will have to import this data from the beagle dog archive.  However it might be worth removing this data.
## 1003-54-12 supposedly all have a lifespan of 162 days, rather implausiable
## 1003-54-6 supposedly both have a lifespan of 767 days, rather implausiable
## 1003-6 age at exposure is given by the mean age of exposure, but we could get the actual true first age of exposure from the database.
## 1003-6 the dose is given in rads per day.  Obviously days of treatment will need to be added.
## 1003-6-1 and 15 appear to be missing.  But likely they have been combined into 1003.6.99.  They need to be seperated because 9 were terminated and the others were controls.  They also need to be given proper values for age, type, and so on.
## 1003-6-9 claims to have 21 animal but on the website it is listed as having only 20.  I will need to check which is right.
## 1003-7 The number of dogs in each group does not match whats stated in the description.
## 1003-7 Units of Roentigen per day must be changed to gray if they can be
## 1005-47 This data from ITRI has no description.  Also dose and age information are missing, so we probably need to fill in all of this guy if we are going to use it.
## 1007-1-4 to 6, strain is listed as BFM, the appears as RFM/Bd in the description
## 1007-2-1 has one more mouse in the data than in the description
## 1007-2-11 has two more mice in the data than in the description
## 1007-2 dose rate is listed as 4 Gy/min in the era description and should be 0.4Gy/min by http://www.jstor.org/stable/pdfplus/3577229.pdf?acceptTC=true
## 1007-2-16 has two less mice in the data than in the description
## 1007-2-7 has two less mice in the data than in the description 
## 1007-3 these are listed as RFM/Un mice here and BFM mice in the database (like another before).  RESOLVED: the description is correct according to http://www.jstor.org/stable/pdfplus/3575059.pdf
## 1007-3-1 and 8 have units in rads even though they are control animals with a dose of zero and the rest have units in grays.  RESOLVED: updated the database to call these gray too
## 1007-3-16 the data has 2165 mice while the description reports that there are 3707
## 1007-3-17, 18 and 19 missing, 18 and 19 are serial sacrifices, so its probalby fine.  17 is the highest dose group so it would be nice to find what happend to them.
## 1007-3-7 is reported to have 0.3 Gray doses in the data and 3 gray does in the description.  I think the description is more sensible here because it fits the progression. RESOLVED: the description is correct the data is wrong and has been updated.  See http://www.jstor.org/stable/pdfplus/3575059.pdf Table I
## 1008-3 is insufficiently described in era to validate.  It must be checked against a third party if it is to be used at all.
## 1008-3 is missing all data on gender as noted before
## 1008-3 treatment ages imply that gestation is 60 days long, for example '8 d post coitus' in the description corresponds to -52 days in the data.  I should double check this gestation time for beagles.
## 11-1-143 is missing its neutron treatment age of exposure.  Only ovarectomy is listed.
## 11-1 has lots of estrogen and overectomy treatments that probably ought to be removed.
## 11-1 I was not able to check the age at first exposure I will need an external source for this 
## 11-1 says that ages were usually 8 weeks with 'some older groups as indicated' but only ages of 56 (8 weeks), 40 (~5.5 weeks) and 49 (7 weeks) are in the data.  The older mice referred to in the description do not appear.
## 11-1-143 is either missing from the data or missing its second treatment condition.  It should be neutron and overectomy
## 11-1-43 has 41 mice in our dataset and only 40 listed in the description
## 11-1-45 has 46 mice in our dataset and only 40 listed in the description
## 11-1-45 has 61 mice in our dataset and only 60 listed in the description
## 11-2 I will need to find an outside source to check age at first irradiation against.
## 11-2 some of these doses are in milligrams even though it appears at first glance that they would be rightly interpreted as a dose in gray instead.  RESOLVED No longer a problem
## 11-2 the description does not indicate which groups are which age, it also indicates that some groups are 17 week and 4 weeks old.  The 17 week old animals appear in the data, but not the 4 week olds.
## 11-2-110 to 115 are listed as 0.04 Gy in the data and 0.4 gray in the description.  The description strikes me as more accurate since it fits the progression.
## 11-2-12 and 11-2-13 seem like they might be reversed... RESOLVED (see above)
## 11-2-12 claims to have estrogen treatment but its not even close to the groups that were to recieve estrogen treatment which start at groups 32 and higher.  Given that this is wrong and its n value is wrong it seems quite suspicious like its from far later and suddenly inserted here.  However it is also possible that it's actually group 14 and the oestrad. treatment this group recieved is being labled as estrogen.  This is made more possible by the fact that 11-2-15 is wrongly labeled as an estrogen recieving animal, though it is in the same category that recieved oestrad in this treatment.  RESOLVED it is clear from the pattern in the data that what is listed as 14 in the description ought to be 12.  This also means that 12 should be 13 and 13 should be 14.
## 11-2-12 is listed as having 58 rats in the description, but the data has 120 examples.  RESOLVED (see above)
## 11-2-12 is listed as recieving estrogen and xrays in the data but not in the description.  RESOLVED (see above)
## 11-2-129 has 9 in our data and 19 in the description
## 11-2-129 is listed as having 19 mice in the description and only has 9 in the database.
## 11-2-13 is listed as having 60 rats in the description, but the data has 58 examples.  Notably this is the (incorrect) number listed in the description for 11-2-12.  So maybe someone skipped 120 on data entry?    RESOLVED (see above)
## 11-2-14 does not appear in the multiple treatments and should according to the description.  Perhaps it has been flipped with 11-2-12?  RESOLVED (see above)
## 11-2-15 is labeled as recieving estrogen treatment, but actually it got oestrad. treatment.  Possible error.
## 11-2-15 is listed as including 20 rats in the description.  In the data there are 40.  RESOLVED No longer a problem
## 11-2-20 does not appear in the description.  Instead 21 appears twice.  I suggest that the first of these apperances at a dose of 0.1 Gy should actually be labeled 20
## 11-2-21 appears twice in the descriptions.  I assume the first appearance is actually 20.  
## 11-2-31 - 48 are listed as included the proper radiation treatment, but many also recieved estrogren and oestrogen treatments.  I need to check if the other columns confirm these treatments.
## 11-2-33 is listed has including 30 rats in the description, but includes 60 rats in the data
## 11-2-34 to 48 there are many mistakes in the number of rats listed from 11-2-34 to 48  RESOLVED No longer a problem
## 11-2-39 has 48 mice in our dataset and only 40 in the description.  The description seems more sane.  RESOLVED there are 8 animals in this groups with an impossible lifespan of zero.  These seem to be erroneous.
## 11-2-56 is listed as including 40 rats in the description, but has 80 rats in the database.  RESOLVED No longer a problem
## 11-2-60 is listed as including 20 rats in the description, but has 40 rats in the database.  RESOLVED No longer a problem
## 11-2-61 - 70 have twice as many rats in the database as are listed in the description.  RESOLVED No longer a problem
## 11-2-67 is labeled as a Sprague Dawl strain in the data but is a WAG/RIJ according to the description which fits the general pattern.   RESOLVED changed association
## 11-2-78 is listed as recieving gamma ray exposure in our data, but it recieved X-ray exposure in the data.  RESOLVED changed to gamma ray
## 11-2-79 to 81 all have the same lifespan, 6993, rather implausiable
## 2-1 are controls for all the other 2-?? rats.  There is a strange break in 1982 where they made a new male control group for reasons that might be checked.
## 2-1 have no treatment ages because they are control animals, but I wonderf when they were assigned to the control condition?  Did any of them die very young?  I should be able to check.  For example the next study 2-10 used mice at age 3 months.  I should suspect that none of its controls would have died at less than 3 months.  Right?
## 2-10 notably includes some groups that recieved two doses, both of xrays but seperated in time.
## 2-10-19 to 21 should have a split dose.  Each should have a second treatment of 6 and 21 should have a third treatment of 6 as well.  If this is true then the first treatments are correct.
## 2-11 the ages seem all wrong.  I will need to correct them from the original literature.
## 2-11-1 and 2 says indicated that the treatment age was 'mothers 3 months'.  It is not clear if this means that the mothers were tracked or their offspring.  If its the offspring then the age indicator does not make much sense.  It might be designated as -??? to indicate a preconception event.
## 2-11-1, 2, 11, 12, and 13 have ambiguous gender assignments in the description.  They should be proofed against orignal sources.
## 2-11-15 has a dose of 33 Gy in the data, but only 3 in the description, the description seems likely to be more accurate because it fits with the other treatments
## 2-12-30 recieved doses localized to the abdomen.  They should be removed
## 2-12-32 has a gender of other, it should be male if its like the rest
## 2-12-41 and 42 are listed as recieving 1.5-3.5 Gy in the description, but only 2.5 Gy in table.  What to do?
## 2-12-43 and 44 only recieved localized radiation they ought to be removed
## 2-12-47 to 51 have units in Grays in the data but have units in mGy in the description.  I think the description is probably correct.
## 2-12-51 is labled as recieving 25 Gy in the data and 53 mGy in the description
## 2-13 should be removed because chemical treatments were used with every radiation treatment.
## 2-14-10 all have the same lifespan, 541 days, rather implausiable
## 2-14-22 all have the same lifespan, 317 days, rather implausiable
## 2-14-23 all have the same lifespan, 423 days, rather implausiable
## 2-14-39 through 58 missing all animals
## 3-1 to 2 and 4 to 5 the strain is listed as BC3F1 C57BlC in the data, but as (C57Bl/Cne x C3H/Cne)F1 (BC3F1) in the description.  RESOLVED not any longer	
## 3-1-10 has a dose 10 times higher in the data than in the description.  I am inclined to believe the description. 
## 3-1-3 to 6 have doses in the data that are 10 times higher than those listed in the descriptions.  I am inclined to believe the data from the descriptions.  RESOLVED descriptions were wrong and should be multiplied by 10 according to 3577210.pdf
## 3-2-10 seems to be missing in the description and each group below that has their group number shifted up by 1
## 3-2-22 and 23 have a 10x lower dose in the data than in the description.  The description seems more accurate as it fits the pattern.
## 3-5 the description indicates that groups 10-... were irradiated at 3 months while the data indicates that they were irradiated at -4 days.  Also the table indicates that the gestation period for these mice is 21.5 days, for example the treatment age in the data is -4 for group 1 which is treated at 17.5 post conception according to the description.  RESOLVED trust the data as seen in 3576356.pdf Tables I, II, and III
## 3-5 there is no mention of females in the description, yet several groups in the data include females	RESOLVED the data is correct (see 3576356.pdf tables I, II, and III)
## 3-5-1 to 19 are nigh impossible to align to the descriptions.  It appears that there are at 2 groups in the data per one in the description with group 19 as a missing control from the 3 month cohort that appears pretty normal below.  The n values do not correspond to what would be expected even if these groups are somehow split.  I think I will need to go to an original source to get a better sense of what is happening to this data.  RESOLVED the original source aligns well with the raw data.  The table in the era seems to only include males and is missing some values.  It should be corrected from original sources.
## 3-5-20 to 50 seem to correspond to groups 3-5-10 through 3-5-40 in the description.  They have all been shifted, however.
## 3-5-41 to 50 are in the database but not in the description
## 3-6 does not have sufficient data in the description to determine if the genders listed in the data are correct
## 3-6 mice are listed as CBAH/Cne in the data and CBA/Cne in the description
## 3-6 the n's are almost all shit.  Never line up exactly.  The number of groups are different as well.  There are some similarties, but they are quite poor.  I need a second source.
## 3-6-20 to 24 are in the database but not in the description table.
## 3-6-24 has no age of exposure in the data and does not exist in the data
## 3-6-6 and 7 claims to be acute neutron exposure in the description but is listed as xrays in the data




###########################################################
# 
# Universal problems
#
# Problems with the data that affect a good portion of the
# data in this dataset.

## ALL be sure to remove applications that are not external exposures.
## All, check that controls are labeled controls
## ALL make control dose_rate = 0, fractions=0, fraction_time=0, fraction_intraval=0	


## Add details to dog studies 1003.51, 1003.52, 1003.54, 1003.55, 1003.6, 1003.7, 1005.47 these might need to be removed completely

## Final pass
	## All need to be checked to ensure that the age for controls matches the age for the animals they were used as control for.  The point here is that control animals were not likely to have been picked from birth.  Rather they probably entered the study at the same age as the other animals.
	## Fill in NA columns
## After vetting some, build a predictive model that predicts which have already been vetted

## A really natural question to ask, perhaps to Gayle or Dave Grdina is how accurate they think these dose assessments are likely to be, especially in light of the reproted variation in group 2-12-41 where they say doses are between 1.5 and 3.5 Gy.  What a huge range.  Could they really be this unsure of the total dose?
## Why are some groups missing from the ERA?  Is there any systematic bias in the groups that are missing.  Maybe ask the ERA this.
## Tell era that they need to remove newlines
## All - I've been looking for something that will clearly define what 'mean after survival' means.  The best I have found is an old Grahn paper that says 'The mean after-survival from the start of daily exposure is the endpoint statistic'.  I take that to mean that they are measuring days lived after the initiation of treatment. - (Grahn 1962 - http://www.osti.gov/energycitations/product.biblio.jsp?osti_id=4635223)
## Tell era that 18.1, 19.2, 19.4-19.7, and 9.9 are missing the tag "No individual level data" in their csvs.
## look for rows that are too similar, like those with group.id 1003-54-6
## Run a pass for application
## All read up on the latest attempt to model DDREF, who did it, what data did they use?
## All application.1 needs to be made into application
## All determine fraction time as total dose divided by (dose rate * number of fractions).  But only if this value is NA.  Also detrmine the other values from time where possible when these values are NA. (assuming that some studies list exposure time and not dose, ect.)
## What is modal energy?  I am assuming this means the most common energy, but its not clear.  It could also mean the average or something else.  But I can't find a clear definition of it.
	## All, remove mice that are known to have died from decomposition, cannibalism, or some other unnatural death.
	## all, collect reasons for study to study variation.  For example beams might not be homoegenous, pathogen free strains act differently, the irriation proceedures might involve varying amounts of stress and exposure to pathogens, different beam characterstics, things that were not anticipated by the authors, etc.
	## all keep note of the general reasons why some data was not collected.  It may be that papers did not specify or that the papers cited could not be found online.
	## All check on ddref estimates from fruit flies
	## do we have any inforation on protons?

