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
# Which Studyies?
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




###########################################################
# 
# Universal problems
#
# Problems with the data that affect a good portion of the
# data in this dataset.

## Go through the TODOs and prioritize again
## Look at animals with 2nd treatment columns and be sure that all the treatements in combination are accurate (as opposed to looking exclusively at the first treatment as I am now)
## All I need to check the database for missing treatment age values.
## All is treatment age ever less than age at death?
## Check mean and median exposure times vs the dataset (they frequently seem wrong)
## Label those study groups which include a chemical treatment so I can easily discriminate them.
## I need to label what controls for what.
## reshape the data to include dose rate, number of fractions and any other information that you believe will be relevant here.
## I need dose rate data for all these treatments
## Lookup all the unique words in the dataset and learn their meanings



## A really natural question to ask, perhaps to Gayle or Dave Grdina is how accurate they think these dose assessments are likely to be, especially in light of the reproted variation in group 2-12-41 where they say doses are between 1.5 and 3.5 Gy.  What a huge range.  Could they really be this unsure of the total dose?
## Read all the descriptions for studies I am including.  Look up any words that I do not already know.
## Why are some groups missing from the ERA?  Is there any systematic bias in the groups that are missing.  Maybe ask the ERA this.
## I need to spot check these results against original papers to see if I can find additional errors.
## All need to be checked to ensure that the age for controls matches the age for the animals they were used as control for.  The point here is that control animals were not likely to have been picked from birth.  Rather they probably entered the study at the same age as the other animals.
## Tell era that they need to remove newlines
## All - I've been looking for something that will clearly define what 'mean after survival' means.  The best I have found is an old Grahn paper that says 'The mean after-survival from the start of daily exposure is the endpoint statistic'.  I take that to mean that they are measuring days lived after the initiation of treatment. - (Grahn 1962 - http://www.osti.gov/energycitations/product.biblio.jsp?osti_id=4635223)
## Tell era that 18.1, 19.2, 19.4-19.7, and 9.9 are missing the tag "No individual level data" in their csvs.
## Add details to dog studies 1003.51, 1003.52, 1003.54, 1003.55, 1003.6, 1003.7, 1005.47 these might need to be removed completely
## Cross reference janus materials against these
## Add material from janus not in here
## All be sure to record what was added before integrating new data so that I know to double check it.



###########################################################
# 
# Study and Group problems
#
# A list of problems with individual groups or studies that
# ought to be addressed.

## 11-1-143 is missing its neutron treatment age of exposure.  Only ovarectomy is listed.
## 1002-1 Age of exposure in study 1002-1 is consistently labled as 400 days yet the study protocol says that exposure was given between 8 and 15 months of age.  I should resolve this
## 1002-1 There is no dose rate in study 1002-1.
## 1002-1 notes that some dogs were bred after exposure.  It would be useful to know which ones were.
## 1002-1 has something called fractionation interval which is a bit mysterious to me.  But I think it needs to be encoded
## 1002-1-99 I should remove this group 1002-1-99 which were 'not assigned'
## 1003-20 I'm not sure the age at exposure is actually always 100.  I imagine that our data has more/better detail on the actual age of exposure
## 1003-20-20 has only 5 animals, it should have 200
## 1003-20-20-41 are missing or almost missing from the era.  Perhaps they are in janus?
## 1003-20-42 has only 187 animals, it should have 200
## 1003-20-47 to 48 claim they are 0.8 neutrons here, but they are listed as 0.2 neutrons in the era description.  Double check these.
## double check age at first irradiation on janus 1003-21 (and all of janus, 1003)
## 1003-22-1 to 14 have doses that are 10x lower in the description than in the data I have
## 1003-22-22 has a dose of 12 in the table, but only 1.2 in the database.  Which is right?
## 1003-23-7 to 8 are listed as gamma in the data but neutron in the descriptions
## 1003-24-4 is listed as 4.17 in the data and 417 in the era description.  I am inclined to trust the data because it is consistent with treatment 1003.24.3
## 1003-27-3 has 200 mice in the data but claims it should have 455 in the description
## 1003-27-1 says its type sham exposure in the data but is listed as none in the description.  This is as opposed to 1003.27.2 which is listed as a sham exposure in both sources.
## 1003-29-23 is listed as 4 Gy in the description, but as 0.4 Gy in the raw data.  I am inclined to believe the data because it is consistent with 1003.29.24, but I need to be sure.
## 1003-30-1 to 4 dose is listed as <NA> but it should be zero as it is in other control cases.
## 1003-30-3 to 4 and 7 to 10 and 13 to 20 are all injected with radioprotectors and should be removed from this analysis.  Also, the fact that these are listed as neutron and gamma exposures while the controls are listed as saline control and WR-2721 is decieving, this should be made consistent in the database.  The same problem occurs with the units which are sometimes grays and sometimes units of solution.  And the Application which is 'not applied' rather than 'external' 
## 1003-5-1, 7, and 8 are missing.  One is especially important because it is a control condition.
## 1003-5-2 notes that '18 are terminated' this is rather important as these 18 will surely affect survival estiamtes.
## 1003-5 doses in the data are in rads while the database is in mGy.  I should simply convert everything in rads over to mGy.
## 1003-51-5 has no description.  I will need to cross check it using an alternate source.  Also doses, units, and application are clearly wrong, so I imagine I will have to import this data from the beagle dog archive.  However it might be worth removing this data.
## 1003-6-1 and 15 appear to be missing.  But likely they have been combined into 1003.6.99.  They need to be seperated because 9 were terminated and the others were controls.  They also need to be given proper values for age, type, and so on.
## 1003-6-9 claims to have 21 animal but on the website it is listed as having only 20.  I will need to check which is right.
## 1003-6 the dose is given in rads per day.  Obviously days of treatment will need to be added.
## 1003-6 age at exposure is given by the mean age of exposure, but we could get the actual true first age of exposure from the database.
##  1003-7 The number of dogs in each group does not match whats stated in the description.
## 1003-7 Units of Roentigen per day must be changed to gray if they can be
## 1005-47 This data from ITRI has no description.  Also dose and age information are missing, so we probably need to fill in all of this guy if we are going to use it.
## 1007-2-1 has one more mouse in the data than in the description
## 1007-2-7 has two less mice in the data than in the description 
## 1007-2-11 has two more mice in the data than in the description
## 1007-2-16 has two less mice in the data than in the description
## 1007-3-17, 18 and 19 missing, 18 and 19 are serial sacrifices, so its probalby fine.  17 is the highest dose group so it would be nice to find what happend to them.
## 1007-3-16 the data has 2165 mice while the description reports that there are 3707
## 1007-3-7 is reported to have 0.3 Gray doses in the data and 3 gray does in the description.  I think the description is more sensible here because it fits the progression.
## 1007-3-1 and 8 have units in rads even though they are control animals with a dose of zero and the rest have units in grays
## 1008-3 is insufficiently described in era to validate.  It must be checked against a third party if it is to be used at all.
## 11-1 has lots of estrogen and overectomy treatments that probably ought to be removed.
## 11-1 I was not able to check the age at first exposure I will need an external source for this 
## 11-2-12 is listed as having 58 rats in the description, but the data has 120 examples
## 11-2-13 is listed as having 60 rats in the description, but the data has 58 examples.  Notably this is the (incorrect) number listed in the description for 11-2-12.  So maybe someone skipped 120 on data entry?
## 11-2-15 is listed as including 20 rats in the description.  In the data there are 40.
## 11-2-20 does not appear in the description.  Instead 21 appears twice.  I suggest that the first of these apperances at a dose of 0.1 Gy should actually be labeled 20
## 11-2-33 is listed has including 30 rats in the description, but includes 60 rats in the data
## 11-2-34 to 48 there are many mistakes in the number of rats listed from 11-2-34 to 48
## 11-2-56 is listed as including 40 rats in the description, but has 80 rats in the database.
## 11-2-60 is listed as including 20 rats in the description, but has 40 rats in the database.
## 11-2-61 - 70 have twice as many rats in the database as are listed in the description.
## 11-2-129 is listed as having 19 mice in the description and only has 9 in the database.
## 11-2 I will need to find an outside source to check age at first irradiation against.
## 2-1 are controls for all the other 2-?? rats.  There is a strange break in 1982 where they made a new male control group for reasons that might be checked.
## 2-10 notably includes some groups that recieved two doses, both of xrays but seperated in time.
## 2-13 should be removed because chemical treatments were used with every radiation treatment.
## 2-14-39 through 58 missing all animals
## 3-2-10 seems to be missing in the description and each group below that has their group number shifted up by 1
## 3-5-41 to 50 are in the database but not in the description
## 3-6-20 to 24 are in the database but not in the description table.
## 9-4-9, 17, 22, 23, and 32 are missing from study 9-4.  In the description this is only labeled for 23 and 32.
## 9-5-17 is missing from the description
## 1007-3 remove serial sacrifices from 1007.3
## 1002-1 What is the total dose rate?
## 11-2-12 and 13 seem to have their n's switched in the description.
## 11-2-12 claims to have estrogen treatment but its not even close to the groups that were to recieve estrogen treatment which start at groups 32 and higher.  Given that this is wrong and its n value is wrong it seems quite suspicious like its from far later and suddenly inserted here.  However it is also possible that it's actually group 14 and the oestrad. treatment this group recieved is being labled as estrogen.  This is made more possible by the fact that 11-2-15 is wrongly labeled as an estrogen recieving animal, though it is in the same category that recieved oestrad in this treatment.
## 11-2-15 is labeled as recieving estrogen treatment, but actually it got oestrad. treatment.  Possible error.
## 11-2-21 appears twice in the descriptions.  I assume the first appearance is actually 20.  
## 11-2-39 has 48 mice in our dataset and only 40 in the description.  The description seems more sane...    
## 11-1-43 has 41 mice in our dataset and only 40 listed in the description
## 11-1-45 has 46 mice in our dataset and only 40 listed in the description
## 11-1-45 has 61 mice in our dataset and only 60 listed in the description
## 11-2-31 - 48 are listed as included the proper radiation treatment, but many also recieved estrogren and oestrogen treatments.  I need to check if the other columns confirm these treatments.
## 11-2-78 is listed as recieving gamma ray exposure in our data, but it recieved X-ray exposure in the data.
## 11-2-110 to 115 are listed as 0.04 Gy in the data and 0.4 gray in the description.  The description strikes me as more accurate since it fits the progression.   
## 11-2-129 has 9 in our data and 19 in the description
## 11-2 some of these doses are in milligrams even though it appears at first glance that they would be rightly interpreted as a dose in gray instead.
## 2-10-19 to 21 should have a split dose.  Each should have a second treatment of 6 and 21 should have a third treatment of 6 as well.  If this is true then the first treatments are correct.
## 2-11-15 has a dose of 33 Gy in the data, but only 3 in the description, the description seems likely to be more accurate because it fits with the other treatments
## 2-12-30 recieved doses localized to the abdomen.  They should be removed
## 2-12-43 and 44 only recieved localized radiation they ought to be removed
## 2-12-41 and 42 are listed as recieving 1.5-3.5 Gy in the description, but only 2.5 Gy in table.  What to do?
## 2-12-51 is labled as recieving 25 Gy in the data and 53 mGy in the description
## 2-12-47 to 51 have units in Grays in the data but have units in mGy in the description.  I think the description is probably correct.
## 3-1-3 to 6 have doses in the data that are 10 times higher than those listed in the descriptions.  I am inclined to believe the data from the descriptions.     
# 3-1-10 has a dose 10 times higher in the data than in the description.  I am inclined to believe the description.
## 3-2-10 appears in the data appears to be missing from the table.  Everything below it appears to be shifted up one.
## 3-2-22 and 23 have a 10x lower dose in the data than in the description.  The description seems more accurate as it fits the pattern.
## 3-5-1 to 19 are nigh impossible to align to the descriptions.  It appears that there are at 2 groups in the data per one in the description with group 19 as a missing control from the 3 month cohort that appears pretty normal below.  The n values do not correspond to what would be expected even if these groups are somehow split.  I think I will need to go to an original source to get a better sense of what is happening to this data.      
## 3-5-20 to 50 seem to correspond to groups 3-5-10 through 3-5-40 in the description.  They have all been shifted, however.
## 3-6 the n's are almost all shit.  Never line up exactly.  The number of groups are different as well.  There are some similarties, but they are quite poor.  I need a second source.
## 3-6-6 and 7 claims to be acute neutron exposure in the description but is listed as xrays in the data
## 9-4-9 is missing from the data, though that is not noted in the description
## 9-4-17 is missing from the data, though that is not noted in the description
## 9-4-22 is missing from the data, though that is not noted in the description
## 9-4-40 is missing from the data, though that is not noted in the description
## 9-5-1 to 13 claim are listed as X-rays in the data and Gamma rays in the study description
## 9-5-14 is missing its group id in the description table (small detail)
## 9-6-2 to 16 are listed as X-ray exposures in the data but as Gamma ray exposures in the descriptions.
## 9-6-19 is listed as 0.18 Gy in the data and 0.54 Gy in the table. I'd be inclined to trust the table.
## 9-6-22 is listed as having 96 mice in the description, but has 196 mice in the data
## 9-6-25 to 27 seem to be exposures just to the thorax, these should probably be removed.

## 1003-54-12 supposedly all have a lifespan of 162 days, rather implausiable
## 1003-54-6 supposedly both have a lifespan of 767 days, rather implausiable
## look for rows that are too similar, like those with group.id 1003-54-6
## 11-2-79 to 81 all have the same lifespan, 6993, rather implausiable
## 2-14-10 all have the same lifespan, 541 days, rather implausiable
## 2-14-22 all have the same lifespan, 317 days, rather implausiable
## 2-14-23 all have the same lifespan, 423 days, rather implausiable


## 1002-1-99 has 6 animals with an 'other' gender.  What are these?
## 1003-27 is listed as species mouse, but this is inacurate these are peromyscus, strain leucopus.  They are more distant from mice than rats are.
## 1003-51 to 55, 5, 6, 7, and 47  details missing from the dog studies as we already knew
## 1007-1-4 to 6, strain is listed as BFM, the appears as RFM/Bd in the description
## All once these studies have been checked and cleaned I should give them an indicator in the data frame noting this, that way it will be easy to return to cleaning and to select those studies which have been cleared.
## 1007-3 these are listed as BFM mice here and RFM mice in the database (like another before)
## 1008-3 is missing all data on gender as noted before
## 11-2-67 is labeled as a Sprague Dawl strain in the data but is a WAG/RIJ according to the description which fits the general pattern.
## 2-11-1, 2, 11, 12, and 13 have ambiguous gender assignments in the description.  They should be proofed against orignal sources.
## 2-12-32 has a gender of other, it should be male if its like the rest
## 3-5 there is not mention of females in the description, yet several groups in the data include females
## 3-1 to 2 and 4 to 5 the strain is listed as BC3F1 C57BlC in the data, but as (C57Bl/Cne x C3H/Cne)F1 (BC3F1) in the description.  
## 3-6 does not have sufficient data in the description to determine if the genders listed in the data are correct
## 3-6 mice are listed as CBAH/Cne in the data and CBA/Cne in the description
## 9-4-1 to 23 are listed as C57BL/Cnb in the data and BALB/c/Cnb in the description.  Notably these have different LD50/30s (whatever that means)?
## 9-5 are listed as strain C57/BL6Bd in the data and as BALB/c/Cnb in the description

## 1007-3 lists ages as 7 weeks +/- 0.5 weeks.  This seems like a reasonable standard error to assume
## 1008-3 treatment ages imply that gestation is 60 days long, for example '8 d post coitus' in the description corresponds to -52 days in the data.  I should double check this gestation time for beagles.

## 11-1 says that ages were usually 8 weeks with 'some older groups as indicated' but only ages of 56 (8 weeks), 40 (~5.5 weeks) and 49 (7 weeks) are in the data.  The older mice referred to in the description do not appear.
## 11-2 the description does not indicate which groups are which age, it also indicates that some groups are 17 week and 4 weeks old.  The 17 week old animals appear in the data, but not the 4 week olds.
## 2-1 have no treatment ages because they are control animals, but I wonderf when they were assigned to the control condition?  Did any of them die very young?  I should be able to check.  For example the next study 2-10 used mice at age 3 months.  I should suspect that none of its controls would have died at less than 3 months.  Right?
## 2-11-1 and 2 says indicated that the treatment age was 'mothers 3 months'.  It is not clear if this means that the mothers were tracked or their offspring.  If its the offspring then the age indicator does not make much sense.  It might be designated as -??? to indicate a preconception event.
## 2-11 the ages seem all wrong.  I will need to correct them from the original literature.
## All I need a column for final treatment age (also something to denote if they were allowed to die in this time period)
## 3-5 the description indicates that groups 10-... were irradiated at 3 months while the data indicates that they were irradiated at -4 days.  Also the table indicates that the gestation period for these mice is 21.5 days, for example the treatment age in the data is -4 for group 1 which is treated at 17.5 post conception according to the description.
## 3-6-24 has no age of exposure in the data and does not exist in the data
