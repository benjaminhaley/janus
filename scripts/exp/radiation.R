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
#  1002-1-1   56  400 none c     0 unit c      not ap
# 1002-1-10   23  400 X-rays   300 exposu      Extern
# 1002-1-11   27  400 X-rays   300 exposu      Extern
# 1002-1-12   23  400 X-rays   300 exposu      Extern
# 1002-1-13   21  400 X-rays   300 exposu      Extern
# 1002-1-14   22  400 X-rays   300 exposu      Extern
# 1002-1-15   11  400 X-rays   300 exposu      Extern
#  1002-1-2   22  400 X-rays   100 exposu      Extern
#  1002-1-3   25  400 X-rays   100 exposu      Extern
#  1002-1-4   20  400 X-rays   100 exposu      Extern
#  1002-1-5   21  400 X-rays   100 exposu      Extern
#  1002-1-6   21  400 X-rays   100 exposu      Extern
#  1002-1-7   20  400 X-rays   100 exposu      Extern
#  1002-1-8   19  400 X-rays   100 exposu      Extern
#  1002-1-9   21  400 X-rays   300 exposu      Extern
# 1002-1-99    6 <NA>   <NA>  <NA>   <NA>        <NA>

#   Group.ID    n  age   type  dose   unit application
# 1003-20-10  200  100 shamex     0  grays      Extern
# 1003-20-11  200  100 gamma-  8.85  grays      Extern
# 1003-20-12  200  100 gamma-  8.85  grays      Extern
# 1003-20-13  200  100 gamma-  8.85  grays      Extern
# 1003-20-14  200  100 gamma-  8.85  grays      Extern
# 1003-20-15  200  100 gamma-  8.85  grays      Extern
# 1003-20-16  200  100 gamma-  8.85  grays      Extern
# 1003-20-17  200  100 gamma-  8.85  grays      Extern
# 1003-20-18  200  100 gamma-  8.85  grays      Extern
# 1003-20-19  200  100 gamma-  11.1  grays      Extern
#  1003-20-2  200  100 shamex     0  grays      Extern
# 1003-20-20    5  100 gamma-  11.1  grays      Extern
#  1003-20-1  200  100 shamex     0  grays      Extern
#  1003-20-3  200  100 shamex     0  grays      Extern
#  1003-20-4  200  100 shamex     0  grays      Extern
# 1003-20-42  187  100 neutro   2.4  grays      Extern
# 1003-20-43  200  100 neutro   0.8  grays      Extern
# 1003-20-44  200  100 neutro   0.8  grays      Extern
# 1003-20-45  400  100 neutro   0.2  grays      Extern
# 1003-20-46  400  100 neutro   0.2  grays      Extern
# 1003-20-47  200  100 neutro   0.8  grays      Extern
# 1003-20-48  200  100 neutro   0.8  grays      Extern
# 1003-20-49  200  100 neutro   2.4  grays      Extern
#  1003-20-5  200  100 shamex     0  grays      Extern
# 1003-20-50  200  100 neutro   2.4  grays      Extern
# 1003-20-51  200  194 neutro   0.8  grays      Extern
# 1003-20-52  100  194 neutro   0.8  grays      Extern
# 1003-20-53  200  194 neutro   2.4  grays      Extern
# 1003-20-54  100  194 neutro   2.4  grays      Extern
# 1003-20-55  200  287 neutro   0.8  grays      Extern
# 1003-20-56   95  287 neutro   0.8  grays      Extern
# 1003-20-57  200  287 neutro   2.4  grays      Extern
# 1003-20-58  100  287 neutro   2.4  grays      Extern
#  1003-20-6  200  100 shamex     0  grays      Extern
#  1003-20-7  200  100 shamex     0  grays      Extern
#  1003-20-8  200  100 shamex     0  grays      Extern
#  1003-20-9  200  100 shamex     0  grays      Extern

 #   Group.ID    n  age   type  dose   unit application
  # 1003-21-1  200  100 shamex     0  grays      Extern
  # 1003-21-2  195  100 shamex     0  grays      Extern
  # 1003-21-3  200  100 gamma-   0.9  grays      Extern
  # 1003-21-4  200  100 gamma-   0.9  grays      Extern
  # 1003-21-5  160  100 gamma-  1.43  grays      Extern
  # 1003-21-6   80  100 gamma-  1.43  grays      Extern
  # 1003-21-7  160  100 gamma-  2.06  grays      Extern
  # 1003-21-8   80  100 gamma-  2.06  grays      Extern
  # 1003-21-9  120  100 gamma-  4.17  grays      Extern
 # 1003-21-10   60  100 gamma-  4.17  grays      Extern
 # 1003-21-11  120  100 gamma-  5.69  grays      Extern
 # 1003-21-12  120  100 gamma-  5.69  grays      Extern
 # 1003-21-13  250  100 neutro   0.2  grays      Extern
 # 1003-21-14  250  100 neutro   0.2  grays      Extern
 # 1003-21-15  200  100 neutro   0.4  grays      Extern
 # 1003-21-16   80  100 neutro   0.4  grays      Extern
 # 1003-21-17  200  100 neutro   0.6  grays      Extern
 # 1003-21-18   80  100 neutro   0.6  grays      Extern
 # 1003-21-19  120  100 neutro   1.2  grays      Extern
 # 1003-21-20   60  100 neutro   1.2  grays      Extern
 # 1003-21-21  120  100 neutro   1.6  grays      Extern
 # 1003-21-22  120  100 neutro   1.6  grays      Extern
 # 1003-21-23   50  100 neutro   2.4  grays      Extern
 # 1003-21-24   50  100 neutro   2.4  grays      Extern

 #   Group.ID    n  age   type  dose   unit application
  # 1003-22-1  280  100 shamex     0  grays      Extern
  # 1003-22-2  200  100 shamex     0  grays      Extern
  # 1003-22-3  675  100 gamma-  2.06  grays      Extern
  # 1003-22-4  120  100 gamma-  2.06  grays      Extern
  # 1003-22-5  455  100 gamma-  4.17  grays      Extern
  # 1003-22-6  400  100 gamma-  4.17  grays      Extern
  # 1003-22-7  275  100 gamma-  9.59  grays      Extern
  # 1003-22-8   80  100 gamma-  9.59  grays      Extern
  # 1003-22-9  225  100 gamma- 19.19  grays      Extern
 # 1003-22-10   60  100 gamma- 19.19  grays      Extern
 # 1003-22-11  190  100 gamma-  38.2  grays      Extern
 # 1003-22-12   30  100 gamma-  38.2  grays      Extern
 # 1003-22-13  140  100 gamma- 51.11  grays      Extern
 # 1003-22-14   40  100 gamma- 51.11  grays      Extern
 # 1003-22-15  675  100 neutro   0.2  grays      Extern
 # 1003-22-16  600  100 neutro   0.2  grays      Extern
 # 1003-22-17  475  100 neutro   0.4  grays      Extern
 # 1003-22-18   80  100 neutro   0.4  grays      Extern
 # 1003-22-19  275  100 neutro   0.6  grays      Extern
 # 1003-22-20   40  100 neutro   0.6  grays      Extern
 # 1003-22-21  225  100 neutro   1.2  grays      Extern
 # 1003-22-22   30  100 neutro   1.2  grays      Extern
 # 1003-22-23  190  100 neutro  1.68  grays      Extern
 # 1003-22-24  150  100 neutro  1.68  grays      Extern
 # 1003-22-25  140  100 neutro   3.2  grays      Extern
 # 1003-22-26   20  100 neutro   3.2  grays      Extern
 # 1003-22-27  400  100 shamex     0  grays      Extern
 # 1003-22-28  450  100 gamma-  8.07  grays      Extern
 # 1003-22-29  500  100 gamma-  26.9  grays      Extern
 # 1003-22-30  400  100 neutro   0.8  grays      Extern
 # 1003-22-31  450  100 neutro   2.4  grays      Extern


 #   Group.ID    n  age   type  dose   unit application
  # 1003-23-1  200  100 shamex     0  grays      Extern
  # 1003-23-2  200  100 gamma-  2.06  grays      Extern
  # 1003-23-3  100  100 gamma-  4.17  grays      Extern
  # 1003-23-4   80  100 gamma-  9.59  grays      Extern
  # 1003-23-5   40  100 gamma- 19.18  grays      Extern
  # 1003-23-6  175  100 shamex     0  grays      Extern
  # 1003-23-7  175  100 gamma-  5.29  grays      Extern
  # 1003-23-8  100  100 gamma-  10.7  grays      Extern
  # 1003-23-9   75  100 gamma-  24.6  grays      Extern
  
  
 #   Group.ID    n  age   type  dose   unit application  
  # 1003-24-1  330  100 shamex     0  grays      Extern
  # 1003-24-2  180  100 shamex     0  grays      Extern
  # 1003-24-3  135  100 gamma-  4.17  grays      Extern
  # 1003-24-4   30  100 gamma-  4.17  grays      Extern
  # 1003-24-5  180  100 gamma- 19.18  grays      Extern
  # 1003-24-6  180  100 gamma- 19.18  grays      Extern
  # 1003-24-7  150  100 neutro   0.4  grays      Extern
  # 1003-24-8   30  100 neutro   0.4  grays      Extern
  # 1003-24-9  200  100 neutro   1.6  grays      Extern
 # 1003-24-10  200  100 neutro   1.6  grays      Extern
 # 1003-24-11  150  515 gamma-  2.06  grays      Extern
 # 1003-24-12   50  515 gamma-  2.06  grays      Extern
 # 1003-24-13  180  515 gamma-  5.69  grays      Extern
 # 1003-24-14  180  515 gamma-  5.69  grays      Extern
 # 1003-24-15  150  515 neutro   0.4  grays      Extern
 # 1003-24-16   50  515 neutro   0.4  grays      Extern
 # 1003-24-17  180  515 neutro   1.6  grays      Extern
 # 1003-24-18  180  515 neutro   1.6  grays      Extern 
 
 #   Group.ID    n  age   type  dose   unit application  
  # 1003-25-1  140  100 shamex     0 millig      Extern
  # 1003-25-2   50  100 shamex     0 millig      Extern
  # 1003-25-3  260  100 gamma-  69.5 millig      Extern
  # 1003-25-4  180  100 gamma-  69.5 millig      Extern
  # 1003-25-5  200  100 gamma-   174 millig      Extern
  # 1003-25-6   20  100 gamma-   174 millig      Extern
  # 1003-25-7  170  100 gamma-   319 millig      Extern
  # 1003-25-8   15  100 gamma-   319 millig      Extern
  # 1003-25-9  260  100 neutro   6.7 millig      Extern
 # 1003-25-10  180  100 neutro   6.7 millig      Extern
 # 1003-25-11  200  100 neutro  16.7 millig      Extern
 # 1003-25-12   20  100 neutro  16.7 millig      Extern
 # 1003-25-13  170  100 neutro  26.7 millig      Extern
 # 1003-25-14   15  100 neutro  26.7 millig      Extern
 
 
 #   Group.ID    n  age   type  dose   unit application  
  # 1003-26-1  200  100 shamex     0 milli       Extern
  # 1003-26-2  200  100 shamex     0 milli       Extern
  # 1003-26-3  200  100 shamex     0 milli       Extern
  # 1003-26-4  300  100 neutro  0.05  grays      Extern
  # 1003-26-5  200  100 neutro   0.1  grays      Extern
  # 1003-26-6  200  100 neutro   0.1  grays      Extern
  # 1003-26-7  200  100 neutro   0.1  grays      Extern
  # 1003-26-8  750  100 shamex     0 milli       Extern
  # 1003-26-9  500  100 gamma- 0.225  grays      Extern
 # 1003-26-10  350  100 gamma-  0.45  grays      Extern
 # 1003-26-11  200  100 gamma-   0.9  grays      Extern
 # 1003-26-12  750  100 neutro  0.01  grays      Extern
 # 1003-26-13  450  100 neutro 0.025  grays      Extern
 # 1003-26-14  350  100 neutro  0.05  grays      Extern
 # 1003-26-15  250  100 neutro   0.1  grays      Extern
 # 1003-26-16  200  100 neutro   0.2  grays      Extern
 # 1003-26-17  150  100 neutro   0.4  grays      Extern
 
 #   Group.ID    n  age   type  dose   unit application   
  # 1003-27-1  245  100 shamex     0  grays      Extern
  # 1003-27-2  210  100 shamex     0  grays      Extern
  # 1003-27-3  200  100 gamma-   0.9  grays      Extern
  # 1003-27-4  200  100 gamma-  1.43  grays      Extern
  # 1003-27-5  200  100 gamma-  2.06  grays      Extern
  # 1003-27-6  170  100 gamma-  4.17  grays      Extern
  # 1003-27-7  200  100 neutro   0.2  grays      Extern
  # 1003-27-8  200  100 neutro   0.4  grays      Extern
  # 1003-27-9  150  100 neutro   0.8  grays      Extern
 # 1003-27-10  150  100 neutro   1.6  grays      Extern
 # 1003-27-11  250  100 neutro   0.4  grays      Extern
 # 1003-27-12  215  100 neutro   1.6  grays      Extern


 #   Group.ID    n  age   type  dose   unit application    
  # 1003-28-1  120  100 shamex     0  grays      Extern
  # 1003-28-2  120  100 neutro   2.4  grays      Extern
  # 1003-28-3  120  100 neutro   2.4  grays      Extern
  # 1003-28-4  120  100 neutro   2.4  grays      Extern
  # 1003-28-5  120  100 neutro   2.4  grays      Extern
 
 #   Group.ID    n  age   type  dose   unit application    
  # 1003-29-1  810  100 shamex     0  grays      Extern
  # 1003-29-2  600  100 shamex     0  grays      Extern
  # 1003-29-3  600  100 gamma-     1  grays      Extern
  # 1003-29-4  600  100 gamma-     1  grays      Extern
  # 1003-29-5  220  100 gamma-     2  grays      Extern
  # 1003-29-6  180  100 gamma-     2  grays      Extern
  # 1003-29-7  295  100 gamma-     3  grays      Extern
  # 1003-29-8   80  100 gamma-     3  grays      Extern
  # 1003-29-9  290  100 gamma-   4.5  grays      Extern
 # 1003-29-10   80  100 gamma-   4.5  grays      Extern
 # 1003-29-11  290  100 gamma-     6  grays      Extern
 # 1003-29-12   80  100 gamma-     6  grays      Extern
 # 1003-29-13  600  100 neutro  0.02  grays      Extern
 # 1003-29-14  600  100 neutro  0.02  grays      Extern
 # 1003-29-15  455  100 neutro 0.075  grays      Extern
 # 1003-29-16  250  100 neutro 0.075  grays      Extern
 # 1003-29-17  250  100 neutro 0.135  grays      Extern
 # 1003-29-18  250  100 neutro 0.135  grays      Extern
 # 1003-29-19  450  100 neutro  0.21  grays      Extern
 # 1003-29-20  250  100 neutro  0.21  grays      Extern
 # 1003-29-21  150  100 neutro   0.3  grays      Extern
 # 1003-29-22  150  100 neutro   0.3  grays      Extern
 # 1003-29-23  285  100 neutro   0.4  grays      Extern
 # 1003-29-24   80  100 neutro   0.4  grays      Extern
 

 #   Group.ID    n  age   type  dose   unit application    
  # 1003-30-1  200  100 saline  <NA> unit c      not ap
  # 1003-30-2  200  100 saline  <NA> unit c      not ap
  # 1003-30-3  200  100 WR-272  <NA> unit c      not ap
  # 1003-30-4  200  100 WR-272  <NA> unit c      not ap
  # 1003-30-5  200  100 gamma-  2.06  grays      Extern
  # 1003-30-6  200  100 gamma-  2.06  grays      Extern
  # 1003-30-7  400  100 gamma-  2.06  grays      Extern
  # 1003-30-8  400  100 gamma-  2.06  grays      Extern
  # 1003-30-9  400  100 gamma-  4.17  grays      Extern
 # 1003-30-10  400  100 gamma-  4.17  grays      Extern
 # 1003-30-11  200  100 neutro   0.1  grays      Extern
 # 1003-30-12  200  100 neutro   0.1  grays      Extern
 # 1003-30-13  400  100 neutro   0.1  grays      Extern
 # 1003-30-14  400  100 neutro   0.1  grays      Extern
 # 1003-30-15  400  100 neutro   0.1  grays      Extern
 # 1003-30-16  400  100 neutro   0.1  grays      Extern
 # 1003-30-17  400  100 neutro   0.4  grays      Extern
 # 1003-30-18  400  100 neutro   0.4  grays      Extern
 # 1003-30-19  400  100 neutro   0.4  grays      Extern
 # 1003-30-20  400  100 neutro   0.4  grays      Extern


 #   Group.ID    n  age   type  dose   unit application    
   # 1003-5-2   92  400 gamma-   0.3 rads p      Extern
   # 1003-5-3   46  400 gamma-   0.8 rads p      Extern
   # 1003-5-4   47  400 gamma-   1.9 rads p      Extern
   # 1003-5-5   24  400 gamma-   3.8 rads p      Extern
   # 1003-5-6   16  400 gamma-   7.5 rads p      Extern
   # 1003-5-7   13  400 gamma-  12.8 rads p      Extern
   # 1003-5-8    8  400 gamma-  26.3 rads p      Extern


 #   Group.ID    n  age   type  dose   unit application    
  # 1003-51-1    7 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-2    1 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-3    4 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-4    5 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-5   12 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-6   11 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-7   11 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-8    2 <NA> Undefi  <NA> unit c      not ap
  # 1003-51-9    9 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-10    2 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-11   15 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-12   24 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-13   23 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-14   64 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-15   22 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-16    1 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-17   30 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-18   41 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-19    4 <NA> Undefi  <NA> unit c      not ap
 # 1003-51-20   10 <NA> Undefi  <NA> unit c      not ap
 

 #   Group.ID    n  age   type  dose   unit application    
  # 1003-52-1   25 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-2    2 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-3    2 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-4    2 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-5    6 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-6    6 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-7    6 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-8    6 <NA> Undefi  <NA> unit c      not ap
  # 1003-52-9    6 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-10    7 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-11    7 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-12    5 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-13    6 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-14    1 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-15    3 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-16    3 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-17    6 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-18    2 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-19    6 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-20    8 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-21    7 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-22    5 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-23    8 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-24    8 <NA> Undefi  <NA> unit c      not ap
 # 1003-52-25    2 <NA> Undefi  <NA> unit c      not ap
 

 #   Group.ID    n  age   type  dose   unit application    
  # 1003-54-1   16 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-2    4 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-3   13 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-4   15 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-5   11 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-6    2 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-7    2 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-8   15 <NA> Undefi  <NA> unit c      not ap
  # 1003-54-9    1 <NA> Undefi  <NA> unit c      not ap
 # 1003-54-10   42 <NA> Undefi  <NA> unit c      not ap
 # 1003-54-11    4 <NA> Undefi  <NA> unit c      not ap
 # 1003-54-12    5 <NA> Undefi  <NA> unit c      not ap
 # 1003-54-13    1 <NA> Undefi  <NA> unit c      not ap
 
 #   Group.ID    n  age   type  dose   unit application     
  # 1003-55-1  136 <NA> Undefi  <NA> unit c      not ap
  # 1003-55-2    6 <NA> Undefi  <NA> unit c      not ap
  # 1003-55-3    2 <NA> Undefi  <NA> unit c      not ap
  # 1003-55-4    4 <NA> Undefi  <NA> unit c      not ap
  # 1003-55-5    1 <NA> Undefi  <NA> unit c      not ap
 

 #   Group.ID    n  age   type  dose   unit application       
   # 1003-6-2   20  463 gamma-   3.8 rads p      Extern
   # 1003-6-3   24  463 gamma-   3.8 rads p      Extern
   # 1003-6-4   20  463 gamma-   3.8 rads p      Extern
   # 1003-6-5   20  463 gamma-   7.5 rads p      Extern
   # 1003-6-6   24  463 gamma-   7.5 rads p      Extern
   # 1003-6-7   19  463 gamma-   7.5 rads p      Extern
   # 1003-6-8   20  463 gamma-   7.5 rads p      Extern
   # 1003-6-9   21  463 gamma-  12.8 rads p      Extern
  # 1003-6-10   21  463 gamma-  12.8 rads p      Extern
  # 1003-6-11   19  463 gamma-  12.8 rads p      Extern
  # 1003-6-12   10  463 gamma-  12.8 rads p      Extern
  # 1003-6-13   20  463 gamma-  26.3 rads p      Extern
  # 1003-6-14   25  463 gamma-  26.3 rads p      Extern
  # 1003-6-99  129 <NA>   <NA>  <NA>   <NA>        <NA>


 #   Group.ID    n  age   type  dose   unit application         
   # 1003-7-1   10   60 gamma-   2.5 Roentg      Extern
   # 1003-7-2    8   61 gamma-     5 Roentg      Extern
   # 1003-7-3    9   53 gamma-    10 Roentg      Extern


 #   Group.ID    n  age   type  dose   unit application            
  # 1005-47-1   79 <NA> Undefi  <NA> unit c      not ap
  # 1005-47-2   34 <NA> Undefi  <NA> unit c      not ap
  # 1005-47-3   20 <NA> Undefi  <NA> unit c      not ap
  # 1005-47-4   18 <NA> Undefi  <NA> unit c      not ap
  # 1005-47-5   22 <NA> Undefi  <NA> unit c      not ap
  # 1005-47-6   15 <NA> Undefi  <NA> unit c      not ap
  # 1005-47-7   13 <NA> Undefi  <NA> unit c      not ap
  # 1005-47-8    8 <NA>   <NA>  <NA>   <NA>        <NA>

  
 #   Group.ID    n  age   type  dose   unit application            
   # 1007-1-1  832   70 none c     0  grays      Extern
   # 1007-1-2  834   70 gamma-   0.5  grays      Extern
   # 1007-1-3  809   70 gamma-     2  grays      Extern
   # 1007-1-4  745   70 none c     0  grays      Extern
   # 1007-1-5  748   70 gamma-   0.5  grays      Extern
   # 1007-1-6  759   70 gamma-     2  grays      Extern
   
   
 #   Group.ID    n  age   type  dose   unit application              
   # 1007-2-1  492   70 none c     0  grays      Extern
   # 1007-2-2  502   70 none c     0  grays      Extern
   # 1007-2-3  253   70 gamma-   0.5  grays      Extern
   # 1007-2-4  254   70 gamma-   0.5  grays      Extern
   # 1007-2-5  251   70 gamma-     1  grays      Extern
   # 1007-2-6  260   70 gamma-     1  grays      Extern
   # 1007-2-7  255   70 gamma-     2  grays      Extern
   # 1007-2-8  259   70 gamma-     2  grays      Extern
   # 1007-2-9  501   70 gamma-     0  grays      Extern
  # 1007-2-10  502   70 gamma-     0  grays      Extern
  # 1007-2-11  251   70 gamma-   0.5  grays      Extern
  # 1007-2-12  244   70 gamma-   0.5  grays      Extern
  # 1007-2-13  250   70 gamma-     1  grays      Extern
  # 1007-2-14  249   70 gamma-     1  grays      Extern
  # 1007-2-15  258   70 gamma-     2  grays      Extern
  # 1007-2-16  250   70 gamma-     2  grays      Extern
 

 #   Group.ID    n  age   type  dose   unit application              
   # 1007-3-1  430   70 none c     0   rads      Extern
   # 1007-3-2  256   70 gamma-   0.1  grays      Extern
   # 1007-3-3   94   70 gamma-  0.25  grays      Extern
   # 1007-3-4  247   70 gamma-   0.5  grays      Extern
   # 1007-3-5  230   70 gamma-     1  grays      Extern
   # 1007-3-6  199   70 gamma-   1.5  grays      Extern
   # 1007-3-7  571   70 gamma-   0.3  grays      Extern
   # 1007-3-8 3713   70 none c     0   rads      Extern
   # 1007-3-9 2696   70 gamma-   0.1  grays      Extern
  # 1007-3-10  930   70 gamma-  0.25  grays      Extern
  # 1007-3-11 1069   70 gamma-   0.5  grays      Extern
  # 1007-3-12  237   70 gamma-  0.75  grays      Extern
  # 1007-3-13 1045   70 gamma-     1  grays      Extern
  # 1007-3-14 1005   70 gamma-   1.5  grays      Extern
  # 1007-3-15  323   70 gamma-     2  grays      Extern
  # 1007-3-16 2165   70 gamma-     3  grays      Extern
  

 #   Group.ID    n  age   type  dose   unit application              
   # 1008-3-1   46  -52 none c     0 roentg      Extern
   # 1008-3-2   14  -52 none c     0 roentg      Extern
   # 1008-3-3   46  -32 none c     0 roentg      Extern
   # 1008-3-4   14  -32 none c     0 roentg      Extern
   # 1008-3-5   46   -5 none c     0 roentg      Extern
   # 1008-3-6   14   -5 none c     0 roentg      Extern
   # 1008-3-7   46    2 none c     0 roentg      Extern
   # 1008-3-8   14    2 none c     0 roentg      Extern
   # 1008-3-9   46   70 none c     0 roentg      Extern
  # 1008-3-10   14   70 none c     0 roentg      Extern
  # 1008-3-11   46  365 none c     0 roentg      Extern
  # 1008-3-12   14  365 none c     0 roentg      Extern
  

 #   Group.ID    n  age   type  dose   unit application              
     # 11-1-1   80   49 none c     0 unit c      not ap
     # 11-1-2   40   49 estrog     4 millig      Implan
     # 11-1-3   40   49 ovarec  <NA> unit c      not ap
     # 11-1-4   80   49 estrog     2 millig      not ap     
     # 11-1-5   40   56 X-rays  0.25  grays      Extern
     # 11-1-6   80   56 X-rays  0.25  grays      Extern
     # 11-1-7   80   56 X-rays  0.25  grays      Extern
     # 11-1-8  120   56 X-rays  0.25  grays      Extern
     # 11-1-9   50   56 X-rays     1  grays      Extern
    # 11-1-10   40   56 X-rays     1  grays      Extern
    # 11-1-11   36   56 X-rays     1  grays      Extern
    # 11-1-12   57   56 X-rays     1  grays      Extern
    # 11-1-13   35   56 X-rays     4  grays      Extern
    # 11-1-14   40   56 X-rays     4  grays      Extern
    # 11-1-15   52   56 X-rays     4  grays      Extern
    # 11-1-16   60   56 X-rays     4  grays      Extern
    # 11-1-17   39   56 accel.  0.05  grays      Extern
    # 11-1-18   80   56 accel.  0.05  grays      Extern
    # 11-1-19   76   56 accel.  0.05  grays      Extern
    # 11-1-20  120   56 accel.  0.05  grays      Extern
    # 11-1-21   38   56 accel.  0.15  grays      Extern
    # 11-1-22   49   56 accel.   0.2  grays      Extern
    # 11-1-23   40   56 accel.   0.2  grays      Extern
    # 11-1-24   38   56 accel.   0.2  grays      Extern
    # 11-1-25   60   56 accel.   0.2  grays      Extern
    # 11-1-26   20   56 accel.   0.8  grays      Extern
    # 11-1-27   40   56 accel.   0.8  grays      Extern
    # 11-1-28   40   56 accel.   0.8  grays      Extern
    # 11-1-29   60   56 accel.   0.8  grays      Extern
    # 11-1-30   40   56 neutro   0.1  grays      Extern
    # 11-1-31   80   56 neutro   0.1  grays      Extern
    # 11-1-32   78   56 neutro   0.1  grays      Extern
    # 11-1-33  120   56 neutro   0.1  grays      Extern
    # 11-1-34  120   56 neutro  0.15  grays      Extern
    # 11-1-35   50   56 neutro   0.3  grays      Extern
    # 11-1-36   40   56 neutro   0.3  grays      Extern
    # 11-1-37   40   56 neutro   0.3  grays      Extern
    # 11-1-38   57   56 neutro   0.5  grays      Extern
    # 11-1-39   40   56 neutro     1  grays      Extern
    # 11-1-40   40   56 neutro     1  grays      Extern
    # 11-1-41   40   56 neutro     1  grays      Extern
    # 11-1-42   57   56 neutro     1  grays      Extern
    # 11-1-43   60   56 neutro   1.5  grays      Extern
    # 11-1-44   39   56 neutro  0.15  grays      Extern
    # 11-1-45   80   56 neutro  0.15  grays      Extern
    # 11-1-46   76   56 neutro  0.15  grays      Extern
    # 11-1-47  120   56 neutro  0.15  grays      Extern
    # 11-1-48   50   56 neutro   0.5  grays      Extern
    # 11-1-49   40   56 neutro   0.5  grays      Extern
    # 11-1-50   38   56 neutro   0.5  grays      Extern
    # 11-1-51   60   56 neutro   0.5  grays      Extern
    # 11-1-52   20   56 neutro   1.5  grays      not ap
    # 11-1-53   40   56 neutro   1.5  grays      not ap
    # 11-1-54   76   56 neutro   1.5  grays      Extern
    # 11-1-55   57   56 neutro   1.5  grays      Extern
    # 11-1-56   79   56 none c     0 unit c      not ap
    # 11-1-57   40   49 estrog     2 millig      Implan
    # 11-1-58   82   40 ovarec  <NA> unit c      not ap
    # 11-1-59   78   49 estrog     2 millig      Implan
    # 11-1-60   40   56 X-rays   0.1  grays      Extern
    # 11-1-61   78   56 X-rays   0.1  grays      Extern
    # 11-1-62   78   56 X-rays   0.1  grays      Extern
    # 11-1-63  120   56 X-rays   0.1  grays      Extern
    # 11-1-64   49   56 X-rays   0.3  grays      Extern
    # 11-1-65   78   56 X-rays   0.3  grays      Extern
    # 11-1-66   38   56 X-rays   0.3  grays      Extern
    # 11-1-67   60   56 X-rays   0.3  grays      Extern
    # 11-1-68   20   56 X-rays     1  grays      Extern
    # 11-1-69   98   56 X-rays     1  grays      Extern
    # 11-1-70   38   56 X-rays     1  grays      Extern
    # 11-1-71   60   56 X-rays     1  grays      Extern
    # 11-1-72   20   56 X-rays     2  grays      Extern
    # 11-1-73   36   56 X-rays     2  grays      Extern
    # 11-1-74   40   56 X-rays     2  grays      Extern
    # 11-1-75   54   56 X-rays     2  grays      Extern
    # 11-1-76   40   56 accel.  0.02  grays      Extern
    # 11-1-77   80   56 accel.  0.02  grays      Extern
    # 11-1-78   78   56 accel.  0.02  grays      Extern
    # 11-1-79  117   56 accel.  0.02  grays      Extern
    # 11-1-80   49   56 accel.  0.08  grays      Extern
    # 11-1-81   40   56 accel.  0.08  grays      Extern
    # 11-1-82   40   56 accel.  0.08  grays      Extern
    # 11-1-83   60   56 accel.  0.08  grays      Extern
    # 11-1-84   20   56 accel.  0.32  grays      Extern
    # 11-1-85   40   56 accel.  0.32  grays      Extern
    # 11-1-86   40   56 accel.  0.32  grays      Extern
    # 11-1-87   57   56 accel.  0.32  grays      Extern
    # 11-1-88   40   56 neutro  0.04  grays      Extern
    # 11-1-89   80   56 neutro  0.04  grays      Extern
    # 11-1-90   80   56 neutro  0.04  grays      Extern
    # 11-1-91  120   56 neutro  0.04  grays      Extern
    # 11-1-92  120   56 neutro  0.05  grays      Extern
    # 11-1-93   50   56 neutro  0.12  grays      Extern
    # 11-1-94   40   56 neutro  0.12  grays      Extern
    # 11-1-95   36   56 neutro  0.12  grays      Extern
    # 11-1-96   60   56 neutro  0.12  grays      Extern
    # 11-1-97   20   56 neutro   0.4  grays      Extern
    # 11-1-98   40   56 neutro     1  grays      Extern
    # 11-1-99   40   56 neutro   0.4  grays      Extern
   # 11-1-100   60   56 neutro   0.4  grays      Extern
   # 11-1-101   38   56 neutro   0.5  grays      Extern
   # 11-1-102   40   56 neutro  0.05  grays      Extern
   # 11-1-103   80   56 neutro  0.05  grays      Extern
   # 11-1-104  156   56 neutro  0.05  grays      Extern
   # 11-1-105  117   56 neutro  0.05  grays      Extern
   # 11-1-106   50   56 neutro  0.15  grays      Extern
   # 11-1-107   40   56 neutro  0.15  grays      Extern
   # 11-1-108   40   56 neutro  0.15  grays      Extern
   # 11-1-109   57   56 neutro  0.15  grays      Extern
   # 11-1-110   20   56 neutro   0.5  grays      Extern
   # 11-1-111   40   56 neutro   0.5  grays      Extern
   # 11-1-112   36   56 neutro   0.5  grays      Extern
   # 11-1-113   57   56 neutro   0.5  grays      Extern
   # 11-1-114  114   56 none c     0 unit c      not ap
   # 11-1-115   40   49 estrog     2 millig      Inject
   # 11-1-116   52   40 ovarec  <NA> unit c      not ap
   # 11-1-117   84   49 estrog     2 millig      Implan
   # 11-1-118   37   56 X-rays  0.25  grays      Extern
   # 11-1-119   80   56 X-rays  0.25  grays      Extern
   # 11-1-120   76   56 X-rays  0.25  grays      Extern
   # 11-1-121  120   56 X-rays  0.25  grays      Extern
   # 11-1-122   49   56 X-rays     1  grays      Extern
   # 11-1-123   40   56 X-rays     1  grays      Extern
   # 11-1-124   40   56 X-rays     1  grays      Extern
   # 11-1-125   60   56 X-rays     1  grays      Extern
   # 11-1-126   30   56 X-rays     2  grays      Extern
   # 11-1-127   57   56 X-rays     2  grays      Extern
   # 11-1-128   19   56 X-rays     4  grays      Extern
   # 11-1-129   66   56 X-rays     4  grays      Extern
   # 11-1-130   60   56 X-rays     4  grays      Extern
   # 11-1-131   87   56 X-rays     4  grays      Extern
   # 11-1-132   40   56 accel. 0.005  grays      Extern
   # 11-1-133   80   56 accel. 0.005  grays      Extern
   # 11-1-134   80   56 accel. 0.005  grays      Extern
   # 11-1-135  123   56 accel. 0.005  grays      Extern
   # 11-1-136   50   56 accel.  0.02  grays      Extern
   # 11-1-137   40   56 accel.  0.02  grays      Extern
   # 11-1-138   40   56 accel.  0.02  grays      Extern
   # 11-1-139   60   56 accel.  0.02  grays      Extern
   # 11-1-140   40   56 accel.  0.05  grays      Extern
   # 11-1-141   19   56 accel.  0.08  grays      Extern
   # 11-1-142   40   56 accel.  0.08  grays      Extern
   # 11-1-143   18   40 ovarec  <NA> unit c      not ap
   # 11-1-144   57   56 accel.  0.08  grays      Extern
   # 11-1-145   38   56 neutro   0.1  grays      Extern
   # 11-1-146   80   56 neutro   0.1  grays      Extern
   # 11-1-147  152   56 neutro   0.1  grays      Extern
   # 11-1-148  117   56 neutro   0.1  grays      Extern
   # 11-1-149   51   56 neutro   0.3  grays      Extern
   # 11-1-150   40   56 neutro   0.3  grays      Extern
   # 11-1-151   78   56 neutro   0.3  grays      Extern
   # 11-1-152   60   56 neutro   0.3  grays      Extern
   # 11-1-153   20   56 neutro     1  grays      Extern
   # 11-1-154   40   56 neutro     1  grays      Extern
   # 11-1-155   40   56 neutro     1  grays      Extern
   # 11-1-156   60   56 neutro     1  grays      Extern
   # 11-1-157   51   56 neutro   1.5  grays      Extern
   # 11-1-158   38   56 neutro  0.15  grays      Extern
   # 11-1-159   78   56 neutro  0.15  grays      Extern
   # 11-1-160   76   56 neutro  0.15  grays      Extern
   # 11-1-161  120   56 neutro  0.15  grays      Extern
   # 11-1-162   50   56 neutro   0.5  grays      Extern
   # 11-1-163   40   56 neutro   0.5  grays      Extern
   # 11-1-164   38   56 neutro   0.5  grays      Extern
   # 11-1-165   60   56 neutro   0.5  grays      Extern
   # 11-1-166   18   56 neutro   1.5  grays      Extern
   # 11-1-167   40   56 neutro   1.5  grays      Extern
   # 11-1-168   38   56 neutro   1.5  grays      Extern
   # 11-1-169   60   56 neutro   1.5  grays      Extern
   
 #   Group.ID    n  age   type  dose   unit application                 
     # 11-2-1   44   56 none c     0  grays      not ap
     # 11-2-2   40   56 X-rays     2  grays      Extern
     # 11-2-3   40   56 X-rays     4  grays      Extern
     # 11-2-4   40   56 accel.   0.2  grays      Extern
     # 11-2-5   40   56 accel.   0.3  grays      Extern
     # 11-2-6   40   56 accel.   0.3  grays      Extern
     # 11-2-7  100   56 none c     0 unit c      not ap
     # 11-2-8   60   56 X-rays   0.2  grays      Extern
     # 11-2-9   60   56 X-rays     1  grays      Extern
    # 11-2-10   60   56 X-rays     2  grays      Extern
    # 11-2-11   60   56 X-rays     2  grays      Extern
    # 11-2-12  120   49 estrog     2 millig      Implan
    # 11-2-13   58   56 X-rays     2  grays      Extern
    # 11-2-14   60   56 X-rays     2  grays      Extern
    # 11-2-15   40   49 estrog     2 millig      Implan
    # 11-2-16   40   56 gamma-     2  grays      Extern
    # 11-2-17   60   56 gamma-     2  grays      Extern
    # 11-2-18   60   56 gamma-     2  grays      Extern
    # 11-2-19   60   56 gamma-     2  grays      Extern
    # 11-2-20   60   56 accel.   0.1  grays      Extern    
    # 11-2-21   60   56 accel.   0.2  grays      Extern
    # 11-2-22   60   56 accel.   0.2  grays      Extern
    # 11-2-23   40   56 none c     0 unit c      not ap
    # 11-2-24   30   56 none c     0 unit c      not ap
    # 11-2-25   40   56 accel.   0.1  grays      Extern
    # 11-2-26   56   56 accel.   0.1  grays      Extern
    # 11-2-27   40   56 accel.   0.1  grays      Extern
    # 11-2-28   60   56 accel.   0.1  grays      Extern
    # 11-2-29   59   56 X-rays   0.4  grays      Extern
    # 11-2-30   60   56 X-rays   0.4  grays      Extern
    # 11-2-31   40   56 none c     0 unit c      not ap
    # 11-2-32   40   49 estrog     2 millig      Implan
    # 11-2-33   60   49 estrog     2 millig      Implan
    # 11-2-34   40   56 gamma-   0.3  grays      Extern
    # 11-2-35   80   56 gamma-   0.3  grays      Extern
    # 11-2-36   40  117 gamma-   0.3  grays      Extern
    # 11-2-37   80  117 gamma-   0.3  grays      Extern
    # 11-2-38   40   56 gamma-   0.3  grays      Extern
    # 11-2-39   96   56 gamma-   0.3  grays      Extern
    # 11-2-40   40   56 gamma-   1.2  grays      Extern
    # 11-2-41   80   56 gamma-   1.2  grays      Extern
    # 11-2-42   40  117 gamma-   1.2  grays      Extern
    # 11-2-43   82  117 gamma-   1.2  grays      Extern
    # 11-2-44   40  117 gamma-   1.2  grays      Extern
    # 11-2-45   92  117 gamma-   1.2  grays      Extern
    # 11-2-46   61   56 X-rays   0.8  grays      Extern
    # 11-2-47   40   56 X-rays   1.2  grays      Extern
    # 11-2-48  270   56 neutro     1  grays      Extern    
    # 11-2-49   60   56 none c     0 unit c      not ap
    # 11-2-50   60   56 none c     0 unit c      not ap
    # 11-2-51  140   56 none c     0 unit c      not ap
    # 11-2-52   40   49 estrog     2 millig      Implan
    # 11-2-53   80   56 X-rays 0.025  grays      Extern
    # 11-2-54   80   56 X-rays   0.1  grays      Extern
    # 11-2-55   40   56 X-rays  0.25  grays      Extern
    # 11-2-56   80   56 X-rays  0.25  grays      Extern    
    # 11-2-57   80   56 X-rays   0.4  grays      Extern
    # 11-2-58   80   56 X-rays   0.4  grays      Extern
    # 11-2-59   20   56 X-rays     1  grays      Extern
    # 11-2-60   40   56 X-rays     1  grays      Extern    
    # 11-2-61   40   56 X-rays     2  grays      Extern
    # 11-2-62   40   56 X-rays     2  grays      Extern
    # 11-2-63   80   56 X-rays     2  grays      Extern
    # 11-2-64   80   56 X-rays     2  grays      Extern
    # 11-2-65   80   56 X-rays     2  grays      Extern
    # 11-2-66   80   56 X-rays     2  grays      Extern
    # 11-2-67   76   56 X-rays     2  grays      Extern
    # 11-2-68   80   56 X-rays     2  grays      Extern
    # 11-2-69   80   56 X-rays     2  grays      Extern
    # 11-2-70   80   56 X-rays     2  grays      Extern
    # 11-2-71   40   56 X-rays     2  grays      Extern
    # 11-2-72   40   56 X-rays     2  grays      Extern
    # 11-2-73   80   56 X-rays     2  grays      Extern
    # 11-2-74   80   56 X-rays     2  grays      Extern
    # 11-2-75   20   56 X-rays     4  grays      Extern
    # 11-2-76   40   56 X-rays     4  grays      Extern
    # 11-2-77   40   56 X-rays     4  grays      Extern
    # 11-2-78   40   56 gamma-     4  grays      Extern
    # 11-2-79   19   56 gamma-     1  grays      Extern
    # 11-2-80    4   56 gamma-     1  grays      Extern
    # 11-2-81    7   56 gamma-     2  grays      Extern
    # 11-2-82   80   56 accel. 0.005  grays      Extern
    # 11-2-83   80   56 accel.  0.02  grays      Extern
    # 11-2-84  120   56 accel.  0.05  grays      Extern
    # 11-2-85   80   56 accel.  0.05  grays      Extern
    # 11-2-86   80   56 accel.   0.1  grays      Extern
    # 11-2-87   20   56 accel.   0.2  grays      Extern
    # 11-2-88   40   56 accel.   0.2  grays      Extern
    # 11-2-89   20   56 neutro 0.075  grays      Extern
    # 11-2-90   38   56 neutro  0.15  grays      Extern
    # 11-2-91   78   56 neutro  0.15  grays      Extern
    # 11-2-92   40   56 neutro   0.5  grays      Extern
    # 11-2-93   39   56 neutro   0.5  grays      Extern
    # 11-2-94   20   56 neutro   0.5  grays      Extern
    # 11-2-95   42   56 neutro   0.5  grays      Extern
    # 11-2-96   20   56 neutro   1.5  grays      Extern
    # 11-2-97   40   56 neutro   1.5  grays      Extern
    # 11-2-98  100   56 none c     0 unit c      not ap
    # 11-2-99   84   56 none c     0 unit c      not ap
   # 11-2-100   60   56 X-rays  0.02  grays      Extern
   # 11-2-101   59   56 X-rays  0.02  grays      Extern
   # 11-2-102   60   56 X-rays  0.08  grays      Extern
   # 11-2-103   59   56 X-rays  0.08  grays      Extern
   # 11-2-104   60   56 X-rays  0.08  grays      Extern
   # 11-2-105   60   56 X-rays  0.08  grays      Extern
   # 11-2-106   68   56 X-rays   0.1  grays      Extern
   # 11-2-107   60   56 X-rays   0.1  grays      Extern
   # 11-2-108   60   56 X-rays   0.4  grays      Extern
   # 11-2-109   60   56 X-rays   0.4  grays      Extern
   # 11-2-110   60   56 X-rays  0.04  grays      Extern
   # 11-2-111   60   56 X-rays  0.04  grays      Extern
   # 11-2-112   60   56 X-rays  0.04  grays      Extern
   # 11-2-113   60   56 X-rays  0.04  grays      Extern
   # 11-2-114   60   56 X-rays  0.04  grays      Extern
   # 11-2-115   60   56 X-rays  0.04  grays      Extern
   # 11-2-116   60   56 X-rays   1.6  grays      Extern
   # 11-2-117   36   56 X-rays   1.6  grays      Extern
   # 11-2-118   60   56 X-rays   1.6  grays      Extern
   # 11-2-119   39   56 X-rays   1.6  grays      Extern
   # 11-2-120   40   56 X-rays   1.6  grays      Extern
   # 11-2-121   60   56 X-rays   1.6  grays      Extern
   # 11-2-122   60   56 X-rays     2  grays      Extern
   # 11-2-123   56   56 X-rays     2  grays      Extern
   # 11-2-124   60   56  -rays   0.4  grays      Extern
   # 11-2-125   60   56  -rays   0.4  grays      Extern
   # 11-2-126    1   56  -rays   0.6  grays      Extern
   # 11-2-127   43   56  -rays   1.6  grays      Extern
   # 11-2-128    1   56  -rays   1.6  grays      Extern
   # 11-2-129    9   56  -rays   1.6  grays      Extern
   # 11-2-130   62   56 accel.  0.04  grays      Extern
   # 11-2-131   84   56 accel.  0.04  grays      Extern
   # 11-2-132   60   56 accel.  0.08  grays      Extern
   # 11-2-133   58   56 accel.   0.2  grays      Extern
   # 11-2-134   57   56 accel.   0.2  grays      Extern
      
   
 #   Group.ID    n  age   type  dose   unit application                 
      # 2-1-1 1135 <NA> none c     0 workin      Inhala
      # 2-1-2  688 <NA> none c     0 workin      Inhala
      # 2-1-3  240 <NA> none c     0 workin      Inhala
      # 2-1-4  313 <NA> none c     0 workin      Inhala
      # 2-1-5  262 <NA> none c     0 workin      Inhala

      
 #   Group.ID    n  age   type  dose   unit application                 
     # 2-10-1   77   92 gamma-     6  grays      Extern
     # 2-10-2   20   92 gamma-    10  grays      Extern
     # 2-10-3   40   92 gamma-    12  grays      Extern
     # 2-10-4   20   92 gamma-    13  grays      Extern
     # 2-10-5   36   92 gamma-  16.5  grays      Extern
     # 2-10-6   40   92 gamma-  18.5  grays      Extern
     # 2-10-7   20   92 gamma-    20  grays      Extern
     # 2-10-8   20   92 gamma-    26  grays      Extern
     # 2-10-9   30   92 gamma-    28  grays      Extern
    # 2-10-10   20   92 gamma-    31  grays      Extern
    # 2-10-11   20   92 gamma-    39  grays      Extern
    # 2-10-12   15   92 gamma-     8  grays      Extern
    # 2-10-13   13   92 gamma-     9  grays      Extern
    # 2-10-14   24   92 gamma-   9.5  grays      Extern
    # 2-10-15   20   92 gamma-    12  grays      Extern
    # 2-10-16   20   92 gamma-    19  grays      Extern
    # 2-10-17   20   92 gamma-    24  grays      Extern
    # 2-10-18   20   92 gamma-  28.5  grays      Extern
    # 2-10-19   40   92 gamma-     6  grays      Extern
    # 2-10-20   38   92 gamma-    10  grays      Extern
    # 2-10-21   87   92 gamma-    12  grays      Extern
    
 #   Group.ID    n  age   type  dose   unit application                 
     # 2-11-1   10   92 gamma-  2.66  grays      Extern
     # 2-11-2   20   92 gamma-  5.96  grays      Extern
     # 2-11-3   10  -28 gamma-  2.66  grays      Extern
     # 2-11-4    5  -28 gamma-  2.66  grays      Extern
     # 2-11-5   65   -8 gamma-  2.66  grays      Extern
     # 2-11-6   66   -8 gamma-  2.66  grays      Extern
     # 2-11-7   63   -8 gamma-  5.96  grays      Extern
     # 2-11-8   69   -8 gamma-  5.96  grays      Extern
     # 2-11-9   56   92 gamma- 14.74  grays      Extern
    # 2-11-10   48   92 gamma- 14.74  grays      Extern
    # 2-11-11  505   92 gamma-     1  grays      Extern
    # 2-11-12  289  275 gamma-  2.83  grays      Extern
    # 2-11-13  120    9 gamma-     3  grays      Extern
    # 2-11-14   60    9 gamma-     3  grays      Extern
    # 2-11-15  120    9 gamma-    33  grays      Extern
    
 #   Group.ID    n  age   type  dose   unit application                    
     # 2-12-1  150   92 neutro 0.012  grays      Extern
     # 2-12-2  150   92 neutro  0.02  grays      Extern
     # 2-12-3   80   92 neutro  0.06  grays      Extern
     # 2-12-4   78   92 neutro   0.1  grays      Extern
     # 2-12-5   75   92 neutro  0.32  grays      Extern
     # 2-12-6   75   92 neutro  0.49  grays      Extern
     # 2-12-7  123   92 neutro   1.5  grays      Extern
     # 2-12-8  104   92 neutro   2.3  grays      Extern
     # 2-12-9   20   92 neutro   2.4  grays      Extern
    # 2-12-10   20   92 neutro  2.54  grays      Extern
    # 2-12-12   60   92 neutro   3.5  grays      Extern
    # 2-12-13   20   92 neutro  3.86  grays      Extern
    # 2-12-14   40   92 neutro   4.4  grays      Extern
    # 2-12-15   40   92 neutro   5.3  grays      Extern
    # 2-12-16   20   92 neutro     6  grays      Extern
    # 2-12-17   20   92 neutro     8  grays      Extern
    # 2-12-18  116   92 neutro   0.4  grays      Extern
    # 2-12-19   79   92 neutro   0.6  grays      Extern
    # 2-12-20  119   92 neutro   0.6  grays      Extern
    # 2-12-21  392   92 neutro   0.6  grays      Extern
    # 2-12-22   80   92 neutro  1.15  grays      Extern
    # 2-12-23   50   92 neutro   1.2  grays      Extern
    # 2-12-24   40   92 neutro  1.73  grays      Extern
    # 2-12-25  120   92 neutro     2  grays      Extern
    # 2-12-26   40   92 neutro  2.86  grays      Extern
    # 2-12-27   58   92 neutro     4  grays      Extern
    # 2-12-28   12   92 accel.     5  grays      Extern
    # 2-12-29   36   92 accel.   7.5  grays      Extern
    # 2-12-30   24   92 accel.   7.5  grays      Extern
    # 2-12-31   11   92 accel.    10  grays      Extern
    # 2-12-32   34   92 accel.    15  grays      Extern
    # 2-12-33   10   92 accel.    20  grays      Extern
    # 2-12-34    9   92 accel.    25  grays      Extern
    # 2-12-35   12   92 accel.     5  grays      Extern
    # 2-12-36   24   92 accel.   3.7  grays      Extern
    # 2-12-37   24   92 accel.   3.7  grays      Extern
    # 2-12-38   24   92 accel.  11.6  grays      Extern
    # 2-12-39   24   92 accel.  18.4  grays      Extern
    # 2-12-40   48   92 accel.  44.6  grays      Extern
    # 2-12-41   60   92 Bremss   2.5  grays      Extern
    # 2-12-42   26   92 Bremss   2.5  grays      Extern
    # 2-12-43   24   92 neutro   3.5  grays      Extern
    # 2-12-44   32   92 neutro     8  grays      Extern
    # 2-12-45   23   92 neutro     2  grays      Extern
    # 2-12-46   22   92 neutro     2  grays      Extern
    # 2-12-47  566   92 none c     0  grays      Extern
    # 2-12-48  152   92 neutro    25  grays      Extern
    # 2-12-49  255  427 neutro    25  grays      Extern
    # 2-12-50  249   92 neutro    25  grays      Extern
    # 2-12-51   51   92 neutro    25  grays      Extern
    
 #   Group.ID    n  age   type  dose   unit application                       
     # 2-13-1   50   92 neutro   1.2  grays      Extern
     # 2-13-2   50   92 para-d   500 parts       Inhala
     # 2-13-3   50   92 para-d   500 parts       Inhala
     # 2-13-4   56   92 para-d   500 parts       Inhala
     # 2-13-5   50   92 para-d   500 parts       Inhala
     # 2-13-6  100   92 neutro   1.2  grays      Extern
     # 2-13-7  100   92 neutro   1.2  grays      Extern
     # 2-13-8  100   92 neutro   1.2  grays      Extern
     # 2-13-9   50   92 tetrac    15 parts       Inhala
    # 2-13-10   60   92 tetrac    15 parts       Inhala
    # 2-13-11   50   92 tetrac    50 millig      Inhala
    # 2-13-12   50   92 tetrac    50 millig      Inhala
    # 2-13-13   29   92 tetrac   150 millig      Inhala
    # 2-13-14   28   92 tetrac   150 millig      Inhala
    # 2-13-15   50   92 tetrac   150 millig      Inhala
    # 2-13-16   50   92 tetrac   150 millig      Inhala
    # 2-13-17  100   92 neutro   1.2  grays      Extern
    # 2-13-18  100   92 neutro   1.2  grays      Extern
    # 2-13-19  100   92 neutro   1.2  grays      Extern
    # 2-13-20  100   92 neutro   1.2  grays      Extern

 #   Group.ID    n  age   type  dose   unit application                       
     # 2-14-1   30   92 neutro   1.5  grays      Extern
     # 2-14-2   40   92 neutro   2.3  grays      Extern
     # 2-14-3   40   92 neutro   1.5  grays      Extern
     # 2-14-4   40   92 neutro   1.5  grays      Extern
     # 2-14-5   40   92 neutro   1.5  grays      Extern
     # 2-14-6   40   92 neutro   2.3  grays      Extern
     # 2-14-7   12   92 neutro   2.8  grays      Extern
     # 2-14-8   10   92 neutro  0.15  grays      Extern
     # 2-14-9   20   92 neutro  0.15  grays      Extern
    # 2-14-10   10   92 neutro  0.23  grays      Extern
    # 2-14-11   20   92 neutro  0.23  grays      Extern
    # 2-14-12   10   92 neutro   0.3  grays      Extern
    # 2-14-13   20   92 neutro   0.3  grays      Extern
    # 2-14-14    9   92 neutro  0.45  grays      Extern
    # 2-14-15   20   92 neutro  0.45  grays      Extern
    # 2-14-16    8   92 neutro   0.4  grays      Extern
    # 2-14-17    8   92 neutro  0.75  grays      Extern
    # 2-14-18   32   92 neutro  0.75  grays      Extern
    # 2-14-19    8   92 neutro   1.1  grays      Extern
    # 2-14-20   32   92 neutro   1.1  grays      Extern
    # 2-14-21   20   92 neutro   1.5  grays      Extern
    # 2-14-22    8   92 neutro   1.5  grays      Extern
    # 2-14-23   32   92 neutro   1.5  grays      Extern
    # 2-14-24    8   92 neutro   2.2  grays      Extern
    # 2-14-25   32   92 neutro   2.2  grays      Extern
    # 2-14-26   30   92 neutro   2.3  grays      Extern
    # 2-14-27   20   92 neutro   2.3  grays      Extern
    # 2-14-28   40   92 neutro   2.3  grays      Extern
    # 2-14-29   36   92 neutro     0  grays      Extern
    # 2-14-30   40   92 neutro   1.5  grays      Extern
    # 2-14-31   40   92 neutro   1.5  grays      Extern
    # 2-14-32   10   92 neutro   1.5  grays      Extern
    # 2-14-33   10   92 neutro   1.5  grays      Extern
    # 2-14-34   30   92 neutro   2.3  grays      Extern
    # 2-14-35   40   92 neutro   2.3  grays      Extern
    # 2-14-36   20   92 neutro     0  grays      Extern
    # 2-14-37   20   92 neutro   2.3  grays      Extern
    # 2-14-38   30   92 neutro   2.3  grays      Extern
    # 2-14-59   20   92 gamma-     4  grays      Extern
    # 2-14-60   20   92 gamma-     8  grays      Extern
    # 2-14-61   20   92 gamma-    12  grays      Extern
    # 2-14-62   18   92 gamma-    16  grays      Extern
    # 2-14-63   48   92 gamma-    20  grays      Extern
    # 2-14-64   48   92 gamma-    15  grays      Extern
    # 2-14-65   24   92 gamma-    20  grays      Extern
    # 2-14-66   48   92 gamma-    20  grays      Extern


 #   Group.ID    n  age   type  dose   unit application                       
      # 3-1-1  353   91 none c     0  grays      Extern
      # 3-1-2  100   91 X-rays  0.04  grays      Extern
      # 3-1-3   84   91 X-rays   0.8  grays      Extern
      # 3-1-4   53   91 X-rays   1.6  grays      Extern
      # 3-1-5   58   91 X-rays   3.2  grays      Extern
      # 3-1-6   57   91 X-rays   6.4  grays      Extern
      # 3-1-7   60   91 X-rays  1.28  grays      Extern
      # 3-1-8   55   91 X-rays  2.56  grays      Extern
      # 3-1-9  279   91 none c     0  grays      Extern
     # 3-1-10  165   91 neutro  0.05  grays      Extern
     # 3-1-11  150   91 neutro  0.01  grays      Extern
     # 3-1-12   95   91 neutro  0.02  grays      Extern
     # 3-1-13   96   91 neutro  0.04  grays      Extern
     # 3-1-14   92   91 neutro  0.08  grays      Extern
     # 3-1-15   48   91 neutro  0.16  grays      Extern


 #   Group.ID    n  age   type  dose   unit application                       
      # 3-2-1  758   35 X-rays     0  grays      Extern
      # 3-2-2   44   35 X-rays   0.5  grays      Extern
      # 3-2-3  108   35 X-rays     1  grays      Extern
      # 3-2-4  139   35 X-rays     2  grays      Extern
      # 3-2-5  110   35 X-rays     3  grays      Extern
      # 3-2-6  137   35 X-rays     4  grays      Extern
      # 3-2-7  125   35 X-rays     5  grays      Extern
      # 3-2-8   58   35 X-rays     6  grays      Extern
      # 3-2-9  133   35 X-rays     7  grays      Extern
     # 3-2-10  193   35 none c     0  grays      Extern
     # 3-2-11   49   35 neutro  0.17  grays      Extern
     # 3-2-12   47   35 neutro  0.36  grays      Extern
     # 3-2-13   48   35 neutro  0.71  grays      Extern
     # 3-2-14   49   35 neutro  1.07  grays      Extern
     # 3-2-15   49   35 neutro  1.43  grays      Extern
     # 3-2-16   96   35 neutro  1.79  grays      Extern
     # 3-2-17   22   35 neutro  2.14  grays      Extern
     # 3-2-18  202   35 neutro 0.025  grays      Extern
     # 3-2-19  148   35 neutro  0.05  grays      Extern
     # 3-2-20  105   35 neutro   0.1  grays      Extern
     # 3-2-21   74   35 neutro  0.17  grays      Extern
     # 3-2-22   53   35 neutro 0.025  grays      Extern
     # 3-2-23   54   35 neutro 0.036  grays      Extern
     # 3-2-24   54   35 neutro 0.535  grays      Extern
     # 3-2-25   52   35 neutro  0.71  grays      Extern

 #   Group.ID    n  age   type  dose   unit application                           
      # 3-3-1   60   92 none c     0  grays      Extern
      # 3-3-2   60   92 X-rays     1  grays      Extern
      # 3-3-3   60   92 X-rays     3  grays      Extern
      # 3-3-4   59   92 X-rays     5  grays      Extern
      # 3-3-5   57   92 X-rays     7  grays      Extern
            
 #   Group.ID    n  age   type  dose   unit application                           
      # 3-4-1  561   92 none c     0  grays      Extern
      # 3-4-2   44   92 X-rays   0.5  grays      Extern
      # 3-4-3  108   92 X-rays     1  grays      Extern
      # 3-4-4  139   92 X-rays     2  grays      Extern
      # 3-4-5  110   92 X-rays     3  grays      Extern
      # 3-4-6  137   92 X-rays     4  grays      Extern
      # 3-4-7  125   92 X-rays     5  grays      Extern
      # 3-4-8   58   92 X-rays     6  grays      Extern
      # 3-4-9  133   92 X-rays     7  grays      Extern
     # 3-4-10  335   92 X-rays     9  grays      Extern
     # 3-4-11   49   92 neutro  0.17  grays      Extern
     # 3-4-12   47   92 neutro  0.36  grays      Extern
     # 3-4-13   48   92 neutro  0.71  grays      Extern
     # 3-4-14   49   92 neutro  1.07  grays      Extern
     # 3-4-15   49   92 neutro  1.43  grays      Extern
     # 3-4-16   96   92 neutro  1.79  grays      Extern
     # 3-4-17   22   92 neutro  2.41  grays      Extern
     
 #   Group.ID    n  age   type  dose   unit application                           
      # 3-5-1   34   -4 none c     0  grays      Extern
      # 3-5-2   39   -4 none c     0  grays      Extern
      # 3-5-3   48   -4 X-rays   0.3  grays      Extern
      # 3-5-4   40   -4 X-rays   0.3  grays      Extern
      # 3-5-5   61   -4 X-rays   0.9  grays      Extern
      # 3-5-6   44   -4 X-rays   0.9  grays      Extern
      # 3-5-7   46   -4 X-rays   1.5  grays      Extern
      # 3-5-8   50   -4 X-rays   1.5  grays      Extern
      # 3-5-9   45   -4 X-rays   2.1  grays      Extern
     # 3-5-10   38   -4 X-rays   2.1  grays      Extern
     # 3-5-11   51   -4 neutro  0.09  grays      Extern
     # 3-5-12   44   -4 neutro  0.09  grays      Extern
     # 3-5-13   44   -4 neutro  0.27  grays      Extern
     # 3-5-14   35   -4 neutro  0.27  grays      Extern
     # 3-5-15   27   -4 neutro  0.45  grays      Extern
     # 3-5-16   31   -4 neutro  0.45  grays      Extern
     # 3-5-17   35   -4 neutro  0.62  grays      Extern
     # 3-5-18   34   -4 neutro  0.62  grays      Extern
     # 3-5-19  430   92 none c     0  grays      Extern
     # 3-5-20   44   92 X-rays   0.5  grays      Extern
     # 3-5-21   48   92 X-rays     1  grays      Extern
     # 3-5-22   50   92 X-rays     2  grays      Extern
     # 3-5-23   50   92 X-rays     3  grays      Extern
     # 3-5-24   48   92 X-rays     4  grays      Extern
     # 3-5-25   68   92 X-rays     5  grays      Extern
     # 3-5-26   58   92 X-rays     6  grays      Extern
     # 3-5-27   74   92 X-rays     7  grays      Extern
     # 3-5-28   49   92 neutro  0.17  grays      Extern
     # 3-5-29   47   92 neutro  0.36  grays      Extern
     # 3-5-30   48   92 neutro  0.71  grays      Extern
     # 3-5-31   49   92 neutro  1.07  grays      Extern
     # 3-5-32   49   92 neutro  1.43  grays      Extern
     # 3-5-33   96   92 neutro  1.79  grays      Extern
     # 3-5-34   22   92 neutro  2.14  grays      Extern
     # 3-5-35   46  580 none c     0  grays      Extern
     # 3-5-36   47  580 X-rays   0.5  grays      Extern
     # 3-5-37   44  580 X-rays     1  grays      Extern
     # 3-5-38   47  580 X-rays     2  grays      Extern
     # 3-5-39   48  580 X-rays     3  grays      Extern
     # 3-5-40   46  580 X-rays     4  grays      Extern
     # 3-5-41   71  580 X-rays     5  grays      Extern
     # 3-5-42   85  580 X-rays     6  grays      Extern
     # 3-5-43   58  580 X-rays     7  grays      Extern
     # 3-5-44   50  580 neutro  0.17  grays      Extern
     # 3-5-45   48  580 neutro  0.36  grays      Extern
     # 3-5-46   51  580 neutro  0.71  grays      Extern
     # 3-5-47   49  580 neutro  1.07  grays      Extern
     # 3-5-48   49  580 neutro  1.43  grays      Extern
     # 3-5-49   42  580 neutro  1.79  grays      Extern
     # 3-5-50   46  580 neutro  2.14  grays      Extern
     
    
 #   Group.ID    n  age   type  dose   unit application                           
      # 3-6-1   60   92 none c     0  grays      Extern
      # 3-6-2   60   92 X-rays     1  grays      Extern
      # 3-6-3   60   92 X-rays     2  grays      Extern
      # 3-6-4   59   92 X-rays     5  grays      Extern
      # 3-6-5   57   92 X-rays     7  grays      Extern
      # 3-6-6   54   92 none c     0  grays      Extern
      # 3-6-7   56   92 X-rays     1  grays      Extern
      # 3-6-8   51   92 X-rays     2  grays      Extern
      # 3-6-9   64   92 X-rays     5  grays      Extern
     # 3-6-10   51   92 X-rays     7  grays      Extern
     # 3-6-11   56   92 none c     0  grays      Extern
     # 3-6-12   83   92 neutro  0.01  grays      Extern
     # 3-6-13   81   92 neutro  0.02  grays      Extern
     # 3-6-14   62   92 neutro  0.04  grays      Extern
     # 3-6-15   66   92 neutro  0.08  grays      Extern
     # 3-6-16   71   92 neutro  0.18  grays      Extern
     # 3-6-17   71   92 none c     0  grays      Extern
     # 3-6-18   60   92 neutro  0.01  grays      Extern
     # 3-6-19   79   92 neutro  0.02  grays      Extern
     # 3-6-20   75   92 neutro  0.04  grays      Extern
     # 3-6-21   61   92 neutro  0.08  grays      Extern
     # 3-6-22   71   92 neutro  0.12  grays      Extern
     # 3-6-23   80   92 neutro  0.18  grays      Extern
     # 3-6-24   74 <NA>   <NA>  <NA>   <NA>        <NA>
     
 #   Group.ID    n  age   type  dose   unit application                           
      # 9-4-1  155   84 none c     0 unit c      not ap
      # 9-4-2  144   84 X-rays   100 roentg      Extern
      # 9-4-3  149   84 X-rays   175 roentg      Extern
      # 9-4-4  107   84 X-rays   350 roentg      Extern
      # 9-4-5   76   84 X-rays   500 roentg      Extern
      # 9-4-6  198   84 X-rays   650 roentg      Extern
      # 9-4-7   57   84 X-rays   750 roentg      Extern
      # 9-4-8   19   84 X-rays   900 roentg      Extern
      
     # 9-4-10  204   84 X-rays   650 roentg      Extern
     # 9-4-11  154   84 X-rays  1000 roentg      Extern
     # 9-4-12  196   84 X-rays  1100 roentg      Extern
     # 9-4-13   74   84 X-rays  1200 roentg      Extern
     # 9-4-14  290   84 X-rays   100 roentg      Extern
     # 9-4-15  272   84 X-rays   175 roentg      Extern
     # 9-4-16  182   84 X-rays   350 roentg      Extern
     
     # 9-4-18  260   84 X-rays   650 roentg      Extern
     # 9-4-19  178   84 X-rays  1000 roentg      Extern
     # 9-4-20  162   84 X-rays  1200 roentg      Extern
     # 9-4-21   80   84 X-rays  1350 roentg      Extern
     
     
     # 9-4-24  131   28 none c     0 unit c      not ap
     # 9-4-25  100   28 X-rays   350 roentg      Extern
     # 9-4-26   69   28 X-rays   650 roentg      Extern
     # 9-4-27  196   28 X-rays   350 roentg      Extern
     # 9-4-28  192   28 X-rays   650 roentg      Extern
     # 9-4-29  101   28 X-rays   200 roentg      Extern
     # 9-4-30   98   28 X-rays   300 roentg      Extern
     # 9-4-31  143   28 X-rays   400 roentg      Extern
     
     # 9-4-33  102   28 X-rays   700 roentg      Extern
     # 9-4-34  100   28 X-rays  1000 roentg      Extern
     # 9-4-35    7   28 X-rays  1500 roentg      Extern
     # 9-4-36  212   28 X-rays   200 roentg      Extern
     # 9-4-37  216   28 X-rays   300 roentg      Extern
     # 9-4-38  194   28 X-rays   400 roentg      Extern
     # 9-4-39  196   28 X-rays   500 roentg      Extern
     # 9-4-41  206   28 X-rays  1000 roentg      Extern
     # 9-4-42   88   28 X-rays  1500 roentg      Extern
     

 #   Group.ID    n  age   type  dose   unit application                           
      # 9-5-1  324   84 none c  <NA> unit c      not ap
      # 9-5-2  193   84 X-rays  0.25  grays      Extern
      # 9-5-3  196   84 X-rays   0.5  grays      Extern
      # 9-5-4  198   84 X-rays     1  grays      Extern
      # 9-5-5  149   84 X-rays     2  grays      Extern
      # 9-5-6   94   84 X-rays     4  grays      Extern
      # 9-5-7  113   84 X-rays     6  grays      Extern
      # 9-5-8  111   84 X-rays  0.25  grays      Extern
      # 9-5-9  110   84 X-rays   0.5  grays      Extern
     # 9-5-10  115   84 X-rays     1  grays      Extern
     # 9-5-11   74   84 X-rays     2  grays      Extern
     # 9-5-12   74   84 X-rays     4  grays      Extern
     # 9-5-13   78   84 X-rays     6  grays      Extern
     # 9-5-14  254   84 neutro  0.02  grays      Extern
     # 9-5-15  225   84 neutro  0.06  grays      Extern
     # 9-5-16  190   84 neutro  0.18  grays      Extern
     # 9-5-17  176   84 neutro  0.54  grays      Extern
     # 9-5-18  141   84 neutro  1.65  grays      Extern
     # 9-5-19  130   84 neutro     3  grays      Extern
          
 #   Group.ID    n  age   type  dose   unit application                                
      # 9-6-1  473   84 none c     0 unit c      not ap
      # 9-6-2  242   84 X-rays  0.25  grays      Extern
      # 9-6-3  239   84 X-rays   0.5  grays      Extern
      # 9-6-4  246   84 X-rays     1  grays      Extern
      # 9-6-5  217   84 X-rays     2  grays      Extern
      # 9-6-6  143   84 X-rays     4  grays      Extern
      # 9-6-7  188   84 X-rays     6  grays      Extern
      # 9-6-8  108   84 X-rays  0.25  grays      Extern
      # 9-6-9  112   84 X-rays   0.5  grays      Extern
     # 9-6-10  116   84 X-rays     1  grays      Extern
     # 9-6-11  115   84 X-rays     2  grays      Extern
     # 9-6-12  118   84 X-rays     4  grays      Extern
     # 9-6-13  117   84 X-rays     6  grays      Extern
     # 9-6-14  106   84 X-rays     1  grays      Extern
     # 9-6-15   93   84 X-rays     2  grays      Extern
     # 9-6-16  115   84 X-rays     4  grays      Extern
     # 9-6-17  195   84 neutro  0.02  grays      Extern
     # 9-6-18  182   84 neutro  0.18  grays      Extern
     # 9-6-19  210   84 neutro  0.18  grays      Extern
     # 9-6-20  135   84 neutro  1.65  grays      Extern
     # 9-6-21   95   84 neutro     3  grays      Extern
     # 9-6-22  196   84 neutro  0.18  grays      Extern
     # 9-6-23  232   84 neutro  0.54  grays      Extern
     # 9-6-24  196   84 neutro  1.65  grays      Extern
     # 9-6-25   96   84 neutro  0.06  grays      Extern
     # 9-6-26   94   84 neutro  0.18  grays      Extern
     # 9-6-27   90   84 neutro  0.54  grays      Extern
     
 #   Group.ID    n  age   type  dose   unit application                                    
      # 9-7-1  105    7 none c     0 unit c      not ap
      # 9-7-2   47    7 neutro 0.125  grays      Extern
      # 9-7-3  102    7 neutro  0.25  grays      Extern
      # 9-7-4  105    7 neutro   0.5  grays      Extern
      # 9-7-5   84    7 neutro     1  grays      Extern
      # 9-7-6   31   21 neutro 0.125  grays      Extern
      # 9-7-7  112   21 neutro  0.25  grays      Extern
      # 9-7-8  121   21 neutro   0.5  grays      Extern
      # 9-7-9  102   21 neutro     1  grays      Extern
     # 9-7-10   72    7 X-rays   0.5  grays      Extern
     # 9-7-11   70    7 X-rays     1  grays      Extern
     # 9-7-12   85    7 X-rays     3  grays      Extern
     # 9-7-13   66   21 X-rays   0.5  grays      Extern
     # 9-7-14   76   21 X-rays     1  grays      Extern
     # 9-7-15   83   21 X-rays     3  grays      Extern
     
 #   Group.ID    n  age   type  dose   unit application                                      
      # 9-8-1   68   84 none c     0 unit c      not ap
      # 9-8-2   57   84 X-rays   0.5  grays      Extern
      # 9-8-3   57   84 X-rays     1  grays      Extern
      # 9-8-4   54   84 X-rays     2  grays      Extern
      # 9-8-5   58   84 X-rays     4  grays      Extern
      # 9-8-6   57   84 X-rays     6  grays      Extern
      # 9-8-7  120   84 X-rays   0.5  grays      Extern
      # 9-8-8  112   84 X-rays     1  grays      Extern
      # 9-8-9  112   84 X-rays     2  grays      Extern
     # 9-8-10  114   84 X-rays     4  grays      Extern
     # 9-8-11  112   84 X-rays     6  grays      Extern
     # 9-8-12  112   84 X-rays   0.5  grays      Extern
     # 9-8-13  116   84 X-rays     1  grays      Extern
     # 9-8-14  226   84 X-rays     2  grays      Extern
     # 9-8-15  226   84 X-rays     4  grays      Extern
     # 9-8-16  118   84 X-rays     6  grays      Extern




###################################################################
#
# Double Vision
# 27 Feb 2013
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



## It appears that animals which recieved multiple treatments appear in multiple rows.  This is clearly true in 11-1 and 11-2 and is probably a MAJOR GENERAL ISSUE.
## Since I have discovered the doubled rows issue I will ignore n, type, dose, and unit from 11-2 onward.  I suggest these will need to be rechecked after fixing the afore problem.



## I need to perform this same style of analysis for gender
## I need to spot check these results against original papers to see if I can find additional errors.
## All need to be checked to ensure that the age for controls matches the age for the animals they were used as control for.  The point here is that control animals were not likely to have been picked from birth.  Rather they probably entered the study at the same age as the other animals.
## I need to check all of these for species and strain as well
## Check mean and median exposure times vs the dataset (they frequently seem wrong)'



## Age of exposure in study 1002-1 is consistently labled as 400 days yet the study protocol says that exposure was given between 8 and 15 months of age.  I should resolve this
## There is no dose rate in study 1002-1 I should try to find what this was.
## Study 1002-1 notes that some dogs were bred after exposure.  It would be useful to know which ones were.
## 1002-1 has something called fractionation interval which is a bit mysterious to me.  But I think it needs to be encoded
## I should remove this group 1002.1.99 which were 'not assigned'
## 1003.20 I'm not sure the age at exposure is actually always 100.  I imagine that our data has more/better detail on the actual age of exposure
## 1003.20.20 has only 5 animals, it should have 200
## 1003.20.20-41 are missing or almost missing from the era.  Perhaps they are in janus?
## 1003.20.42 has only 187 animals, it should have 200
## 1003.20.47-48 claim they are 0.8 neutrons here, but they are listed as 0.2 neutrons in the era description.  Double check these.
## double check age at first irradiation on janus 1003.21 (and all of janus, 1003)
## 1003.22.1-14 have doses that are 10x lower in the description than in the data I have
## 1003.22.22 has a dose of 12 in the table, but only 1.2 in the database.  Which is right?
## 1003.23.7-8 are listed as gamma in the data but neutron in the descriptions
## 1003.24.4 is listed as 4.17 in the data and 417 in the era description.  I am inclined to trust the data because it is consistent with treatment 1003.24.3
## 1003.24.11-18 claim to have an age of first exposure of 515, but this does not jive well with the study description claiming a single test exposure at age 520, especially since the MAS in many of these groups is greater than 520
## 1003.27.3 has 200 mice in the data but claims it should have 455 in the description
## 1003.27.1 says its type sham exposure in the data but is listed as none in the description.  This is as opposed to 1003.27.2 which is listed as a sham exposure in both sources.
## 1003.29.23 is listed as 4 Gy in the description, but as 0.4 Gy in the raw data.  I am inclined to believe the data because it is consistent with 1003.29.24, but I need to be sure.
## 1003.30.1-4 dose is listed as <NA> but it should be zero as it is in other control cases.
## 1003.30.3-4 and 7-10 and 13-20 are all injected with radioprotectors and should be removed from this analysis.  Also, the fact that these are listed as neutron and gamma exposures while the controls are listed as saline control and WR-2721 is decieving, this should be made consistent in the database.  The same problem occurs with the units which are sometimes grays and sometimes units of solution.  And the Application which is 'not applied' rather than 'external' 
## All - I've been looking for something that will clearly define what 'mean after survival' means.  The best I have found is an old Grahn paper that says 'The mean after-survival from the start of daily exposure is the endpoint statistic'.  I take that to mean that they are measuring days lived after the initiation of treatment. - (Grahn 1962 - http://www.osti.gov/energycitations/product.biblio.jsp?osti_id=4635223)
## 1003.5.1, 7, and 8 are missing.  One is especially important because it is a control condition.
## 1003.5.2 notes that '18 are terminated' this is rather important as these 18 will surely affect survival estiamtes.
## 1003.5 doses in the data are in rads while the database is in mGy.  I should simply convert everything in rads over to mGy.
## 1003.51-5 has no description.  I will need to cross check it using an alternate source.  Also doses, units, and application are clearly wrong, so I imagine I will have to import this data from the beagle dog archive.  However it might be worth removing this data.
## 1003.6.1 and 15 appear to be missing.  But likely they have been combined into 1003.6.99.  They need to be seperated because 9 were terminated and the others were controls.  They also need to be given proper values for age, type, and so on.
## 1003.6.9 claims to have 21 animal but on the website it is listed as having only 20.  I will need to check which is right.
## 1003.6 the dose is given in rads per day.  Obviously days of treatment will need to be added.
## 1003.6 age at exposure is given by the mean age of exposure, but we could get the actual true first age of exposure from the database.
##  1003.7 The number of dogs in each group does not match whats stated in the description.
## 1003.7 Units of Roentigen per day must be changed to gray if they can be
## 1005.47 This data from ITRI has no description.  Also dose and age information are missing, so we probably need to fill in all of this guy if we are going to use it.
## 1007.2.1 has one more mouse in the data than in the description
## 1007.2.7 has two less mice in the data than in the description 
## 1007.2.11 has two more mice in the data than in the description
## 1007.2.16 has two less mice in the data than in the description
## 1007.3.17, 18 and 19 missing, 18 and 19 are serial sacrifices, so its probalby fine.  17 is the highest dose group so it would be nice to find what happend to them.
## 1007.3.16 the data has 2165 mice while the description reports that there are 3707
## 1007.3.7 is reported to have 0.3 Gray doses in the data and 3 gray does in the description.  I think the description is more sensible here because it fits the progression.
## 1007.3.1 and 8 have units in rads even though they are control animals with a dose of zero and the rest have units in grays
## 1008.3 is insufficiently described in era to validate.  It must be checked against a third party if it is to be used at all.
## 11-1-4 claims to have 40 mice in the description, but there are 80 in our dataset.
## 11-1 has lots of estrogen and overectomy treatments that probably ought to be removed.
## 11-1-6 and 7 claims to have 40 mice in the description, but there are 80 in our dataset.
## 11-1-8 claims to have 40 mice in the description, but there are 120 in our dataset.
## 11-1 I am not going to continue to proof n for now.  Suffice it to say that many are wrong.  It will be helpful to find a second source.
## 11-1 I was not able to check the age at first exposure I will need an external source for this   
## 11-2-12 is listed as having 58 rats in the description, but the data has 120 examples
## 11-2-13 is listed as having 60 rats in the description, but the data has 58 examples.  Notably this is the (incorrect) number listed in the description for 11-2-12.  So maybe someone skipped 120 on data entry?
## 11-2-15 is listed as including 20 rats in the description.  In the data there are 40.
## 11-2-20 does not appear in the description.  Instead 21 appears twice.  I suggest that the first of these apperances at a dose of 0.1 Gy should actually be labeled 20
## 11-2-33 is listed has including 30 rats in the description, but includes 60 rats in the data
## there are many mistakes in the number of rats listed from 11-2-34 to 48
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
## Add material from janus not in here
## Cross reference janus materials against these
## Tell era that they need to remove newlines
## Add details to dog studies 1003.51, 1003.52, 1003.54, 1003.55, 1003.6, 1003.7, 1005.47 these might need to be removed completely
## remove serial sacrifices from 1007.3
## Tell era that 18.1, 19.2, 19.4-19.7, and 9.9 are missing the tag "No individual level data" in their csvs.
## Label those study groups which include a chemical treatment so I can easily discriminate them.
## What is the total dose rate in 1002.1
