# ###############################################################
# # ddref.R
# # April 2013
# #
# # Use data from archives to determine the dose and dose rate
# # effectiveness factor for lifespan studies involving external
# # radiation.
# #
# # The working definition of DDREF will be borrowed from the
# # BEIR VII report:
# #
# # 	"This value for DDREF can be estimated from a fit of the acute 
# #	 data using the relationship described above (i.e., E = αD + βD2).
# #	 Thus, the DDREF = [(αD + βD2)/D]/(αD/D)=(αD + βD2)/αD, which 
# #	 equals 1 + Dβ/α or 1 + D/(α/β)"
# #	
# # 	http://www.nap.edu/openbook.php?record_id=11340&page=44
# #
# # I will ignore issues of fractionation and dose rate for the time
# # being.

	# ### Libraries
	# library(plyr)
	# library(ggplot2)
	# library(survival)

	# ### Data
	# setwd('~/janus/scripts')
	# data <- readRDS('../data/external5.rds')

	# ### Helpers
	# createSurvivalFrame <- function(f.survfit){
			
		# # initialise frame variable
		# f.frame <- NULL
		
		# # create vector for strata identification
		# f.strata <- NULL
		# for(f.i in 1:length(f.survfit$strata)){
			# # add vector for one strata according to number of rows of strata
			# f.strata <- c(
				# f.strata, 
				# rep(names(f.survfit$strata)[f.i], 
				# f.survfit$strata[f.i])
			# )
		# }
			
		# # create data.frame with data from survfit 
		# # (create column for strata)
		# f.frame <- data.frame(
			# time=f.survfit$time, 
			# n.risk=f.survfit$n.risk, 
			# n.event=f.survfit$n.event, 
			# n.censor = f.survfit$n.censor, 
			# surv=f.survfit$surv, 
			# upper=f.survfit$upper, 
			# lower=f.survfit$lower, 
			# strata=factor(f.strata)
		# )
	
		# # create ﬁrst two rows (start at 1) for each strata
		# for(f.i in 1:length(f.survfit$strata)){
			
			# # take only subset for this strata from data
			# f.subset <- subset(
				# f.frame, 
				# strata==names(f.survfit$strata)[f.i]
			# )
			# # create ﬁrst two rows (time: 0, time of ﬁrst event)
			# f.start <- data.frame(
				# time=c(0, f.subset$time[1]), 
				# n.risk=rep(f.survfit[f.i]$n, 2), 
				# n.event=c(0,0), 
				# n.censor=c(0,0), 
				# surv=c(1,1), 
				# upper=c(1,1), 
				# lower=c(1,1), 
				# strata=rep(names(f.survfit$strata)[f.i],2)
			# )
			# # add ﬁrst two rows to dataset
			# f.frame <- rbind(f.start, f.frame)
			# # remove temporary data
			# rm(f.start, f.subset)
		# }
		# # reorder data
		# f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
		# # rename row.names
		# rownames(f.frame) <- NULL
	
		# # return frame
		# return(f.frame)
	# }

	
	# ### Clean
	
	# # reported in Roentgens
	# s <- data$cluster %in% c(
		# '1003-2-1', 
		# '1003-20-1', 
		# '1003-21-1', 
		# '1003-22-1', 
		# '1003-24-1', 
		# '1003-27-1', 
		# '1003-29-1', 
		# '1002-1-1'
	# )
	# data$dose[s] <- data$dose[s] * 0.0094
	# # Remove Clusters
	# s <- c(
		# '1003-30-1', 	# two treatments are tightly spaced
		# '1003-26-1',		# doesn't fit description
		# '2-10-1',		# doesn't fit description
		# '2-11-1',		# doesn't fit description
		# '3-1-1',		# doesn't fit description
		# '1003-25-1', 	# lifespan
		# '1003-5-1', 	# lifespan
		# '1003-6-1', 	# lifespan
		# '1003-7-1'		# lifespan
		
	# )
	# data <- data[!data$cluster %in% s,]
	# # high LET / local
	# data <- data[!data$quality %in% c(
		# 'accel. alpha local',
		# 'accel. alpha whole body',
		# 'accel. neutrons 0.1-10 MeV',
		# 'neutrons 1-10 MeV',
		# 'neutrons C-252',
		# 'neutrons fission',
		# 'neutrons>10 MeV',
		# 'X-rays local'
	# ),]
	# # Impossible lifespans
	# s <- data$cluster %in% c('11-2-16') & data$lifespan > 2000
	# data <- data[!s,]



	# ### Group
	# data$group <- paste(
		# data$cluster,
		# data$strain, 
		# #data$dose_rate, 
		# data$quality,
		# #data$fractions,
		# #data$fraction_interval,
		# data$other_treatments
	# )
	# data <- ddply(head(data, nrow(data)), .(cluster), function(df){
		# cat(length(unique(df$dose)), ' ')
		# if(length(unique(df$dose)) < 2){ return(NULL) }
		# controls <- df[df$dose == 0,]
		# if(nrow(controls) < 1) { return(df) }
		# for(group in unique(df$group)){
			# controls$group <- group
			# df <- rbind(df, controls)
		# }
		# df
	# })
	# data <- ddply(data, .(group), function(df){
		# doses <- sum(!is.na(unique(df$dose)))
		# cat(doses, df$group[1], '\n')
		# if(doses < 3) { return(NULL) }
		# df
	# })
	
	
	# ### Show
	# g <- data
	# g <- g[!is.na(g$cluster),]
	# g$n <- as.numeric(as.factor(g$group))
	# g$name <- paste(g$cluster, g$n)
	# ggplot(g, aes(dose, lifespan, group=factor(dose))) + 
		# geom_boxplot(outlier.size = 0.5) +
		# facet_wrap(~name, scales='free_y') + 
		# theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		# theme(strip.text.x = element_text(size = 6)) +
		# xlim(0, 10) +
		# stat_smooth(aes(group=1), method = "lm", formula=y~x+I(x^2))
	
	
	
	# ### Show survival

	# # multiple strata
	# t.Surv <- Surv(data$lifespan, rep(1, nrow(data)))
	# t.survfit <- survfit(t.Surv~group + dose, data=data)
	# t.survframe <- createSurvivalFrame(t.survfit)

	# # fix names
	# split <- strsplit(as.character(t.survframe$strata), 'dose=')
	# t.survframe$dose <- as.numeric(laply(split, function(x) x[2]))
	# t.survframe$group <- laply(split, function(x) x[1])
	# t.survframe$group_name <- as.numeric(as.factor(t.survframe$group))
	# t.survframe$strata <- as.numeric(as.factor(t.survframe$strata))

	# # Plot
	# ggplot(data=t.survframe, aes(color=dose, group=strata)) + 
		# geom_step(
			# aes(x=time, y=surv), 
			# direction="hv"
		# ) + 
		# facet_wrap(~group_name, scales='free_x')

	# # Cumulative Hazards
	# ggplot(data=t.survframe, aes(color=dose, group=strata)) + 
		# geom_step(
			# aes(x=time, y=-log(surv)), 
			# direction="hv"
		# ) + 
		# facet_wrap(~group_name, scales='free_x') +
		# ylim(0, 4)
	
	# # A key
	# data$group_number <- as.numeric(as.factor(data$group))
	# key <- ddply(data, .(group_number), function(df){
		# df[1, c(
			# 'cluster',
			# 'strain', 
			# 'dose_rate', 
			# 'quality',
			# 'fractions',
			# 'fraction_interval',
			# 'other_treatments'
		# )]	
	# })
	# write.csv(key, file='temp.csv')
	# groups <- unique(paste(data$group_number, data$group))
	
	
	# # Cox Model
	# r <- dlply(data, .(group), function(df){
		# s <- Surv(df$lifespan, rep(1, nrow(df)))
		# coxph(s ~ dose + I(dose^2), data=df)
	# })
	# r2 <- ldply(r, function(cox){
		# s 		<- summary(cox)
		# coef 	<- s$coefficients
		# a 		<- coef[1,'coef']
		# a.se 	<- coef[1,'se(coef)']
		# B 		<- coef[2,'coef']
		# B.se 	<- coef[2,'se(coef)']
		# ddref_1Gy<-1 + B/a
		# ddref_se<- ddref_1Gy * ((a.se/a)^2 + (B.se/B)^2)^0.5
		
		# data.frame(
			# coef, 
			# term=rownames(coef), 
			# ddref_1gy=ddref_1Gy,
			# ddref_se=ddref_se,
			# n=s$n
		# )
	# })
	
	# # Show
	# r2$group_name <- as.numeric(as.factor(r2$group))
	# ggplot(r2, aes(group_name, exp.coef., color=term)) + 
		# geom_point() + 
		# geom_errorbar(aes(
			# ymin=exp(coef-se.coef.), 
			# ymax=exp(coef+se.coef.) 
		# ))

	# ggplot(r2, aes(group_name, ddref_1gy, color=factor(group_name %% 5))) + 
		# geom_point() + 
		# geom_errorbar(aes(
			# ymin=ddref_1gy-ddref_se, 
			# ymax=ddref_1gy+ddref_se 
		# )) +
		# ylim(-5,7) + 
		# geom_hline(aes(yintercept=1))
		
		

###############################################################
#
# What are the thresholds?
# April 2013
#
# DDREF is defined ambiguosly.  It can be derived from acute
# exposures as 1 + Dβ/α  where response ~  D*α + D*β^2.  But for 
# the purposes of radiation protection, we need a functional 
# definition, that distinguishes a cutoff of dose and doserate
# beyond which the DDREF correction ought to be used.  What are 
# these cutoffs?  I will do a literature search to find out.
#
# ICRP 2007 3.2.1 (70-73)
# DDREF is 2 based on LLS dose response curves, 'experimental
# data', and 'probabilistic uncerainty analysis' conducted by others
# (NCRP, 1997, EPA, 1999, NCI/CDC, 2003, Annex A).
#
# ICRP 2007 A.3.1 (A 62)
# When dose rates are lower than around 0.1 Gy/hour there is repair 
# of cellular radiation injury during the irradiation. This causes 
# the b component to decrease and to reach zero at very low dose 
# rates. The a component is not modiﬁable by changing dose rate. 
#
# BEIR VII
#  Chapter 2
# How B decreases with dose rate (Edwards and others 1989)
# Estimate DDREF from animal data (Tucker and others 1998)
# More DDREF from animals data (Lorenz and others 1994)
# More DDREF in animals studies (Ullrich and Storer 1979a; Ullrich and others 1987)
#  Chapter 10
# DREF is the term for dose rate reduction, as opposed to both
# dose and dose rate reduction
# pg 246 claims that up to 24 hours seems to be required for
#        full repair from a single dose
# pg 248 Says LSS DDREF are roughly equivilant to UNSCEAR DDREF
#        estimated at 1 Sv of exposure.
# pg 250 used a cutoff of 1.5 - 2 Gy for animal data to avoid
#        leveling off effects!!!!!!
# pg 254 when fractionated, dose response can be described as
#        a*D + B*(D^2/K) where K is the number of fractions
# pg 255 they were forced to use mean survival times, instead of
#        lifespan, because they did not have individual level data!
#        They admit this is problematic, something I can address!
# pg 255 importantly they ignore data that does not account for 
#        competing sources of risk.  I do not know if I can do this!
# 		 But perhaps this is rather irrelevant in the case of lifespan
#		 most of these studies 
# pg 255 They only used the accute exposures from Edwards (1992)
#        excluding tables 1 and 2
# 
# References
# Edwards 1989, Chromosome aberrations in human lymphocytes
# Edwards 1992, Low Dose and Low Dose Rate Effects in Laboratory Animals
# Lorenz 1994, Dose and dose-rate dependence of the frequency of hprt deficient T lymphocytes in the spleen of the 137Cs gamma-irradiated mouse
# Tucker 1998, The accumulation of chromosome aberrations and Dlb-1 mutations in mice with highly fractionated exposure to gamma radiation
# Ullrich 1979a, Influence of gamma irradiation on the development of neoplastic disease in mice. I. Reticular tissue tumors
# Ullrich 1987, Myeloid leukemia in male RFM mice following irradiation with fission spectrum neutrons or gamma rays.
#

# Results:
#
# 	1. Survival hazard was not used in BEIR VII, only mean lifespan
#      this means I am doing something unique.
#   2. BEIR VII used 1.5 Sv as an upper threshold, because response
#      falls off above this level, I should too.
#   3. When doses are fractionated apply the equation:
#      a*D + B*(D^2/K), where K is the number of fractions
#   4. Dealing with doserate changes is hard, pg 246 suggests that
#      repair processes take up to 24 hours, so this might be a
#      natural break point.
#	5. Estimate DDREF at 1 Sv to make it compatible with lss estimates

###############################################################
#
# Combining DDREF
# April 2013
#
# The aim of this research is to employ many estimates of DDREF
# to produce a single final estimate.  But as DDREF is a ratio
# of two coefficients, it does not fit a neat normal distribution
# and is therefore not clear to me how it should be merged.
#
# Next I will read literature on this problem and try to
# determine a suitable approach.

# Notes
# Beir VII pgs 254 - 260 describe their methodology.  In breif
# they tried fixing the ratio a/B at a range of values and then
# calculating the maximum likelihood of fitted values (allowing
# a and B to move together) across a range of a/B ratios.  They
# determined the total likelihood of the model by multiplying
# the likelihoods of the individual models.
# pg 255 indicates that only data from oak ridge was used, not
# much to go on!  http://www.jstor.org/stable/3575012?seq=1




###################################################################	
# Cumulative ERR
# May 2013
#
# Survival curves are a great way to look at time course data
# but it is difficult to see the nature of the relationship between
# a variable and multiple survival curves.
#
# For example if mortality is increasing as the square of dose, this
# is not visually obvious from a survival curve.
#
# We could show relative mortality vs dose, but mortality estimates 
# are quite variable for any given time point, and this variation 
# might obscure the relationship.  Instead we might display
# cumulative relative mortality vs dose.  Cumulative mortality
# is better behaved than point estimates of mortality and so the
# relative cumulative mortality should also be better behaved.  Excess
# relative risk should also be similar for cumulative estimates vs
# point estimates.
#
# Here I want to try and draw these graphs I propose.

	# Libraries
	library(plyr)
	library(ggplot2)
	library(reshape2)

	# Constants
	baseline_mortality <- function(time) 0.000001*time^2
	times <- 1:300
	observations_per_group = 100
	err <- function(dose) 1 + dose + dose^2
	doses <- 0:10
	
	# Data
	groups <- rep(observations_per_group, length(doses))
	names(groups) <- doses
	survival <- data.frame(llply(groups))
	names(survival) <- names(groups)
	for(time in times){
		for(d in (1:length(doses))){
			dose <- doses[d]
			mortality <- baseline_mortality(time) * err(dose)
			groups[d] <- sum(runif(groups[d]) > mortality)
		}
		survival <- rbind(survival, groups)
	}
	survival$time <- c(0, times)

	# Survival
	survival <- melt(survival, id.vars='time')
	names(survival) <- c('time', 'dose', 'n')
	survival$dose <- as.numeric(as.character(survival$dose))
	survival$survival <- survival$n / observations_per_group
	survival$cumulative_mortality <- -log(survival$survival)
	survival$cumulative_err <- 
		survival$cumulative_mortality / 
		survival$cumulative_mortality[survival$dose == 0]
	
	
	# Graph
	ggplot(survival, 
		aes(time, survival, color=as.factor(dose))
	) + geom_path()
	
	g <- survival[
		abs(survival$cumulative_err) < Inf &
		!is.na(survival$cumulative_err)
	,]
	ggplot(g, 
		aes(dose, cumulative_err, color=time)
	) + geom_point()	 +
		geom_smooth(method='lm', formula=y~poly(x, 2))
	model <- glm(cumulative_err ~ I(dose^2) + time, data=g)
	summary(model)
	
		
# Results
#
# Surival curves actually mask a quadratic relation.  Cumulative
# excess relative risk makes it much more obvious (see graphs).
# Notably, the actual curve in the excess relative risk model is
# not a good approximation of the dose response curve, probably
# because points with high error bars are weighted equally and
# thereby bias the result.
#
# Survival Curves
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-05-20%20at%203.06.41%20PM.png
# 
# Cumulative ERR
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-05-20%20at%203.08.39%20PM.png
#
# Conclusions
#
# I recommend including generating the cumulative excess relative
# risk graphs in my report.



###############################################################
#
# Data Funnel
# April 2013
#
# Introduction:
# Create a data set that might be used for DDREF analysis and
# make a description of this data.

	@here
## DDREF.R now needs to handle data assigned to multiple clusters (maybe)


	focus_study = '11-1'

	# LIBRARIES
	library(plyr)
	library(ggplot2)
	library(survival)


	# DATA
	setwd('~/janus/scripts')
	d <- readRDS('../data/external5.rds')


	# HELPERS	
	# Make survival data amenable to ggplot
	surv_to_df <- function(surv){
		df <- with(surv, data.frame(
			time,
			n.risk,
			n.event,
			n.censor,
			surv,
			upper,
			lower,
			strata=rep(names(strata), strata)
		))
	}

	# Print treatment data
	get_treatment <- function(data){
		with(data, paste(
			round(dose, 3), 'Gy',
			'at', round(dose_rate, 3),'Gy/min', 
			'in', fractions, 'fractions'
		))
	}

	# Print group summaries
	group_summary <- function(data){
		.all <- function(x) paste(unique(x), collapse=' ')
		treatments <- get_treatment(data)
		with(data, c(
			study			= .all(study_id),
			strain			= .all(strain),
			males			= sum(sex == 'Male'),
			females			= sum(sex == 'Female'),
			mean_lifespan	= round(mean(lifespan)),
			sd_lifespan		= round(sd(lifespan)),
			age_at_treatment= round(mean(age_at_treatment)),
			treatments		= .all(treatments),
			other_treatments= .all(other_treatments),
			warnings		= .all(warning_reason)
		))
	}

	# Report for funnel graph
	count <- function(data){
		with(data, c(
			studies=	length(unique(file)),
			treatments=	length(unique(group_id)),
			animals=nrow(data),
			one_study=sum(data$study_id == focus_study),
			exclude=sum(data$exclude)
		))
	}
	
		
	# Define
	
	roentgens <- c(
		'1003-2-1', 
		'1003-20-1', 
		#'1003-21-1', 
		'1003-22-1', 
		'1003-24-1', 
		#'1003-27-1', 
		'1003-29-1', 
		'1002-1-1'
	)
	
	does_not_fit_description <- c(
		'1003-26-1',	
		'2-10-1',
		'2-11-1',
		'3-1-1'
	)
	
	treatments_too_close <- c(
		'1003-30-1'	
	)
	
	lifespan <- c(
		'1003-25-1',
		'1003-5-1',
		'1003-6-1',
		'1003-7-1'
	)
	
	bad_qualities <- c(
		'accel. alpha local',
		'accel. alpha whole body',
		'accel. neutrons 0.1-10 MeV',
		'neutrons 1-10 MeV',
		'neutrons C-252',
		'neutrons fission',
		'neutrons>10 MeV',
		'X-rays local', 
		'gamma-rays local'
	)
			
	threshold_dose <- 1.5
	
	
	# Fix
	
	# Convert Dose
	s <- d$cluster %in% roentgens
	d$dose[s] <- d$dose[s] * 0.0094
		
	# Add missing fractions
	d$fractions[is.na(d$fractions)] <- 1
	d$fractions[d$fractions == 0] <- 1
	
	# Add fractions seperated by days
	d$day_fractions <- d$fractions
	s <- d$fraction_interval < 1 & !is.na(d$fraction_interval)
	d$day_fractions[s] <- d$fractions[s] * d$fraction_interval[s]
	
	# Duplicate those in multiple clusters
	ddply(data, .(cluster), function(df){
		s <- strsplit(df$cluster[1], ' ')
		....
		
	})
	l <- llply(s, length)
			
	# FILTER
	
	# Initial counts
	count(d)		
	# 34 studies, 909 treatments, 128K animals
	
	# Only low-LET, whole body
	d <- d[!d$quality %in% bad_qualities,]     
	count(d)		
	# 34 studies, 540 treatments, 84K animals

	# Do not fit descriptions
	d <- d[!d$cluster %in% does_not_fit_description,]   
	count(d)		
	# 32 studies, 497 treatments, 79K animals
	
	# Treatments too tightly spaced
	d <- d[!d$cluster %in% treatments_too_close,]   
	count(d)		
	# 32 studies, 495 treatments, 78K animals
	
	# Lifespan treatments
	d <- d[!d$cluster %in% lifespan,]   
	count(d)		
	# 28 studies, 470 treatments, 75K animals

	# Outliers
	impossible_lifespan <- 
		d$cluster %in% c('11-2-16') & 
		d$lifespan > 2000
	d <- d[!impossible_lifespan,]
	count(d)		
	# 29 studies, 474 treatments, 77K animals
	
	# Dose too high (matching BEIR specs)
	d <- d[!d$dose > threshold_dose,] 
	count(d)		
	# 20 studies, 203 treatments, 47K animals

	# Missing dose data
	d <- d[!is.na(d$dose),]
	count(d)		
	# 20 studies, 202 treatments, 44K animals



	# Remove those that should be excluded
	d <- d[!d$exclude,]
	count(d)		
	# 20 studies, 201 treatments, 44K animals

	# Remove cases with few treatment groups	
	d <- ddply(d, .(cluster), function(df){
		doses <- length(unique(df$dose))
		if(doses < 3) { return(NULL) }
		df
	})
	count(d)		
	# 12 studies, 175 treatments, 27K animals
	
	# Warnings
	# show, but do not remove
	count(d[d$warning == FALSE,])
	# 9 studies, 97 treatments, 15K animals


	
	# Show
	
	# Summaries
	group_summaries <- ddply(d, .(group_id), function(df){
		group_summary(df)
	})
	cluster_summaries <- ddply(d, .(cluster), function(df){
		group_summary(df)
	})	
	write.csv(
		group_summaries, 
		file='results/ddref_group_summaries.csv'
	)
	write.csv(
		cluster_summaries, 
		file='results/ddref_cluster_summaries.csv'
	)

	# Save Data for later use
	write.csv(
		d,
		file='data/ddref.csv'
	)
	
	
# Results
#
# Data is filtered and saved as ddref.csv for later use.  
# See ddref_group_summaries.csv and ddref_cluster_summaries.csv
# for some pretty results.  See also the funnel graph.


###############################################################
#
# More BEIR
# April 2013
#
# Introduction:
# Use the BEIR proceedure to develop a better estimate of DDREF.


	@todo run excluding warnings
	@todo check that this proceedure is right

	# Libraries
	library(plyr)
	library(ggplot2)
	library(survival)
	
	# Data
	setwd('~/janus/scripts')
	d <- read.csv('data/ddref.csv')
	
	# Mean lifespan
	g <- d
	g$n <- as.numeric(as.factor(g$cluster))
	g$name <- paste(g$cluster, g$n)
	ggplot(g, aes(dose, lifespan, group=factor(dose), color=warning)) + 
		geom_boxplot(outlier.size = 0.5) +
		facet_wrap(~name, scales='free_y') + 
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		theme(strip.text.x = element_text(size = 6)) +
		xlim(-0.5, 2.5) +
		stat_smooth(aes(group=1), method = "lm", formula=y~x+I(x^2))
	# original
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-06-07%20at%209.46.04%20AM.png




	@stop

	# Survival
	t.Surv <- Surv(d$lifespan, rep(1, nrow(d)))
	t.survfit <- survfit(t.Surv~cluster+dose, data=d)
	t.survframe <- surv_to_df(t.survfit)
	capture <- function(start, end, x=t.survframe$strata){
		sub(end, '', sub(start, '', x))
	}
	t.survframe$dose <- as.numeric(capture('^.*dose=', ' .*$'))
	t.survframe$cluster <- capture('^.*cluster=', ', dose=.*$')
	t.survframe$cluster <- capture('^.*cluster=', ' .*$')
	t.survframe$strata <- as.numeric(t.survframe$strata)
	t.survframe$cluster <- paste(
		as.character(t.survframe$cluster),
		as.numeric(as.factor(t.survframe$cluster))
	)
	ggplot(data=t.survframe, aes(color=dose, group=strata)) + 
		geom_step(
			aes(x=time, y=surv), 
			direction="hv"
		) + 
		facet_wrap(~cluster, scales='free_x')

	# Cumulative Hazards
	ggplot(data=t.survframe, aes(color=dose, group=strata)) + 
		geom_step(
			aes(x=time, y=-log(surv)), 
			direction="hv"
		) + 
		facet_wrap(~cluster, scales='free_x') +
		ylim(0, 4)
	
	# A key
	d$cluster_number <- as.numeric(as.factor(d$cluster))
	key <- ddply(d, .(cluster_number), function(df){
		df[1, c(
			'cluster',
			'strain', 
			'dose_rate', 
			'quality',
			'fractions',
			'fraction_interval',
			'other_treatments'
		)]	
	})
	write.csv(key, file='temp.csv')
	clusters <- unique(paste(d$cluster_number, d$cluster))
	
	
	# Model
	# Go through various ratios B/a
	# build models with this ratio fixed so that
	#
	#   E ~ a*D + (r*a)*(D^2/K)
	#
	# where E is the -log(Survival), a is the coefficient of
	# the regression, r is the fixed ratio, D is dose, and K
	# is number of fractions.
	ratios <- seq(-1, 10, 0.1)
	models <- dlply(d, .(cluster), function(df){
		s <- Surv(df$lifespan, rep(1, nrow(df)))
		m <- llply(ratios, function(r){
			df$adj_dose <- df$dose + r*(df$dose^2)/df$fractions
			coxph(s ~ adj_dose, data=df)		
		})
		names(m) <- ratios
		m
	})
	log_likelihoods <- ldply(models, function(model_cluster){
		ldply(model_cluster, function(m){
			log_likelihoods <- m$loglik
			names(log_likelihoods) <- c('alternative', 'null')
			log_likelihoods
		})
	})
	scaled_likelihoods <- ddply(log_likelihoods, .(cluster), function(df){
		relative <- exp(df$null - df$alternative)
		df$scaled_likelihood <- relative / sum(relative)
		df
	})
	scaled_likelihoods <- ddply(scaled_likelihoods, .(.id), function(df){
		s <- prod(df$scaled_likelihood)
		rbind(
			df,
			data.frame(
				cluster='combined',
				.id=df[1,'.id'],
				alternative=NA,
				null=NA,
				scaled_likelihood=s
			)
		)
	})
	
	subset <- scaled_likelihoods$cluster == 'combined'
	scaled_likelihoods$scaled_likelihood[subset] <- 
		scaled_likelihoods$scaled_likelihood[subset] / 
		sum(scaled_likelihoods$scaled_likelihood[subset])
	
	# Show Independent models
	g <- scaled_likelihoods
	g$cluster <- substr(g$cluster, 0, 30)
	g$curvature <- as.numeric(g$.id)
	ggplot(g, aes(
		curvature, 
		scaled_likelihood, 
		group=cluster
	)) + geom_line() + facet_wrap(~cluster, scales='free')
	
	
###############################################################
#
# Bootstrap Estimates of DDREF
# May 2013
#
# Introduction:
# Here I will bootstrap to estimate the a/B ratio for each
# cluster, then save the result for use in meta-analysis.  I will
# also add the central estimate and variance to ddref.csv for
# later use.

	# Libraries
	library(plyr)
	library(ggplot2)
	library(survival)
	
	# Data
	setwd('~/janus/scripts')
	d <- read.csv('data/ddref.csv')

	# Configuration
	n.bootstraps <- 1000
	
	# Helpers
	bootstrap <- function(x) sample(x, length(x), replace=TRUE)
	get_ddref <- function(df){
		df$a <- df$dose
		df$B <- (df$dose)^2 / df$day_fractions
		s  <- Surv(df$lifespan, rep(1, nrow(df)))
		m <- coxph(s ~ a + B, data=df)
		r <- m$coefficients[c('a', 'B')]		
		r['ddref'] <- 1 + r['B']/r['a']
		r
	}
		
	# Get Bootstrap estimates
	ddref <- ddply(d, .(cluster), function(df){
		cat('\n', unique(df$cluster), ": ")
		ldply(1:n.bootstraps, function(b){
			cat('.')
			get_ddref(df[bootstrap(1:nrow(df)),])
		})
	})
		
	
	# Show Bootstrap Estimates
	ggplot(ddref, aes(ddref, color=cluster)) + 
		geom_density() +
		xlim(-5, 5)
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-05-23%20at%207.25.57%20PM.png
	
	
	# Add mean and variance to data
	summaries <- ddply(ddref, .(cluster), function(df){c(
		u=  mean(df$ddref, na.rm=T),
		o=  sd(df$ddref, na.rm=T)
	)})
	d <- ddply(d, .(cluster), function(df){
		s <- summaries[summaries$cluster == unique(df$cluster),]
		df$bootstrap_u <- s[1,'u']
		df$bootstrap_o <- s[1,'o']
		df
	})
	
	# Show Effect vs SE plot
	ggplot(summaries, aes(u, cluster)) + 
		geom_point() + 
		geom_errorbarh(aes(xmax=u+o, xmin=u-o)) + 
		geom_vline(x=1)
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-05-23%20at%207.17.26%20PM.png

	# Save
	write.csv(ddref, file='data/ddref_boostrap_estimates.csv')
	write.csv(d, file='data/ddref.csv')

# Results
#
# These error bars are pretty huge for some of these studies, I guess
# because alpha can be quite small in some cases.  I suppose its not
# too important, these studies with huge error bars will not count 
# for much in the summary statistics.  Including them is roughly 
# equivilant to including them.
#
# Perhaps more interesting is that in most cases 1 + B/a is less than
# one implying a negative curvature, that is acute doses are less 
# disruptive than fractionated ones.  We will see if this result holds
# as we clean up the data.

	
	
###############################################################
#
# Meta-analysis of Bootstrap estimates
# May 2013
#
# Introduction:
# I will apply a random effects model to estimate ddref across
# the studies.  I will follow the method layed out in Introduction
# to Meta Analysis 2009 (Chapte 12) using the DerSimonian and Laird
# method to measure T^2.

	# Libraries
	library(plyr)
	library(ggplot2)
	library(survival)

	# Data
	setwd('~/janus/scripts')
	ddref <- read.csv(file='data/ddref_boostrap_estimates.csv')
	d <- read.csv(file='data/ddref.csv')
	
	
	# Helpers
	
	# Fixed effect analysis
	estimate_meta_effect <- function(w, y){
		list(
			effect = sum(w*y)/sum(w),
			variance = 1/sum(w)
		)
	}

	# Estimate between study variance (tau2)
	# by DerSimonian and Laird method
	estimate_tau2 <- function(w, y){
		df <- length(w) - 1
		Q <- sum(w*y^2) - sum(w*y)^2/sum(w)
		C <- sum(w) - sum(w^2)/sum(w)
		
		(Q - df) / C
	}
	
	# Report
	report_effect <- function(u, v) {
		o <- v^0.5
		cat(u, '+/-', o*1.96)
	}
	
	# Get study effects
	get_summaries <- function(data){
		ddply(data, .(cluster), function(df){c(
			u =  mean(df$ddref, na.rm=T),
			v =  var(df$ddref, na.rm=T)
		)})
	}
	
	# Analyze
	analyze <- function(data){
		summaries <- get_summaries(data)
		summaries$fixed_w <- 1/summaries$v
		fixed <- with(summaries, estimate_meta_effect(fixed_w, u))
		tau2 <- with(summaries, estimate_tau2(fixed_w, u))
		summaries$random_v <- summaries$v + tau2
		summaries$random_w <- 1/summaries$random_v
		random <- with(summaries, estimate_meta_effect(random_w, u))
		report_effect(random$effect, random$variance)		
	}
	
	analyze(ddref)
	warning_clusters <- unique(d$cluster[d$warning])
	analyze(ddref[!ddref$cluster %in% warning_clusters,])

# Results
#
# We see a random effect 0.27 +/- 0.33.  Intersting and surprising
# result.  We will see how it develops.
	
	