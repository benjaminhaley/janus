# ddref.R
#
# Use data from archives to determine the dose and dose rate
# effectiveness factor for lifespan studies involving external
# radiation.
#
# The working definition of DDREF will be borrowed from the
# BEIR VII report:
#
# 	"This value for DDREF can be estimated from a fit of the acute 
#	 data using the relationship described above (i.e., E = αD + βD2).
#	 Thus, the DDREF = [(αD + βD2)/D]/(αD/D)=(αD + βD2)/αD, which 
#	 equals 1 + Dβ/α or 1 + D/(α/β)"
#	
# 	http://www.nap.edu/openbook.php?record_id=11340&page=44
#
# I will ignore issues of fractionation and dose rate for the time
# being.

	### Libraries
	library(plyr)
	library(ggplot2)
	library(survival)

	### Data
	setwd('~/janus/scripts')
	data <- readRDS('../data/external5.rds')

	### Helpers
	createSurvivalFrame <- function(f.survfit){
			
		# initialise frame variable
		f.frame <- NULL
		
		# create vector for strata identification
		f.strata <- NULL
		for(f.i in 1:length(f.survfit$strata)){
			# add vector for one strata according to number of rows of strata
			f.strata <- c(
				f.strata, 
				rep(names(f.survfit$strata)[f.i], 
				f.survfit$strata[f.i])
			)
		}
			
		# create data.frame with data from survfit 
		# (create column for strata)
		f.frame <- data.frame(
			time=f.survfit$time, 
			n.risk=f.survfit$n.risk, 
			n.event=f.survfit$n.event, 
			n.censor = f.survfit$n.censor, 
			surv=f.survfit$surv, 
			upper=f.survfit$upper, 
			lower=f.survfit$lower, 
			strata=factor(f.strata)
		)
	
		# create ﬁrst two rows (start at 1) for each strata
		for(f.i in 1:length(f.survfit$strata)){
			
			# take only subset for this strata from data
			f.subset <- subset(
				f.frame, 
				strata==names(f.survfit$strata)[f.i]
			)
			# create ﬁrst two rows (time: 0, time of ﬁrst event)
			f.start <- data.frame(
				time=c(0, f.subset$time[1]), 
				n.risk=rep(f.survfit[f.i]$n, 2), 
				n.event=c(0,0), 
				n.censor=c(0,0), 
				surv=c(1,1), 
				upper=c(1,1), 
				lower=c(1,1), 
				strata=rep(names(f.survfit$strata)[f.i],2)
			)
			# add ﬁrst two rows to dataset
			f.frame <- rbind(f.start, f.frame)
			# remove temporary data
			rm(f.start, f.subset)
		}
		# reorder data
		f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
		# rename row.names
		rownames(f.frame) <- NULL
	
		# return frame
		return(f.frame)
	}

	
	### Clean
	
	# reported in Roentgens
	s <- data$cluster %in% c(
		'1003-2-1', 
		'1003-20-1', 
		'1003-21-1', 
		'1003-22-1', 
		'1003-24-1', 
		'1003-27-1', 
		'1003-29-1', 
		'1002-1-1'
	)
	data$dose[s] <- data$dose[s] * 0.0094
	# Remove Clusters
	s <- c(
		'1003-30-1', 	# two treatments are tightly spaced
		'1003-26-1',		# doesn't fit description
		'2-10-1',		# doesn't fit description
		'2-11-1',		# doesn't fit description
		'3-1-1',		# doesn't fit description
		'1003-25-1', 	# lifespan
		'1003-5-1', 	# lifespan
		'1003-6-1', 	# lifespan
		'1003-7-1'		# lifespan
		
	)
	data <- data[!data$cluster %in% s,]
	# high LET / local
	data <- data[!data$quality %in% c(
		'accel. alpha local',
		'accel. alpha whole body',
		'accel. neutrons 0.1-10 MeV',
		'neutrons 1-10 MeV',
		'neutrons C-252',
		'neutrons fission',
		'neutrons>10 MeV',
		'X-rays local'
	),]
	# Impossible lifespans
	s <- data$cluster %in% c('11-2-16') & data$lifespan > 2000
	data <- data[!s,]



	### Group
	data$group <- paste(
		data$cluster,
		data$strain, 
		#data$dose_rate, 
		data$quality,
		#data$fractions,
		#data$fraction_interval,
		data$other_treatments
	)
	data <- ddply(head(data, nrow(data)), .(cluster), function(df){
		cat(length(unique(df$dose)), ' ')
		if(length(unique(df$dose)) < 2){ return(NULL) }
		controls <- df[df$dose == 0,]
		if(nrow(controls) < 1) { return(df) }
		for(group in unique(df$group)){
			controls$group <- group
			df <- rbind(df, controls)
		}
		df
	})
	data <- ddply(data, .(group), function(df){
		doses <- sum(!is.na(unique(df$dose)))
		cat(doses, df$group[1], '\n')
		if(doses < 3) { return(NULL) }
		df
	})
	
	
	### Show
	g <- data
	g <- g[!is.na(g$cluster),]
	g$n <- as.numeric(as.factor(g$group))
	g$name <- paste(g$cluster, g$n)
	ggplot(g, aes(dose, lifespan, group=factor(dose))) + 
		geom_boxplot(outlier.size = 0.5) +
		facet_wrap(~name, scales='free_y') + 
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		theme(strip.text.x = element_text(size = 6)) +
		xlim(0, 10) +
		stat_smooth(aes(group=1), method = "lm", formula=y~x+I(x^2))
	
	
	
	### Show survival

	# multiple strata
	t.Surv <- Surv(data$lifespan, rep(1, nrow(data)))
	t.survfit <- survfit(t.Surv~group + dose, data=data)
	t.survframe <- createSurvivalFrame(t.survfit)

	# fix names
	split <- strsplit(as.character(t.survframe$strata), 'dose=')
	t.survframe$dose <- as.numeric(laply(split, function(x) x[2]))
	t.survframe$group <- laply(split, function(x) x[1])
	t.survframe$group_name <- as.numeric(as.factor(t.survframe$group))
	t.survframe$strata <- as.numeric(as.factor(t.survframe$strata))

	# Plot
	ggplot(data=t.survframe, aes(color=dose, group=strata)) + 
		geom_step(
			aes(x=time, y=surv), 
			direction="hv"
		) + 
		facet_wrap(~group_name, scales='free_x')

	# Cumulative Hazards
	ggplot(data=t.survframe, aes(color=dose, group=strata)) + 
		geom_step(
			aes(x=time, y=-log(surv)), 
			direction="hv"
		) + 
		facet_wrap(~group_name, scales='free_x') +
		ylim(0, 4)
	
	# A key
	data$group_number <- as.numeric(as.factor(data$group))
	key <- ddply(data, .(group_number), function(df){
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
	groups <- unique(paste(data$group_number, data$group))
	
	
	# Cox Model
	r <- dlply(data, .(group), function(df){
		s <- Surv(df$lifespan, rep(1, nrow(df)))
		coxph(s ~ dose + I(dose^2), data=df)
	})
	r2 <- ldply(r, function(cox){
		s 		<- summary(cox)
		coef 	<- s$coefficients
		a 		<- coef[1,'coef']
		a.se 	<- coef[1,'se(coef)']
		B 		<- coef[2,'coef']
		B.se 	<- coef[2,'se(coef)']
		ddref_1Gy<-1 + B/a
		ddref_se<- ddref_1Gy * ((a.se/a)^2 + (B.se/B)^2)^0.5
		
		data.frame(
			coef, 
			term=rownames(coef), 
			ddref_1gy=ddref_1Gy,
			ddref_se=ddref_se,
			n=s$n
		)
	})
	
	# Show
	r2$group_name <- as.numeric(as.factor(r2$group))
	ggplot(r2, aes(group_name, exp.coef., color=term)) + 
		geom_point() + 
		geom_errorbar(aes(
			ymin=exp(coef-se.coef.), 
			ymax=exp(coef+se.coef.) 
		))

	ggplot(r2, aes(group_name, ddref_1gy, color=factor(group_name %% 5))) + 
		geom_point() + 
		geom_errorbar(aes(
			ymin=ddref_1gy-ddref_se, 
			ymax=ddref_1gy+ddref_se 
		)) +
		ylim(-5,7) + 
		geom_hline(aes(yintercept=1))
		
		