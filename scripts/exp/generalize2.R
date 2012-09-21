##############################################################################
#
# Generalizability of Radiation Lifespan Models
# Adapted from generalize.R
# bmh June 2012

# Overview

  # Determine how well lifespan prediction modeling techniques perform on 
  # Janus experiments for presentation to my comittee.  Address issues raised
  # in generalize.R and eliminate between species analysis.
  
# Issues from generalize.R

  # 1. Recode irradiation as assigned dose, rather than dose recieved 
  #    so that it does not covary with lifespan.
  # 2. Apply additional models, at least nnet and hopefully ensemble these.
  # 3. Make outcome lifespan relative to controls post treatment assignment.
  # 4. Form better graphs of input data and predictions.

##############################################################################


### configuration ############################################################
#
# load packages
#

	setwd('~/janus')						# All includes rely on this
	source('scripts/data/data.R')			# loads janus and beagle data
	source("scripts/util/package.R")		# load and install libraries
	source("scripts/util/select.R")		    # filter data frames
	source("scripts/util/cv.R")             # build test and validation sets
	source("scripts/util/perform.R")        # How to calculate common performance functions
	source("scripts/util/f.builder.R")      # Help us compose complex formula
	
	package$load(c(
		"ggplot2", 							# graphing
		"plyr", 						  	# grouping
		"gbm",								# kickin' ass
		"caret"                             # one ring to rule them all
	))
	
	#
	# define acceptable data
	#     column.name=c(acceptable row values),
	#
	
	acceptable <- list(                        # 10683 excluded total
	    cause_of_death=c(       
			"Died", "Sacrifice, moribund"      # only natural deaths (6776 excluded)
		),
	
		experiment=c(
			"2", "3", "4", "7", "8", "9"
			#, "10"                            # Exclude peromyscus (2390)
			, "11", "12", "13"
			#, "14"                            # Exclude amiofostine treated animals (4000)
		),
		species=c(                             # 723 are labled blank
			"musculus"                         #, "peromyscus"
		),
		sex=c("M", "F")                        # Remove the 25 un-gendered animals 
	                                           # because they mess up function calls
	)
	
	# 
	# Column types
	#
	factors <- c( 
		"cause_of_death", "sex", "species", "experiment", "group"
	) 


### preprocessing ############################################################
#
	data <- data$load(from_cache=TRUE)             # load janus data
	                           
	data <- select$by_acceptable(data, acceptable) # Remove unacceptable animals
	                                               
	for(f in factors){                             # Convert data types where needed 
		data[[f]] <- factor(data[[f]])
	}

	# Control and dose summary
	data$sieverts <- data$cgy_total_gamma + data$cgy_total_neutron
	data$is_control <- data$sieverts == 0
	data$quality    <- 'C'
	data$quality[data$cgy_total_gamma   > 0] <- 'G'
	data$quality[data$cgy_total_neutron > 0] <- 'N'

	# Create a new outcome
	#
	# Relative age will be the the age of an animal minus the mean age
	# of control animals from the same experiment that lived at least
	# until the age of first treatment (assigned age)
	#
	# Notably almost all control animals lived until at least the age
	# of first_treatment
	#
	data <- ddply(data, .(experiment, sex), function(df){
		
		# find rolling mean
		ages            <- sort(df[df$is_control, c("age_days")], decreasing=TRUE)
		rolling_mean    <- cumsum(ages) / seq_along(ages)
		
		# Match to age at first exposure
		df              <- df[order(df$first_exposure_age_days),]
		ages            <- sort(ages)
		rolling_mean    <- sort(rolling_mean)
		df$mean_age_if_alive <- rolling_mean[findInterval(df$first_exposure_age_days, ages) + 1]

		df
	})
	data$relative_age <- data$age_days - data$mean_age_if_alive
	
	# Find lifetime assignments
	treatment_count_by_group <- daply(data, .(group), function(df){
		length(unique(df$cgy_total_gamma)) + length(unique(df$cgy_total_neutron))
	})
	lifelong_groups <- names(treatment_count_by_group[treatment_count_by_group > 4])
	data$until_death                                  <- FALSE
	data$until_death[data$group %in% lifelong_groups] <- TRUE
	
	# Determine assigned treatment (rather than delivered treatment)
	t_factors <- c("cgy_total_gamma", "cgy_total_neutron", 
	               "fractions_gamma", "fractions_neutron")
	for(t in t_factors){
		n <- paste(t, 'assigned', sep='_'); 
		data[,n] <- data[t]
		lifers <- data$group %in% lifelong_groups
		data[lifers,n] <- median(data[lifers,t])
	}
			
	# A nice working subset
	small <- function(n=1000, d=data) { set.seed(1); d[sample(nrow(d), replace=T), ][1:n,] }

	# Some convienences
	data$cgy_per_min <- data$cgy_per_min_gamma + data$cgy_per_min_neutron
	data$cgy_total   <- data$cgy_total_gamma + data$cgy_total_neutron
	data$cgy_total_assigned   <- data$cgy_total_gamma_assigned + data$cgy_total_neutron_assigned
	data$fractions_assigned   <- data$fractions_gamma_assigned + data$fractions_neutron_assigned

### data summary ###########################################################

	# There are 36446 mice in 8 studies with 76 treatment groups.
	no <- function(v){length(unique(v))}
	nrow(data); no(data$experiment); no(data$group)

	# Treatment groups recieved varying total doses, dose rates, and number 
	# of fractions, of Gamma or Neutron irradiation.  Here we present a 
	# random treatment group.
	
	# You can see that...
	
	# These studies varied treatment by total dose, dose rate, fractionation, and type
	# of radaition.  A infogram of these 

	group_summary <- ddply(data, .(experiment, group), function(df){
		data.frame(
			n=nrow(df), 
			males=sum(df$sex == "M"), 
			females=sum(df$sex == "F"), 
			age_days=round(mean(df$age_days)), 
			#mean_age_if_alive=mean(df$mean_age_if_alive), 
			relative_age=round(mean(df$relative_age)),
			first_exposure_age_days=round(mean(df$first_exposure_age_days)), 
			#control=all(df$is_control), 
			quality=df$quality,		
			cgy_total=round(mean(df$cgy_total_gamma) + mean(df$cgy_total_neutron)),  
			cgy_per_min=round(mean(df$cgy_per_min_gamma) + mean(df$cgy_per_min_neutron), 2),  
			fractions=round(mean(df$fractions_gamma) + mean(df$fractions_neutron)), 
			until_death=all(df$until_death)#, 
			#cgy_total_gamma_assigned=mean(df$cgy_total_gamma_assigned), 
			#cgy_total_neutron_assigned=mean(df$cgy_total_neutron_assigned), 
			#fractions_gamma_assigned=mean(df$fractions_gamma_assigned), 
			#fractions_neutron_assigned=mean(df$fractions_neutron_assigned)
			)})
	
	group_summary <- unique(group_summary)
			
	set.seed(2); group_summary[sample.int(nrow(group_summary), 1),]
	
	write.csv(group_summary, 'summary.csv')
		
	# Study factors
	ggplot(small(), aes(x = cgy_total, y = cgy_per_min, shape=sex, color=quality)) +
	geom_point(alpha=0.5) + facet_grid(experiment ~ .) + 
	scale_y_continuous(breaks=c(0,30)) 
	
	# Against lifespan
	ggplot(small(), aes(cgy_total, age_days, color=quality)) +
	geom_point(alpha=0.1, size=1) + geom_smooth()
	
	# Against remaining
	ggplot(small(), aes(cgy_total_gamma + cgy_total_neutron, relative_age, color=quality)) + 
	geom_point(alpha=0.5, size=1) + geom_smooth()

# Cox Functions
#
# Hazards
#
#     I will start by building the simplest model that I can and using it to 
#     derive the baseline hazard function.  This will be useful for all 
#     subsequent models.

	hazard <- basehaz(coxph(Surv(data$age_days) ~ 1, data = data))
	ggplot(hazard) + geom_point(aes(time, hazard*3), color="red") + geom_point(aes(time, hazard)) + scale_y_continuous('cumulative hazard rate')

# Visualize for example
# 



# My Hazards
#
#     The rates I found are dubious.  When I derive it on my own, it seems clear
#     that they were showing the cumulative hazard rate.  

	hazards_table <- function(timings) {
		haz_table         <- count(timings)
		names(haz_table)  <- c("time", "events")
		
		# Add in the zero time point if it is not already there
		if(min(haz_table$time) > 0) { haz_table <- rbind(c(0,0), haz_table)}
		
		surviving         <- sum(haz_table$events) - cumsum(haz_table$events)
		haz_table$dt      <- c(haz_table$time[2:nrow(haz_table)] - 
		                       haz_table$time[1:nrow(haz_table) - 1], 1e100)
		haz_table$hazard  <- haz_table$events / (haz_table$events + surviving)
		                   
		haz_table
	}
	
	my.hazard <- hazards_table(data$age_days)
	
	ggplot(my.hazard[1:2000,]) + geom_point(aes(time, hazard), size=0.5, alpha=0.5)
		

# One thing to note here is that dt is not constant,
# so the hazard rate is defined over different time spans.
# This is nice for the math, but perhaps decieving at 
# first glance.


# Median Lifespan
#
#     Lets try using cox to find the median lifespans

	haz.median         <- function(time, hazard) {
		                      n        <- length(time)
		                      dt       <- c(time[2:n] - time[1:n - 1], 1e100)
                              survival <- 1 - cumprod(1 - hazard)
                              median   <- time[which.min(abs(survival - 0.5))]
                                                            
                              median                              
                          }
    
    haz.median(time=c(0,2,5), hazard=c(1/4, 1/3, 1)) == 2
    
	haz.median(my.hazard$time, my.hazard$hazard); median(data$age_days)

# Mean lifespan
#
#   This seems to work poorly
#
	# haz.mean           <- function(time, hazard) {
		                      # n        <- length(time)
		                      # dt       <- c(time[2:n] - time[1:n - 1], 1e100)
                              # survival <- cumprod(1 - hazard)
                              # mean     <- sum((dt * survival)[1:length(dt) - 1])
                              
                              # mean
                          # }
                          
    # haz.mean(time=c(0,2,5), hazard=c(1/4, 1/3, 1)) == (1/4)*0 + (1/4)*2 + (1/2)*5 
    # haz.mean(time=c(0,2,5), hazard=c(1/4, 1/3, 1)*) 
    # == (1/4)*0 + (1/4)*2 + (1/2)*5 
    
    # haz.mean(my.hazard$time, my.hazard$hazard); mean(data$age_days)
		

#### Model data ####
	n = nrow(data)
	cvs = 20

	subset     <- small(n)                                            # working data
	X_factors  <- c(
		'sex', 
		'first_exposure_age_days', 
		'until_death', 
		'cgy_total_gamma_assigned', 
		'cgy_total_neutron_assigned', 
		'fractions_gamma_assigned', 
		'fractions_neutron_assigned',
		'cgy_per_min_gamma', 
		'cgy_per_min_neutron'
	)
	X          <- subset[,X_factors]                                  # modeling factors
	X          <- data.frame(llply(X, as.numeric))                    # make numeric
	y          <- subset$relative_age                                 # dependent vars
	y01        <- (y + 1000)/2000                                     # scale y for nnet


#### Validation strategy ####
	tmp        <- createDataPartition(y, p=0.8, times=cvs)            # cv by observation
	by_obs     <- trainControl(method = "LGOCV", index = tmp)
	grp        <- subset$group                                        # cv by group
	r_grp      <- sample(unique(grp))
	n_grp      <- length(r_grp)
	p_grp      <- split(r_grp, ceiling(seq_along(r_grp)/(n_grp / cvs)))
	tmp        <- llply(p_grp, function(p){which(!grp %in% p)})
	names(tmp) <- paste('Resample', 1:cvs, sep='')
	by_grp     <- trainControl(method = "LGOCV", index = tmp ) 
	exp        <- subset$experiment                                   # cv by experiment
	tmp        <- llply(unique(exp), function(e){which(exp != e)})
	names(tmp) <- paste('Resample', 1:length(unique(exp)), sep='')
	by_exp     <- trainControl(method = "LGOCV", index = tmp )               


#### Define custom models ####
	# cox
	cox_custom = list(
	   parameters = data.frame(.x=1)   # tuning parameters
	  ,model = function(data, weights, parameters, levels, last, ...){
	  	i <- c(
	  		"cgy_total_gamma_assigned * sex",
	  		"I(cgy_total_gamma_assigned^2)",
	  		"I(cgy_total_gamma_assigned^2) * sex",
	  		"cgy_total_neutron_assigned * sex",
	  		"I(cgy_total_neutron_assigned^2)",
	  		"I(cgy_total_neutron_assigned^2) * sex",
	  		"cgy_per_min_gamma * sex",
	  		"cgy_per_min_neutron * sex",
	  		"cgy_per_min_gamma * cgy_total_gamma_assigned",
	  		"cgy_per_min_gamma * cgy_total_gamma_assigned * sex",
	  		"cgy_per_min_neutron * cgy_total_neutron_assigned",
	  		"cgy_per_min_neutron * cgy_total_neutron_assigned * sex",

	  		"until_death * cgy_total_gamma_assigned * sex",
	  		"until_death * I(cgy_total_gamma_assigned^2)",
	  		"until_death * I(cgy_total_gamma_assigned^2) * sex",
	  		"until_death * cgy_total_neutron_assigned * sex",
	  		"until_death * I(cgy_total_neutron_assigned^2)",
	  		"until_death * I(cgy_total_neutron_assigned^2) * sex",
	  		"until_death * cgy_per_min_gamma * sex",
	  		"until_death * cgy_per_min_neutron * sex",
	  		"until_death * cgy_per_min_gamma * cgy_total_gamma_assigned",
	  		"until_death * cgy_per_min_gamma * cgy_total_gamma_assigned * sex",
	  		"until_death * cgy_per_min_neutron * cgy_total_neutron_assigned",
	  		"until_death * cgy_per_min_neutron * cgy_total_neutron_assigned * sex"
	  		)
	  	f <- c(i, X_factors)
	  	formula      <- f.builder$get_formula("Surv(data$.outcome)", f)
		list(coxph(formula(formula), data))
	  }
	  ,prediction = function(object, newdata){
	  	h <- hazards_table(y)
	  	model = object[[1]]
	  	aaply(predict(model, newdata), 1, function(x){
	  		haz.median(h$time, h$hazard * exp(x))
	  	})
	  }
	  ,probability = NULL
	  ,sort = function(df){df}
	)
	
	cox_by_exp   <- by_exp
	cox_by_grp   <- by_grp
	cox_by_obs   <- by_obs
	cox_by_exp$custom <- cox_custom
	cox_by_grp$custom <- cox_custom
	cox_by_obs$custom <- cox_custom

                 
#### Build Models ####
	models_by_exp <- list(
		 gbm=train(X, y, method="gbm", trControl=by_exp)
		,nnet=train(X, y01, "nnet", trControl = by_exp, preProcess = c("center", "scale"))
		,cox=train(X, y, "custom", trControl = cox_by_exp)
	)
	models_by_grp <- list(
		 gbm=train(X, y, method="gbm", trControl=by_grp)
		,nnet=train(X, y01, "nnet", trControl = by_grp, preProcess = c("center", "scale"))
		,cox=train(X, y, "custom", trControl = cox_by_grp)
	)
	models_by_obs <- list(
		 gbm=train(X, y, "gbm", trControl = by_obs)
		,nnet=train(X, y01, "nnet", trControl = by_obs, preProcess = c("center", "scale"))
		,cox=train(X, y, "custom", trControl = cox_by_obs)
	)
	
	#summary(models_by_exp$gbm$finalModel)
	#plot(nnet_by_exp)
	

#### Get Predictions ####
	get_pre <- function(m, data=X){ 
		p <- data.frame(predict(m, data))                             # predict
		p$nnet <- p$nnet * 2000 - 1000                                # adjust nnet
		p 
	}
	p_by_obs <- get_pre(models_by_obs)
	p_by_grp <- get_pre(models_by_grp)
	p_by_exp <- get_pre(models_by_exp)
	t_by_obs <- get_pre(models_by_obs, testX)
	t_by_grp <- get_pre(models_by_grp, testX)
	t_by_exp <- get_pre(models_by_exp, testX)
	
	
#### Evaluate Models ####
	perform_graph <- function(m){bwplot(resamples(m), metric = "Rsquared")}
	perform_graph(models_by_obs)
	perform_graph(models_by_grp)
	perform_graph(models_by_exp)
		
	model_significance <- function(m){ summary(diff(resamples(m))); }
	model_significance(models_by_obs)
	model_significance(models_by_grp)
	model_significance(models_by_exp)
		
	max_r2 <- function(m){
		r <- m$results
		i <- which.max(r$Rsquared)
		c(r2=r$Rsquared[i], r2_SD=r$RsquaredSD[i])
	}
	
#### Save model performance over time ####
	p_by_input <- readRDS("performance_by_input.RDS")
	
	temp <- data.frame(rbind(
			cbind(ldply(models_by_exp, max_r2), by='exp'),
			cbind(ldply(models_by_grp, max_r2), by='grp'),
			cbind(ldply(models_by_obs, max_r2), by='obs')
			))
	p_by_input <- unique(rbind(p_by_input, data.frame(
		n.obs=nrow(X), 
		n.exp=length(unique(subset$experiment)), 
		n.grp=length(unique(subset$group)), 
		temp
		)))
	saveRDS(p_by_input, "performance_by_input.RDS")

#### Test Points ####
	# Gamma test set
	n <- 20
	max_dose <- 800
	delta <- max_dose / n
	testG <- matrix(rep(c(
		2, 120, 0, 0,  0, 60,  0,  .1,  0,
		1, 120, 0, 0,  0, 60,  0,  .1,  0,
		2, 120, 0, 0,  0,  1,  0,  .1,  0,
		1, 120, 0, 0,  0,  1,  0,  .1,  0,
		2, 120, 0, 0,  0, 60,  0, 10,  0,
		1, 120, 0, 0,  0, 60,  0, 10,  0,
		2, 120, 0, 0,  0,  1,  0, 10,  0,
		1, 120, 0, 0,  0,  1,  0, 10,  0), n),
		dimnames = list(c(),X_factors),
		ncol = 9,
		byrow = TRUE
	)
	testG[,'cgy_total_gamma_assigned'] <- sort(rep((1:(max_dose/delta))*delta, nrow(testG) / n))
	
	# Neutron test set
	n <- 20
	max_dose <- 160
	delta <- max_dose / n
	testN <- matrix(rep(c(
		2, 120, 0,  0, 30,  0, 60,  0, .01,       # Neutrons
		1, 120, 0,  0, 30,  0, 60,  0, .01,
		2, 120, 0,  0, 30,  0,  1,  0, .01,       # less fractions
		1, 120, 0,  0, 30,  0,  1,  0, .01,
		2, 120, 0,  0, 30,  0, 60,  0,  1,       # higher rate
		1, 120, 0,  0, 30,  0, 60,  0,  1,
		2, 120, 0,  0, 30,  0,  1,  0,  1,
		1, 120, 0,  0, 30,  0,  1,  0,  1), n),
		dimnames = list(c(),X_factors),
		ncol = 9,
		byrow = TRUE
	)
	testN[,'cgy_total_neutron_assigned'] <- sort(rep((1:(max_dose/delta))*delta, nrow(testN) / n))
	
	# Full test set
	testX <- rbind(testN, testG)
	

#### Get Represenative Data ####

	# Useful to find ranges of representative data.
	
	# cluster_on <- c(
		# "cgy_total_gamma_assigned", 
		# "cgy_total_neutron_assigned", 
		# "fractions_gamma_assigned", 	
		# "fractions_neutron_assigned", 
		# "cgy_per_min_gamma", 
		# "cgy_per_min_neutron", 	
		# "first_exposure_age_days"
	# )
	# get_rep <- function(p, n=10, by=cluster_on){
		# d <- cbind(p, subset)#[! duplicated(X),]                       # only unique outcomes
		# f_age <- d$first_exposure_age_days
		# d <- d[105 < f_age & f_age < 125,]
		# reps <- d[!duplicated(kmeans(scale(d[,to_cluster]), n)$cluster),]
	# }


#### Plot test points ####

	test_to_full <- function(X=testX){
		X <- data.frame(X)
		X$quality <- 'C'
		X$quality[X$cgy_total_gamma_assigned > 0] <- 'G'
		X$quality[X$cgy_total_neutron_assigned > 0] <- 'N'
		X$cgy_total_assigned <- X$cgy_total_gamma_assigned + X$cgy_total_neutron_assigned
		X$cgy_per_min <- X$cgy_per_min_gamma + X$cgy_per_min_neutron
		X$fractions_assigned <- X$fractions_neutron_assigned + X$fractions_gamma_assigned
		
		X
	}
	get_treatment_names <- function(reps){
		#reps <- reps[order(reps$quality, reps$cgy_total_assigned),]
		q    <- reps$quality
		sex  <- c('M', 'F')[reps$sex]
		tot  <- reps$cgy_total_assigned
		rate <- reps$cgy_per_min
		frac <- reps$fractions_assigned
		names <- paste(
			q,'-',
			sex,'-',
			#format(tot, digits=1),'cGy','-',
			format(rate, digits=1),'/min','-',
			format(frac),'fr.'
		)
		factor(names, levels=rev(unique(names)))
	}

	model_names <- c(
		"cox"
		,"gbm" 
		#,"glm" 
		,"nnet"
	)

	plot_reps <- function(reps, m=model_names){
		long = reshape(reps, varying=m, direction="long", v.names='p', times=m, timevar="model")
		long <- long[long$sex != 1,]
		ggplot(long, aes(cgy_total_assigned, p, color=model)) + 
		geom_point(alpha=0.5, size=1) +
		geom_path() +
		facet_wrap(~ treatment, scales = "free")
	}
	
	# Test points
	reps_by_obs <- cbind(test_to_full(), t_by_obs)
	reps_by_grp <- cbind(test_to_full(), t_by_grp)
	reps_by_exp <- cbind(test_to_full(), t_by_exp)
	reps_by_obs$treatment <- get_treatment_names(reps_by_obs)
	reps_by_grp$treatment <- get_treatment_names(reps_by_grp)
	reps_by_exp$treatment <- get_treatment_names(reps_by_exp)
	plot_reps(reps_by_obs)
	plot_reps(reps_by_grp)
	plot_reps(reps_by_exp)
	
	# A random representative sample
	set.seed(1); 
	plot_reps(reps_by_obs[reps_by_obs$treatment == "G - F -  0.10 /min -  1 fr.",])
	
	ggplot(
		data=p_by_input, 
		aes(n.obs, r2, color=.id)
	) + 
	geom_errorbar(aes(ymin=r2-r2_SD, ymax=r2+r2_SD), alpha=0.1) + 
	geom_point() + facet_wrap(~ by) + geom_path()

	# Which experiments went wrong
	data.frame(r2=round(models_by_exp$nnet$resample$Rsquared, 3), exp=unique(exp))
	
	# Show the principal of cross validation
	n = 100
	d <- data.frame(
		time=1:n, 
		fun=(1:n)^runif(n), 
		set=rep(c('train', 'validate'), 50)
	) 
	g <- ggplot(d, aes(time, fun)) + geom_point();
	g2 <- ggplot(d, aes(time, fun, color=set)) + geom_point();
	
	g
	g + geom_path(color="blue")
	g + geom_smooth(method="lm", se=FALSE)
	g2 + geom_path()
	g2 + geom_smooth(method="lm", se=FALSE)