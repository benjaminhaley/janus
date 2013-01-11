# Marissa's paper
#
# An attempt to recreate Marissa's analysis in A Retrospective Analysis.
# I've added way more cross validation, but stuck generally to her datasets
# and approach.
#
# This is a cleaned up version of marissa.R
#
# bmh Oct 2011 - Oct 2012

# LIBRARIES
	# Load the janus data
	setwd('~/janus')
	source('scripts/data/load_janus_data.R')
	data <- j.data$load(from_cache=TRUE)
	
	# Some helper functions
	source('scripts/util/freq.R')
	source('scripts/util/odds_ratio.R')
	source("scripts/util/package.R")
	
	# Load the annotations
	source('scripts/data/ontology.R')
	c <- ontology$load_columns()
	#r <- ontology$load_rows(data)
	
	# Extra packages
	package$load(c("aod", "plyr", "ggplot2", "reshape", "gbm", "penalized"))
	
	
# CONFIGURATION
	refresh_cache <- TRUE
	set.seed(1)
	

# DATA
#
# Here we will look at jm-13 at all animals with necropsy dates
# in accordance with Marissa's orginal analysis.
#
# It is good that we are looking at a single experiments because
# it means that we do not have to concern ourselves with study
# to study variation.
#
# We will look at lethal and non-lethal pathologies simultaneously
# and look at all toxicities which occur more than 30 times in the
# data set.
#
# We will also look at the following groups of pathologies
# 
# Note: some of the grouped categories are sparsely populated
#       namely skeletal, nervous, male, and cardio

	groups <- list(
		cardio = c('MIC', 'MYO', 'PCD', 'THRT'),   # only has 7 members!
		female = c('CGL', 'MET', 'MGL', 'OVE', 'TCGL', 'TMGL', 'TOVE', 'TUTE', 
		           'TVAG', 'UTE', 'VAG', 'VOL'),
		integument = c('DER', 'TCON', 'TSKN'),
		lymph = c('NTYG', 'NTYL', 'TRD', 'TTRD', 'TTYG', 'TTYL'),
		male = c('PEN', 'PST', 'SEM', 'TEP', 'TEPI', 'TPST', 'TSMV', 'TTST'),
		nervous = c('BRN', 'CNS', 'PIT', 'TBRN', 'TCNS', 'TMIN', 'TPIT'),
		resp = c('EMB', 'EMP', 'HTX', 'MIL', 'PNC', 'PNU', 'PRF', 'TADN', 'TMIL'),
		skeletal = c('BON', 'TBON', 'TMUS'),
		vascular = c('ANE', 'ANU', 'HEM', 'HRG', 'THR', 'TVAS')
	)
	
	# And the following factors will be treated as factors	
	factors <- c('radn', 'sex', 'tmt')
	
	# And a minimum number of toxicities for significance
	min_toxicities <- 30
	
	# load data
	data <- data[data$expt == 13 & data$necroscopy_date != "",]
	data <- data[sample(1:nrow(data)),]
	
	# merge lethal and non_lethal results
	merged_macros <- ontology$merge_macros(data)
	data <- cbind(data[,!names(data) %in% c$MACROS_], merged_macros)
	
	# build grouped pathologies
	for(g in names(groups)){
		data[,g] <- rowSums(data[,groups[[g]]])
	}
	
	# build multi-organ system failtures
	data$multiple <- as.numeric(rowSums(data[,names(groups)] > 0) > 1)
	
	# Remove toxicities with less than 30 representatives
	common_toxicities <- freq$get_columns(data, c$MACROS, minimum_sum=min_toxicities)
	common_toxicities <- common_toxicities[!is.na(common_toxicities)]
	data <- data[,!names(data) %in% c$MACROS | names(data) %in% common_toxicities]
	for(f in factors){
		data[,f] <- factor(data[,f])
	}
	
	# Add an intercept column, always 1
	data$i <- 1
	
	# Change dose from cGy to Gy
	data$total_dose	<- data$total_dose / 100
	data$dose_rate  <- data$dose_rate / 100

	# Make age in years
	data$age	<- data$age / 365
	
	# List all toxicities 
	# and produce a long form table where each toxicity gets its own column
	all_toxicities = c(common_toxicities, names(groups), 'multiple')
	long <- melt(data, measure.vars=c(all_toxicities))
	long$p <- 0
	long$type <- 'toxicity'
	long$type[long$variable %in% names(groups)] <- 'grouped'
	
	

# HELPER FUNCTIONS
#	

	# caching
	library('digest')
	dir.create('cache', showWarnings = FALSE)
	memo <- function(fn, memo_dir='cache'){
		"Save results to disk when a function is called
		Return cached results when the function is called 
		with the same arguments"
		cached_fn <- function(..., rerun=FALSE){
			hash <- digest(list(...))
			file_path = paste(memo_dir, '/', hash, sep='')
			if(!file.exists(file_path) | rerun){
				result = fn(...)
				saveRDS(result, file_path)
			} else {
				print('using cache')
			}
			result <- readRDS(file_path)
			result
		}
		cached_fn
	}
	
	
	# Link functions
	logit <- function(p){-log(1/p - 1)}
	unlogit <- function(p){1 / (1 + exp(-p))}
	
	
	# Plotting functions
	plot_toxicity_incidence <- function(toxicity){
		# Useful troubleshooting plot
		ggplot(data[data[,toxicity] == 1,], aes(age, total_dose, color=sex)) + 
		geom_point() + 
		facet_wrap(~radn)
	}
	
	plot_predictions <- function(predictions, data){
		# Useful troubleshooting plot
		data$p <- predictions
		ggplot(data, aes(age, p, color=sex)) + geom_point() + scale_y_log10()
	}
	
	plot_toxicity <- function(title, type, radn, ymin, ymax, 
									 color='blue', se=TRUE, x='age', 
									 y='value', data=long){
		data <- data[data$type == type & data$radn == radn,]
		if(color %in% names(data)){data$color <- data[,color]} else {data$color <- color}
		data$y <- data[,y]
		data$x <- data[,x]
		base <- ggplot(data, aes(x, y, color=color, group=color))
		if(color == 'blue'){ base <- ggplot(data, aes(x, y)) }
		base + 
			geom_point(size=0.5, alpha=0.5) + 
			geom_smooth(size=0.5, alpha=0.5, se=se) + 
			facet_wrap(~ variable, nrow=3) + 
			ylim(ymin, ymax) +
			opts(title=title) +
			opts(axis.text.x=theme_text(angle=-90)) + 
			ylab("Incidence") + 
			xlab("Age at Death (days)") + 
		ggsave(paste(title, '.png', sep=''), width=11.3, height=6.71)
		title
	}
	plot_toxicity_cached <- memo(plot_toxicity)

	normalize_aggregates <- function(aggregate){
		ddply(aggregate, .(tox), function(a){
			a$mean = a$mean - a$mean[1]
			a
		})
	}

	plot_performance <- function(aggregate, title){
		aggregate <- normalize_aggregates(aggregate)
		# plot
		ggplot(aggregate, aes(f, mean)) + 
			geom_boxplot() + 
			opts(axis.text.x=theme_text(angle=-90)) +
			ylab('Relative Log Likelihood')
		ggsave(paste(title, '.png', sep=''), width=11.3, height=6.71)
		title
	}
	plot_performance_cached <- memo(plot_performance)
	


# TABLE 1
#
# Get counts of the number of animals in each treatment group

	table.1 <- ddply(data, .(radn, total_dose, dose_rate, fractions), function(df){
		c(n=nrow(df))
	})
	write.csv(table.1, 'treatments.csv')

# TABLE 2
#
# Get toxicity descriptions and counts
#

	table.2 <- 
		data.frame(
			symbol=all_toxicities,
			description=ontology$macro2janus_description(all_toxicities),
			n=laply(all_toxicities, function(t){sum(data[,t])})
		)
	write.csv(table.2, 'toxicities.csv')
	
# FIGURE 1
#
# Toxicity rates as a function of age
# 

	plot_toxicity_cached(
		title= 'Toxicity vs Age in Control Animals',
		type = 'toxicity',
		radn = 'C',
		ymin = 0,
		ymax = 1,
		rerun = refresh_cache
	)
	
	# Remove!	
	# plot_toxicity_cached(
		# title= 'Grouped Toxicity Counts vs Age in Control Animals',
		# type = 'grouped',
		# radn = 'C',
		# ymin = 0,
		# ymax = 1,
		rerun = refresh_cache
	# )	

# FIGURE 2
#
# Toxicity rates as a function of age and gender
#

	plot_toxicity_cached(
		title= 'Toxicity vs Age by Gender in Control Animals',
		type = 'toxicity',
		radn = 'C',
		ymin = 0,
		ymax = 1,
		color='sex', 
		se=FALSE,
		rerun = refresh_cache
	)
	
	# Remove!	
	# plot_toxicity_cached(
		# title= 'Grouped Toxicity Counts vs Age by Gender in Control Animals',
		# type = 'grouped',
		# radn = 'C',
		# ymin = 0,
		# ymax = 5,
		# color='sex',
		rerun = refresh_cache
	# )


# FIGURE 3
#
# Toxicity rates as a function of age and treatment
#		

if(refresh_cache){

	plot_toxicity_cached(
		title= 'Toxicity vs Age by Type of Radiation',
		type = 'toxicity',
		radn = c('C', 'N', 'G'),
		ymin = 0,
		ymax = 1,
		color='radn',
		se=FALSE,
		rerun = refresh_cache
	)
	
	# Remove!	
	# plot_toxicity_cached(
		# title= 'Grouped Toxicity Counts vs Age by Type of Radiation',
		# type = 'grouped',
		# radn = c('C', 'N', 'G'),
		# ymin = 0,
		# ymax = 5,
		# color='radn',
		rerun = refresh_cache
	# )
	
	plot_toxicity_cached(
		title= 'Toxicity vs Age by Treatment Group in Gamma Irradiated Animals',
		type = 'toxicity',
		radn = c('C', 'G'),
		ymin = 0,
		ymax = 1,
		color='total_dose',
		se=FALSE,
		rerun = refresh_cache
	)
	
	# remove!
	# plot_toxicity_cached(
		# title= 'Grouped Toxicity Counts vs Age by Treatment Group in Gamma Irradiated Animals',
		# type = 'grouped',
		# radn = c('C', 'G'),
		# ymin = 0,
		# ymax = 5,
		# color='tmt',
		# se=TRUE,
		# rerun = refresh_cache
	# )
	
	plot_toxicity_cached(
		title= 'Toxicity vs Age by Treatment Group in Neutron Irradiated Animals',
		type = 'toxicity',
		radn = c('C', 'N'),
		ymin = 0,
		ymax = 1,
		color='total_dose',
		se=FALSE,
		rerun = refresh_cache
	)
	
	# Remove!
	# plot_toxicity_cached(
		# title= 'Grouped Toxicity Counts vs Age by Treatment Group in Neutron Irradiated Animals',
		# type = 'grouped',
		# radn = c('C', 'N'),
		# ymin = 0,
		# ymax = 5,
		# color='tmt',
		# se=TRUE,
		# rerun = refresh_cache
	# )	
}


# MODEL SELECTOR
#
# This work follows a general re-usuable structure.  We build a 
# series of models, ask what works best following bootstrapping
# and then return the performance values associated with each
# modeling effort so that it can be selected.  The following functions
# will help to automate this process.

	bootstrap <- function(data, n=1, fn){
		"run a function over bootstrapped copies of data n times"
		ldply(1:n, function(i){
			d = data[sample(1:nrow(data), replace=TRUE),]
			result = fn(d)
			result$bootstrap <- i
			result
		})
	}

	bootstrap_list <- function(data, n=1, fn){
		"run a function over bootstrapped copies of data n times"
		llply(1:n, function(i){
			d = data[sample(1:nrow(data), replace=TRUE),]
			fn(d)
		})
	}

	cvl_wrap <- function(f, u, l1, l2, data, model='logistic', 
				         maxiter=25, fold=5, standardize=TRUE){
				         	
		cvl(
			response = as.formula(as.character(f)),
			unpenalized = as.formula(as.character(u)),
			lambda1=l1, 
			lambda2=l2, 
			model=model, 
			data=data, 
			maxiter=maxiter, 
			fold=fold, 
			standardize=standardize
		)		
	}
		
	aggregate_performance <- function(data){
		"Find average performance and standard deviation
		expects data of the form: tox, f, l1, l2, performance"
		numerics <- c('mean', 'sd', 'z')
		aggregate <- ddply(data, .(tox, f, l1, l2, u), function(df){
			o = df$performance
			c(mean=mean(o), sd=sd(o), z=mean(o)/sd(o))
		})
		for(n in numerics){aggregate[,n] <- as.numeric(aggregate[,n])}
		aggregate
	}

	select_best <- function(aggregate, by=tox){
		"Pick the best model from series of models
		expects data to include: mean, sd, and z scores"
		ddply(aggregate, by, function(df){
			df[df$mean == max(df$mean, na.rm=T),]
		})
	}
	
	sample_df <- function(df, n, replace=FALSE){
		df[sample(1:nrow(df)),][1:n,]
	}
	
	get_performances <- function(data, bootstraps, combinations, modeling_function){
		"Apply modeling function to each combination over each variable in the dataframe
		will produce an outcome with performance related to variable and combination"
		p <- ddply(data, .(variable), function(df){
			bootstrap(df, bootstraps, function(d){
				adply(combinations, .(1), function(c){modeling_function(c, d)})
			})	
		})
	}
	get_performances_cached <- memo(get_performances)
	
	get_coefficients <- function(data, combinations, modeling_function){
		"Apply modeling function to each combination over each variable in the dataframe
		will produce a list of dataframes for each combination and variable
		the dataframe will contain seperate estimates for each bootstrap"
		results <- dlply(data, .(variable), function(d){
				tox = d$variable[1]
				c <- combinations[combinations$tox == tox,]
				modeling_function(c, d)
		})
	}
	
	add_predictions <- function(data, combinations, prediction_function){
		adply(combinations, .(1), function(c){
			prediction_function(c)
		})
	}

	summarize_coefficient <- function(df, fn){
		m <- laply(df, fn)
		names(m) <- names(df)
		m <- m[!names(m) %in% 'bootstrap']
		m
	}

	model <- function(formulas, 
					  labmda1s=c(0), 
					  lambda2s=c(0.00000001), 
					  unpenalized=c('~0'),
					  data=long,
					  data_to_predict=long,
					  bootstraps=20, 
					  seed=1, 
					  modeling_function=function(c, d){cvl_wrap(c$f, c$u, c$l1, c$l2, d)}
					  ) {
		 
		# Controlled Randomization
		set.seed(seed)
			
		combinations <- expand.grid(
			f=formulas, 
			l1=labmda1s, 
			l2=lambda2s, 
			u=unpenalized, 
			stringsAsFactors=FALSE
		)
	
		# get performances
		performance <- get_performances_cached(data, bootstraps, combinations, function(c, d){modeling_function(c, d)$cvl}, rerun=refresh_cache)
				
		names(performance) <- c('tox', 'f', 'l1', 'l2', 'u', 'performance', 'bootstrap')	
		
		# Aggregate bootstraps
		aggregate <- aggregate_performance(performance)
	
		# Choose the best models
		baselines <- select_best(aggregate, by='tox')
	
		# Predict control data
		data <- add_predictions(data, baselines, function(c){
			df <- data[data$variable == c$tox,]
			df$p <- modeling_function(c, df)$predictions
			df
		})
		data <- data[,(!names(data) %in% names(baselines))]
	
		# Predict all data
		data_to_predict <- add_predictions(data, baselines, function(c){
			train <- data[data$variable == c$tox,]
			test <- data_to_predict[data_to_predict$variable == c$tox,]
			m <- modeling_function(c, train)
			test$p <- predict(m$fullfit, as.formula(c$f), as.formula(c$u), test)
			test
		})
		data_to_predict$control_prediction <- logit(data_to_predict$p)
		data_to_predict <- data_to_predict[,
							(!names(data_to_predict) %in% names(baselines))
						   ]

		
		# get coefficients
		coefficients <- get_coefficients(data, baselines, function(c, data){	
			f       <- as.formula(paste(c$f, '-', '1'))
			coef(summary(glm(formula = f, family='binomial', data=data)))
		})		
		
		
		# Results object
		results = list()	
		results$combinations <- combinations
		results$performance <- performance
		results$aggregate <- aggregate
		results$baselines <- baselines
		results$data <- data
		results$data_to_predict <- data_to_predict
		results$coefficients <- coefficients
		results
	}

	z <- function(values){mean(values)/sd(values)}
	
	
	get_significance_label <- function(p){
		# Return a '.' if p < 0.1, a '*' if p < 0.05...
		sig <- rep('   ', length(p))
		names(sig) <- names(p)
		sig[p < 0.1] <- '.  '
		sig[p < 0.05] <- '*  '
		sig[p < 0.005] <- '** '
		sig[p < 0.0005] <- '***'
		
		sig
	}
	
	get_odds_ratio <- function(coefficient, n=1){
		# Odds ratios given logistic regression coefficients
		# n corresponds to the number of units that are to change
		exp(coefficient * n)
	}
	
	coefficients_summary <- function(coefficients){
		
		# We are testing:
		bonferroni_correction <- length(coefficients) *	# each toxicity
								 2 * 					# each type of radiation
								 2   					# for dose and gender effects
		
		df <- rbind.fill(llply(coefficients, function(c){
			
			# transpose
			c <- t(c)
			
			# Add a term when gender is used
			if('total_dose' %in% names(c) & 'total_dose.sexM' %in% names(c)){
				c$total_dose_females<- c$total_dose
				c$total_dose_males	<- c$total_dose + c$total_dose.sexM
				c$gender_difference <- c$total_dose.sexM
				c$total_dose.sexM <- NULL
				c$total_dose <- NULL
			}
			
			
			means 	<- c['Estimate',]
			se 		<- c['Std. Error',]
			zscore 	<- c['z value',]
			pscore 	<- 2*pnorm(-abs(zscore)) * bonferroni_correction
			sig 	<- get_significance_label(pscore)
			high_or	<- get_odds_ratio(means + se*1.96)
			low_or	<- get_odds_ratio(means - se*1.96)
			terms   <- names(means)
			
			# gussie it up
			results <- sprintf("%.3f - %.3f %s", high_or, low_or, sig)
			
			results <- data.frame(llply(results, list), stringsAsFactors=FALSE)
			names(results) <- terms
			
			results
		}))
		
		df[is.na(df)] <- '-'
		df$term <- names(coefficients)
		
		df
	}



# BASELINE MODELS
#
# To estimate the effect of radiation exposure we must first
# estimate the baseline risk of each toxicity (or grouped toxicity) to 
# using animals in the control group.  Generally these models should
# account for the effects of age and gender, but there are many such
# models some of which will perform better for certain toxicities than
# others.
#
# For example, a toxicity which is very rare will betray little information
# about itself.  For this type of toxicity, a simple model will suffice, and
# in fact outperform more complex models which are prone to over-fitting
# scant evidence.
#
# To find a 'good' baseline model we will test several and measure their
# performances on a left out cross validation group.  We will rerun this 
# analysis multiple times using bootstrapped versions of the dataset in
# order to find the baseline model that performs best.
#
# Penalized Logitstic regression will be used for binary outcomes and 
# penalized poisson regression will be used for count outcomes.  Performance
# will be measured using the likelihood functions of predictions on the left-
# out data.

	# Control variables
	baseline_formulas <- c(
		'value ~ i',
		'value ~ i + age',
		'value ~ sex',
		'value ~ sex*age',
		'value ~ age + I(age^2)',
		'value ~ sex*age + sex*I(age^2)',
		'value ~ i + (age<850) + (age<1000) + (age<1150)',
		'value ~ sex*(age<850) + sex*(age<1000) + sex*(age<1150)'
	)
	
	# debugging
	long <- long[long$variable %in% c('DER', 'TOVE', 'TADN'),]
	#long <- head(long, 100)
	
	control_long <- long[
		long$radn == "C" & 
		long$type == 'toxicity'
	,]
	
	r <- model(formulas=baseline_formulas, data=control_long, data_to_predict=long, seed=2983)
	long <- r$data_to_predict
	

# FIGURE 4
#
# Here we see our ability to predict control data using models based
# on age and gender.  Performance is measured by bootstrapping on 
# cross validation sets.  
#
# We also see a cleaner predictive function relating age and outcomes
# than hte knots supplied by ggplot, giving us some extra confidence
# that that given models are sane ones.


	# Graph the performance of various models
	plot_performance_cached(
		r$aggregate,
		title='Relative Log Likelihood of Various Models Across All Toxicities',
		rerun=refresh_cache
	)
	
	# Show the chosen models
	r$baselines

	# Graph baseline predictions
	plot_toxicity_cached(
		title= 'Predicted Toxicity vs Age by Gender in Control Animals',
		type = 'toxicity',
		radn = 'C',
		ymin = 0,
		ymax = 1,
		color='sex',
		data=long,
		y='p',
		rerun = refresh_cache
	)

	# Show the coefficients of derived models
	table.3 <- coefficients_summary(r$coefficients)
	write.csv(table.3, 'control_coefficients.csv')
	
	# Save the models we chose
	write.csv(r$baselines, 'baseline_models.csv')

	
# GAMMA EXPERIMENTAL MODELS
#
# Now that we have decent baselines, its time to make models that
# add the effects of radiation.  We will consider several options

	# Control variables
	gamma_formulas <- c(
		'value ~ control_prediction',
		'value ~ control_prediction*total_dose - control_prediction - total_dose',
		'value ~ control_prediction*total_dose*sex - sex - total_dose - control_prediction'	)
		
	# Data
	gamma_long <- long[
		long$radn %in% c("C", "G") & 
		long$type == 'toxicity'
	,]
		
	# debugging
	#gamma_long <- gamma_long[gamma_long$variable %in% some_tox,]
	#gamma_long <- sample_df(gamma_long, nrow(gamma_long))

	r_gamma <- model(
		formulas=gamma_formulas, 
		data=gamma_long, 
		data_to_predict=gamma_long, 	
		modeling_function=function(c, d){
		
		# predict baselines on control data
		train 	<- d[d$radn == 'C',]
		c2     	<- r$baselines[r$baselines$tox == d[1,"variable"],]
		f       <- as.formula(paste(c2$f, '-', '1'))
		m 		<- glm(formula = f, family='binomial', data=train)
		d$control_prediction <- d$control_prediction <- predict(m, newdata=d)
		
		# return model 
		cvl_wrap(c$f, c$u, c$l1, c$l2, d)
	})
	
	gamma_long <- r_gamma$data_to_predict
	
# Figure 5
#
# Here we see predictions on experimental models using dose, sex,
# and the previous predictions.

	plot_toxicity_cached(
		title= 'Predicted Toxicity vs Age in Gamma Irradiated and Control Animals',
		type = 'toxicity',
		radn = c('C', 'G'),
		ymin = 0,
		ymax = 1,
		color='total_dose',
		data=gamma_long,
		se = FALSE,
		y='p',
		rerun = refresh_cache
	)
	
	r_gamma$aggregate
	
	# Which model perfomed the best?
	plot_performance_cached(
		r_gamma$aggregate,
		title='Relative Log Likelihood of Various Gamma Models Across All Toxicities',
		rerun=refresh_cache
	)
	
	# What our models predict
	table.4 <- coefficients_summary(r_gamma$coefficients)
	write.csv(table.4, 'gamma_coefficients.csv')	
	
	
# NEUTRON EXPERIMENTAL MODELS
#
# Now that we have decent baselines, its time to make models that
# add the effects of radiation.  We will consider several options

	# Control variables
	neutron_formulas <- c(
		'value ~ control_prediction',
		'value ~ control_prediction + total_dose',
		'value ~ control_prediction + total_dose*sex - sex'	)
		
	# Data
	neutron_long <- long[
		long$radn %in% c("N", "C") & 
		long$type == 'toxicity'
	,]
		
	# debugging
	# neutron_long <- neutron_long[neutron_long$variable %in% some_tox,]
	# neutron_long <- sample_df(neutron_long, nrow(neutron_long))

	r_neutron <- model(formulas=neutron_formulas, data=neutron_long, data_to_predict=neutron_long)
	neutron_long <- r_neutron$data_to_predict
	

	plot_toxicity_cached(
		title= 'Predicted Toxicity vs Age in Neutron Irradiated and Control Animals',
		type = 'toxicity',
		radn = c('N', 'C'),
		ymin = 0,
		ymax = 1,
		color='tmt',
		data=neutron_long,
		y='p',
		se=FALSE,
		rerun = refresh_cache
	)
	
# Figure 6
#
# As with figure 5, but on Neutron irradiated data
	
	# Which model perfomed the best?
	plot_performance_cached(
		r_neutron$aggregate,
		title='Relative Log Likelihood of Various Neutron Models Across All Toxicities',
		rerun=refresh_cache
	)
	
	table.5 <- coefficients_summary(r_neutron$coefficients)
	write.csv(table.5, 'neutron_coefficients.csv')	
	
	
# Inspect the data a bit

inspect_coefficients <- function(table){
	print('significant dose relationships')
	print(table[table$conclusions == 'dose',])
	
	print('')
	print('significant dose-sex relationships')
	print(table[table$conclusions == 'sex',])
}
inspect_coefficients(table.4)

. <- function(){
	sets 	<- 1:100
	data$cv <- sample(sets, nrow(data), replace=TRUE)
	for(i in sets){
		test 	= data[data$cv == i,]
		train 	= data[data$cv != i,]
		formula = ACI ~ sex*age + total_dose*radn - radn - total_dose
		model 	= glm(formula, family='binomial', data=train)
		predictions = predict(model, newdata=test)
		
		data$p[data$cv == i] = unlogit(predictions)
	}
	sum(log(data$p[data$ACI == 0]) + log((1 - data$p)[data$ACI == 1]))	
}


ggplot(data, aes(age, p, color=paste(radn, sex), group=paste(radn, sex))) +
			geom_point(size=0.5, alpha=0.5) + 
			geom_smooth(size=0.5, alpha=0.5, se=FALSE) + 
			opts(axis.text.x=theme_text(angle=-90)) + 
			ylab("Incidence") + 
			xlab("Age at Death (days)") + 
			scale_colour_brewer(palette="Set1")
