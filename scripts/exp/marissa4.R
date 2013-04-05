# Marissa's paper - Ovarian Tumors
#
# Hacked from marissa.3 to include ovarian micropathologies
#
# bmh 12 March 2013

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
	package$load(c("aod", "plyr", "ggplot2", "reshape2", "gbm", "penalized"))
	
	
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

	
	# And the following factors will be treated as factors	
	factors <- c('radn', 'sex', 'tmt')
	
	# And a minimum number of toxicities for significance
	min_toxicities <- 30
	
	# load data
	data <- data[data$expt == 13 & data$necroscopy_date != "",]
	data <- data[sample(1:nrow(data)),]
		
	# merge lethal and non_lethal results
	merged_macros <- ontology$merge_macros(data)
	data <- cbind(data, merged_macros)
		
	
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
	
	# Add data on low doses,
	# this cutoff is just above the first treatment category which is the biggest
	threshold_N <- 0.05
	threshold_G <- 1.0
	data$is_low_dose <- TRUE
	data$is_low_dose[(data$radn == 'N') & (data$total_dose > threshold_N)] <- FALSE
	data$is_low_dose[data$radn == 'G' & data$total_dose > threshold_G] <- FALSE
	table(data$radn, data$total_dose)
	table(data$radn, data$is_low_dose)

	# Define micro codes
	micro_toxicities <- c(
		#'TOAC',	# rare
		#'TOAO',	# rare
		#'TOCO',
		'TOGC',		# non-metastatic ganulosa
		#'TOPA',
		#'TOSC',
		'TOTO',		# non-metatstic leutoma
		'TOVO',		# non-metastatic hemangioma
		#'TOVS',	# not found
		#'TAWO',  	# not found
		#'TCWO',	# rare
		#'THWO',	# not found
		#'TIWO',	# not found
		#'TKWO',	# not found
		#'TLWO',	# not found
		#'TNWO',	# not found
		#'TRWO',	# rare
		#'TUWO',	# rare
		#'TXWO',	# not found
		#'TYWO',	# not found
		'TOTA'		# tubular adenoma ovary, not metastatic
	)
	
	# Add micro data
	# 
	url <- 'http://dl.dropbox.com/u/1467692/jm13_micros.csv'
	micros <- read.csv(url, as.is=T)
	micros <- micros[micros$micro_code %in% micro_toxicities,]
	all <- micro_toxicities
	empty <- data.frame(matrix(0, nrow=1, ncol=length(all)))
	names(empty) <- all
	temp <- ddply(micros, .(animal_id), function(df){
		output <- empty
		output[,df$micro_code] <- 1
		output
	})
	micros <- temp
	rownames(micros) <- micros$animal_id
	
	data <- merge(data, micros, by='row.names', all.x=T)
	data[,micro_toxicities][is.na(data[,micro_toxicities])] <- 0

# animal_id      TOAC      TOAO      TOCO      TOGC      TOPA 
  # 1217045         1         4         0        54         0 
     # TOSC      TOTO      TOVO      TOVS      TCWO      TRWO 
        # 0        49        47         8         1         2 
     # TUWO      TOTA 
        # 1       162 
# ~150 are non-metastatic tubular adenomas, TOTAs
# ~50 are non-metastatic hemangiomas, TOVO
# ~50 are non-metastatic leutoma, TOTO
# ~50 are non-metastatic ganulosa, TOGC

table(data$TOVE, data$TOTA) # 100/150 shared
table(data$TOVE, data$TOVO) # 40/50 shared
table(data$TOVE, data$TOTO) # 40/50 shared
table(data$TOVE, data$TOGC) # 40/50 shared

	# Make treatment names more sane
	data$tmt <- factor(paste(data$radn, data$total_dose, 'Gy'))
	
	# Replace common_toxicities with micro_toxicities
	common_toxicities <- c(
		micro_toxicities, 
		'TOVE', 
		'TOVE_L', 
		'TOVE_N'
	)

	
	# List all toxicities 
	# and produce a long form table where each toxicity gets its own column
	all_toxicities = c(common_toxicities)
	long <- melt(data, measure.vars=c(all_toxicities))
	long$p <- 0
	long$type <- 'toxicity'
	long$type[long$variable %in% names(groups)] <- 'grouped'
	long <- long[long$type == 'toxicity',]
	
	short <- head(long, 10000)


	
	

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
									 y='value', data=long, color_label='color', ...){
		data <- data[data$type == type & data$radn == radn,]
		if(color %in% names(data)){data$color <- data[,color]} else {data$color <- color}
		data$y <- data[,y]
		data$x <- data[,x]
		base <- ggplot(data, aes(x, y, color=color, group=color))
		if(color == 'blue'){ base <- ggplot(data, aes(x, y)) }
		base + 
			geom_point(size=0.5, alpha=0.5) + 
			facet_wrap(~ variable, nrow=3) + 
			ylim(ymin, ymax) +
			opts(title=title) +
			opts(axis.text.x=theme_text(angle=-90)) + 
			ylab("Incidence") + 
			xlab("Age at Death (years)") + 
			labs(color=color_label) +
			stat_smooth(size=0.5, alpha=0.5, se=se, ...) +
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
	write.csv(table.1, 'ovarian_treatments.csv')

# TABLE 2
#
# Get toxicity descriptions and counts
#

	table.2 <- ddply(long, .(variable), function(df){
		tox = as.character(df$variable[1])
		total = nrow(df)
		c(
			description=ontology$macro2janus_description(tox),
			n=sum(df$value),
			rate=round(sum(df$value)/total, 4),
			round(daply(df, .(tmt), function(d){sum(d$value)}) / total, 4)
		)
	})
	
	temp <- function(var, description, table){
		table[table$variable == var,'description.TOGC'] <- description
		table
	}
		
	table.2 <- temp('TOGC', 'non-metastatic ganulosa', table.2)
	table.2 <- temp('TOTO', 'non-metastatic leutoma', table.2)
	table.2 <- temp('TOVO', 'non-metastatic hemangiomas', table.2)
	table.2 <- temp('TOTA', 'non-metastatic tubular adenomas', table.2)

	write.csv(table.2, 'ovarian toxicities.csv')
	
# FIGURE 1
#
# Toxicity rates as a function of age
# 

	plot_toxicity_cached(
		title= 'Ovarian Toxicity vs Age in Control Animals',
		type = 'toxicity',
		radn = 'C',
		ymin = 0,
		ymax = 1,
		
		rerun = refresh_cache
	)

# FIGURE 2
#
# Toxicity rates as a function of age and gender
#

	plot_toxicity_cached(
		title= 'Ovarian Toxicity vs Age by Gender in Control Animals',
		type = 'toxicity',
		radn = 'C',
		ymin = 0,
		ymax = 1,
		color='sex', 
		se=TRUE,
		method='glm',
		formula='y ~ x + I(x^2)',
		color_label='Sex',
		rerun = refresh_cache
	)
	

# FIGURE 3
#
# Toxicity rates as a function of age and treatment
#		

if(refresh_cache){


	plot_toxicity_cached(
		title= 'Ovarian Toxicity vs Age by Type of Radiation',
		type = 'toxicity',
		radn = c('C', 'N', 'G'),
		ymin = 0,
		ymax = 1,
		color='radn', 
		se=TRUE,
		method='glm',
		formula='y ~ x + I(x^2)',
		color_label='Radiation Quality',
		rerun = refresh_cache
	)
	



# Helper Functions
#
expected_false_positives  <- function(n, threshold=0.05){n * threshold}
get_significant_results <- function(p_values, threshold=0.05){p_values < threshold}

bonferroni_significances <- function(p_values, n=length(p_values), threshold=0.05){
	# Determines if values are significant after applying the bonferroni correction
	p_values < threshold / n
}

fp_vs_tp <- function(p_values, thresholds=c(10:1/20, 10:1/200, 10:1/2000)){
	# Calculate expected false positives vs expected true positives over the threshold range
	ldply(thresholds, function(t){
		fp = expected_false_positives(length(p_values), t)
		sig = sum(get_significant_results(p_values, t))
		c(
			p=t,
			false_positives=round(fp, 1),
			true_positives=round(sig - fp, 1)
		)
	})
}


# Radiation responses
#
# Which individual toxicities does radiation moderate?

	# Build models
	tmt.test <- function(df){
		# build a model
		f <- 'value ~ sex*age + sex*I(age^2) +
	    	  tmt'
		m <- glm(f, family='binomial', data=df)
		
		# output memory efficient summary statistics
		coefficients <- data.frame(coef(summary(m)))
	
		# odds ratios
		coefficients$odds.ratio <- exp(coefficients$Estimate)
		coefficients$name       <- rownames(coefficients)
		of_interest <- grepl('^tmt', rownames(coefficients))
		coefficients[of_interest,]
	}

	tmt.specific <- ddply(long, .(variable), tmt.test)

	
	# Find significant results
	#
	# Definately signficant are those which are significantly true after
	# a bonferroni correction for the number of tests being performed.
	#
	# Probably significant are those results which have a p value
	# greater than a threshold (below) determined by our false positive
	# test to result in mostly true positive results

	fp <- fp_vs_tp(tmt.specific$Pr...z..)
	
	good_cutoff <- 0.002  # determined from fp
 
	tmt.specific$definitely.sig <- bonferroni_significances(tmt.specific$Pr...z..)
	tmt.specific$probably.sig <- get_significant_results(tmt.specific$Pr...z.., good_cutoff)

	
	# Look at the data now
	fp
	tmt.specific[tmt.specific$definitely.sig,]
	tmt.specific[tmt.specific$probably.sig,]
	
	# Write the data to file
	write.csv(fp, "Ovarian False Positives in treatment test")
	write.csv(tmt.specific, "Ovarian Treatment test")
	
	#
	# LINEAR VERSION
	#
	# I want to try this same analysis but with a linear model
	
	# Build models
	radn.test <- function(df){
		# build a model
		f <- 'value ~ sex*age + sex*I(age^2) +
	    	  total_dose*radn - total_dose - radn'
		m <- glm(f, family='binomial', data=df)
		
		# output memory efficient summary statistics
		coefficients <- data.frame(coef(summary(m)))
	
		# odds ratios
		coefficients$odds.ratio <- exp(coefficients$Estimate)
		coefficients$name       <- rownames(coefficients)
		of_interest <- grepl('total_dose.radn.', rownames(coefficients))
		coefficients[of_interest,]
	}

	radn.specific <- ddply(long, .(variable), radn.test)

	
	# Find significant results
	#
	# (as above)

	fp <- fp_vs_tp(radn.specific$Pr...z..)
	
	good_cutoff <- 0.002  # determined from fp
 
	radn.specific$definitely.sig <- bonferroni_significances(radn.specific$Pr...z..)
	radn.specific$probably.sig <- get_significant_results(radn.specific$Pr...z.., good_cutoff)

	
	# Look at the data now
	fp
	radn.specific[radn.specific$definitely.sig,]
	radn.specific[radn.specific$probably.sig,]
	
	# Write the data to file
	write.csv(fp, "Ovarian False Positives in radiation test")
	write.csv(radn.specific, "Ovarian Radiation test")




# Dose Modulation
#
# Do low doses change response rates

	dose.test <- function(df){
		# build a model
		#f <- 'value ~ sex*(age<850/365) + sex*(age<1000/365) + sex*(age<1150/365) +
	    #	  tmt*sex'
		f <- 'value ~ sex*age + sex*I(age^2) +
	    	  total_dose*radn*is_low_dose -
	    	  total_dose*is_low_dose - radn*is_low_dose'
		m <- glm(f, family='binomial', data=df)
		
		# output memory efficient summary statistics
		coefficients <- data.frame(coef(summary(m)))
	
		# odds ratios
		coefficients$odds.ratio <- exp(coefficients$Estimate)
		coefficients$name       <- rownames(coefficients)
		of_interest <- grepl('total_dose.radn..is_low_doseTRUE', rownames(coefficients))
		coefficients[of_interest,]
	}
	
	dose.specific <- ddply(long, .(variable), dose.test)
	
	# Interpretation
	#
	# This one is quite interesting.  TOVE seems to be hypersensitive to even low
	# doses of radiation so that 1 Gy has nearly the same effect as higher doses.
	# This effect is significant even beyond the bonferroni correction, which is
	# pretty interesting.

	fp <- fp_vs_tp(dose.specific$Pr...z..)
	
	dose.specific$definitely.sig <- bonferroni_significances(dose.specific$Pr...z..)

	
	# Look at the data now
	fp
	dose.specific[dose.specific$definitely.sig,]
	
	# Write the data to file
	write.csv(fp, "Ovarian False Positives in dose test")
	write.csv(dose.specific, "ovarian dose test")

