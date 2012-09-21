# An attempt to recreate Marissa's analysis in A Retrospective Analysis...
# should exercise the new janus system
# bmh Oct 2011 - Aug 2012

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

# DATA
#
# Here we will look at jm-13 at all animals with necropsy dates
# in accordance with Marissa's orginal analysis.
#
# It is good that we are looking at a single experiments because
# it means that we do not have to concern ourselves with study
# to study variation.

data <- data[data$expt == 13 & data$necroscopy_date != "",]
data <- data[sample(1:nrow(data)),]

# HELPER FUNCTIONS
#
pos <- function(x, cutoff){
	# x minus some value where positive
	x <- x - cutoff
	x[x< 0] <- 0
	x
}

# Link functions
logit <- function(p){-log(1/p - 1)}
unlogit <- function(p){1 / (1 + exp(-p))}
	
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



# OUTCOMES
#
# We will look at lethal and non-lethal pathologies simultaneously
# and look at all toxicities which occur more than 30 times in the
# data set.
#
# We will also look at the following groups of pathologies
# 
# Note: some of these categories are sparsely populated
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

# merge lethal and non_lethal results
merged_macros <- ontology$merge_macros(data)
data <- cbind(data[,!names(data) %in% c$MACROS_], merged_macros)

# build grouped pathologies
for(g in names(groups)){
	data[,g] <- rowSums(data[,groups[[g]]])
}

# Remove toxicities with less than 30 representatives
common_toxicities <- freq$get_columns(data, c$MACROS, minimum_sum=min_toxicities)
common_toxicities <- common_toxicities[!is.na(common_toxicities)]
data <- data[,!names(data) %in% c$MACROS | names(data) %in% common_toxicities]
for(f in factors){
	data[,f] <- factor(data[,f])
}

# List all toxicities 
# and produce a long form table where each toxicity gets its own column
all_toxicities = c(common_toxicities, names(groups))
long <- melt(data, measure.vars=c(all_toxicities))
long$p <- 0
long$type <- 'toxicity'
long$type[long$variable %in% names(groups)] <- 'grouped'


# TABLES
#
# TABLE 1.
#
# Get counts of the number of animals in each treatment group

table.1 <- ddply(data, .(radn, total_dose, dose_rate, fractions), function(df){
	c(n=nrow(df))
})
table.1

# TABLE 2.
#
# Get toxicity descriptions and counts

table.2 <- 
	data.frame(
		symbol=all_toxicities,
		description=ontology$macro2janus_description(all_toxicities),
		n=laply(all_toxicities, function(t){sum(data[,t])})
	)
table.2

# Figure 1.
#
# Lets look at toxicity results as a function of age

plot.1.toxicities <- ggplot(long[long$type == 'toxicity' & long$radn == "C",], aes(age, value)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5) + 
	facet_wrap(~ variable) + 
	ylim(0, 1) +
	opts(title="Toxicity vs Age in Control Animals")
plot.1.toxicities
plot.1.toxicities.url= "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-09-05%20at%202.06.58%20PM.png"

plot.1.grouped <- ggplot(long[long$type == 'grouped' & long$radn == "C",], aes(age, value)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5) + 
	facet_wrap(~ variable) + 
	ylim(0, 5) + 
	opts(title="Grouped Toxicity counts vs Age in Control Animals")
plot.1.grouped
plot.1.grouped.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-09-05%20at%202.18.56%20PM.png"

# Figure 2.
#
# Lets look at toxicity results as a function of age and gender

plot.2.toxicities <- ggplot(long[long$type == 'toxicity' & long$radn == "C",], aes(age, value, color=sex)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5) + 
	facet_wrap(~ variable) + 
	ylim(0, 1) +
	opts(title="Toxicity counts vs Age and Gender in Control Animals")

plot.2.toxicities
plot.2.toxicities.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-09-05%20at%202.21.14%20PM.png"

plot.2.grouped <- ggplot(long[long$type == 'grouped' & long$radn == "C",], aes(age, value, color=sex)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5) + 
	facet_wrap(~ variable) + 
	ylim(0, 5) + 
	opts(title="Grouped Toxicity counts vs Age and Gender in Control Animals")

plot.2.grouped
plot.2.grouped.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-09-05%20at%202.24.24%20PM.png"

# Figure 3.
#
# Lets look at toxicity as a function of age and treatment

plot.3.general.toxicity <- ggplot(long[long$type == 'toxicity',], aes(age, value, color=radn)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5) + 
	facet_wrap(~ variable) + 
	ylim(0, 1)
plot.3.general.toxicity
plot.3.general.toxicity.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-08-23%20at%2011.37.17%20AM.png"

plot.3.general.grouped <- ggplot(long[long$type == 'grouped',], aes(age, value, color=radn)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5) + 
	facet_wrap(~ variable) + 
	ylim(0, 5)
plot.3.general.grouped
plot.3.general.grouped.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-08-23%20at%2011.38.29%20AM.png"

plot.3.gamma.toxicity <- ggplot(long[long$radn %in% c('G', 'C') & long$type == 'toxicity',], aes(age, value, color=tmt)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5, se=FALSE) + 
	facet_wrap(~ variable) + 
	ylim(0, 1)
plot.3.gamma.toxicity
plot.3.gamma.toxicity.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-08-23%20at%2011.40.29%20AM.png"

plot.3.gamma.grouped <- ggplot(long[long$radn %in% c('G', 'C') & long$type == 'grouped',], aes(age, value, color=tmt)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5, se=FALSE) + 
	facet_wrap(~ variable) + 
	ylim(0, 5)
plot.3.gamma.grouped
plot.3.gamma.grouped.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-08-23%20at%2011.41.53%20AM.png"

plot.3.neutron.toxicities <- ggplot(long[long$radn %in% c('N', 'C') & long$type == 'toxicity',], aes(age, value, color=tmt)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5, se=FALSE) + 
	facet_wrap(~ variable) + 
	ylim(0, 1)
plot.3.neutron.toxicities
plot.3.neutron.toxicities.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-08-23%20at%2011.44.06%20AM.png"

plot.3.neutron.grouped <- ggplot(long[long$radn %in% c('N', 'C') & long$type == 'grouped',], aes(age, value, color=tmt)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_smooth(size=0.5, alpha=0.5, se=FALSE) + 
	facet_wrap(~ variable) + 
	ylim(0, 5)
plot.3.neutron.grouped
plot.3.neutron.grouped.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-08-23%20at%2011.44.55%20AM.png"

# Figure 4. 
#
# Lets make sure our models are well parameterized
# Especially in regards to age which has a complex
# relationship to the incidence of disease.
#
# We will do this by modeling the relationship between
# age and disease with k-splines.

formula <- value ~ age + I(pos(age, 877)^3) + I(pos(age, 1045)^3) + I(age^2) + I(age^3)
models <- dlply(long[long$type == 'toxicity',], .(variable), function(df){	 
	glm(
		formula=formula, 
		family=gaussian, 
		data=df
	); 
})

subset <- (long$type == 'toxicity')
long[subset,]$p <- ddply(long[subset,], .(variable), function(df){
	model <- models[[df$variable[1]]]
	prediction <- predict(model)
	data.frame(prediction)
})[,2]

plot.4.age.model.toxicities <- ggplot(long[long$type == 'toxicity',], aes(age, p)) + 
	geom_point(size=0.5, alpha=0.5) + 
	geom_line(size=0.5, alpha=0.5) + 
	facet_wrap(~ variable) + 
	ylim(0, 1)
plot.4.age.model.toxicities
plot.4.age.model.toxicities.url = "http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-08-23%20at%201.33.03%20PM.png"
# compare to plot.1.grouped.url
# it looks really close.  By eye, I'd say this is a good model for the relationship
# between lifespan and 


# **** BAD ATTEMPT WILL NOT BE USED - DETAILS BELOW *****
# Table 3
#
# Lets look at those radiation doses responded to gender
# First we will setup baseline incidence rates, in control
# animals and then look for radiation responses

subset <- (long$type == 'toxicity')
control_models <- dlply(long[subset,], .(variable), function(df){
	formula <- value ~ age + pos(age, 877) + pos(age, 1045) + sex
	model <- glm(formula, family=binomial, data=df[df$radn == 'C',]) 
})
radiation_models <- dlply(long[subset,], .(variable), function(df){
	formula <- value ~ offset(p) + total_dose - 1
	control_model <- control_models[[df$variable[1]]]
	df$p <- predict(control_model, df)
	model <- glm(formula, family=binomial, data=df[df$radn %in% c('C', 'G'),]) 
	model
})
experimental_models <- dlply(long[subset,], .(variable), function(df){
	formula <- value ~ offset(p) + total_dose*sex - sex - total_dose - 1
	rad_model <- radiation_models[[df$variable[1]]]
	df$p <- predict(rad_model, df)
	model <- glm(formula, family=binomial, data=df[df$radn %in% c('C', 'G'),]) 
	model
})


# Problems with table 3
#
# My first attempt at table 3 produces some rather unacceptable results
# take for instance the pathology DER.  Our 'radiation model' is meant 
# to capture the effect of radiation on incidence.

	summary(radiation_models[['DER']])
	
# According to this result, as radiation increases the odds of DER
# occurance drops to zero.  A graph of the DER data proves that this
# is not so.

	ggplot(data[data$DER == 1,], aes(age, total_dose, color=sex)) + 
	geom_point() + 
	facet_wrap(~radn)
		
# Clearly radiation has its fair share of DER entries.  Moreover,
# the data for control animals has no female cases!  
#
# If we look at the expected odds by of the control models the results
# are absurd.  Observe the following:

	temp   <- data[data$radn %in% c('C'),]
	temp$p <- 1 / (1 + exp(-predict(control_models[['DER']])))
	
	quantile(temp$p)
	ggplot(temp, aes(age, p, color=sex)) + geom_point() + scale_y_log10()
	
# We predict odds of DER for middle aged females as low as
# 1 in 20 billion.  Surely we cannot get this level of confidence
# from the 13 of 1093 observations of DER positive mice that we have
# in our dataset.
#
# This problem can be addressed by using a penalized logistic regression
# (note I tested several values of l1 and l2 in caret without making much
#  difference.  I dropped the use of caret because I could not get it to
#  perform logistic regressions very well.)
	
	temp    <- data[data$radn %in% c('C'),]
	input   <- ~ age + pos(age, 877) + pos(age, 1045) + sex
	model 	<- penalized(DER, input, lambda1=1, lambda2=1, model='logistic', data=temp)
	temp$p  <- predict(model, input, data=temp)
	
	quantile(temp$p)
	ggplot(temp, aes(age, p, color=sex)) + geom_point() + scale_y_log10()

# The predictions are much more reasonable, bottoming out at 0.0004 or about
# 1 in 3000 a much more reasonable value considering the sparsity of our observations
#
# But what about the performance of subsequent models

	temp    <- data[data$radn %in% c('C', 'G'),]
	temp$p  <- -log(1/predict(model, input, data=temp) - 1)

	formula <- DER ~ offset(p) + total_dose - 1
	model <- glm(formula, family=binomial, data=temp) 
	summary(model)
	
# Notice the much more reasonable conclusion that radiation
# has little effect on DER rates.  This is confirmed by a
# table of radaition vs DER occurance

	daply(data, .(radn), function(df){sum(df$DER) / nrow(df)})
	

# Lessons from table 3 mishap
#
# Well we can see the dangers of non-penalized regressions
# I think we need to look carefully at significant p values,
# graph or make tables of the underlying data and look to see
# if our quick interpretations confirm the significance seen in
# the results.
#
# If they do not, then we are likely to be viewing some sort of
# anomolous behavior.
#
# In a more general sense I see a massive reason to employ even
# small amounts of regularlization in every regression, this can
# save us from very stupid conclusions.  It speaks to the utility
# of a baysian approach.  Keep us robust!

# **** BAD ATTEMPT WILL NOT BE USED - DETAILS BELOW *****
# Table 3 (2nd attempt)
#
# This time we will use regularized regression to form
# the baseline models.  We will also check our conclusions
# by graphing the results.

	# Find the robust effects of age and gender
	subset <- (long$type == 'toxicity')
	input <- ~ age + pos(age, 877) + pos(age, 1045) + sex
	control_models <- dlply(long[subset,], .(variable), function(df){
		data <- df[df$radn == 'C',]
		model <- penalized(value, input, lambda1=4, lambda2=4, model='logistic', data=data)
	})
	
	# Make predictions using control data
	temp <- data[data$radn %in% c('C', 'G'),]
	control_predictions <- llply(control_models, function(m){
		predict(m, input, data=temp)
	})
	
	# Make sure there is nothing weird going on
	sort(unlist(llply(control_predictions, min)))
	sort(unlist(llply(control_predictions, max)))
	
	plot_toxicity_incidence("KID")
	plot_predictions(control_predictions[["KID"]], temp)
	
	
# Problems with the 2nd attempt
#
# Even after regularization, there are still problems with our
# attempt to model baseline risk.  For instance KID outcomes
# are estimated at 1e-21 in very young mice.  This seems to be
# impossibly low given the sparsity of data.
#
# Regularization could be used to fight this weird outcome
# but it will then eliminate gender effects that are likely
# to be real.
#
# My synopsis is that our indiscriminate cutting of the age 
# into quantiles is inapprpriate because in cases where there
# are very few observations there can should be less knots
# in the spline fitter.
#
# I could remedy this by avoiding cuts in sparse data like so:

	temp    <- data[data$radn %in% c('C'),]
	input   <- ~ age + sex
	model 	<- penalized(KID, input, lambda1=.1, lambda2=.1, model='logistic', data=temp)
	temp    <- data[data$radn %in% c('C', 'G'),]
	temp$p  <- predict(model, input, data=temp)
	
	quantile(temp$p)
	plot_toxicity_incidence("KID")
	table(data$KID, data$radn)
	ggplot(temp, aes(age, p, color=sex)) + geom_point() + scale_y_log10()
	
# This looks more reasonble by many measures.  For the rest I will use
# age limits based on the number of observations.


# Table 3 (3rd attempt)
#
# Now we are varying the number of age cuts that we use to fit
# the data.  Dense data like NTYG will have 3 cuts, sparse data
# like KID will have no cuts

	# Link functions
	logit <- function(p){-log(1/p - 1)}
	unlogit <- function(p){1 / (1 + exp(-p))}
	
	# Useful troubleshooting plots
	plot_toxicity_incidence <- function(toxicity){
		ggplot(data[data[,toxicity] == 1,], aes(age, total_dose, color=sex)) + 
		geom_point() + 
		facet_wrap(~radn)
	}
	plot_predictions <- function(predictions, data){
		data$p <- predictions
		ggplot(data, aes(age, p, color=sex)) + geom_point() + scale_y_log10()
	}
	
	# the number and position of knots that we should use
	get_input <- function(toxicity){
		n <- sum(data[data$radn == 'C', as.character(toxicity)])
		if(n > 200){
			input <- ~ age + pos(age, 900) + pos(age, 1100) + sex		
		}
		else if(n > 100){
			input <- ~ age + pos(age, 1000) + sex
		}
		else{
			input <- ~ age + sex	
		}
		input
	}

	# Find the robust effects of age and gender
	subset <- (long$type == 'toxicity')
	control_models <- dlply(long[subset,], .(variable), function(df){
		input   <- get_input(df$variable[1])
		data    <- df[df$radn == 'C',]
		model   <- penalized(value, input, lambda1=.2, lambda2=.5, model='logistic', data=data)
	})
	
	# Make predictions using control models
	temp <- data[data$radn %in% c('C', 'G'),]
	control_predictions <- llply(names(control_models), function(n){
		input <- get_input(n)
		predict(control_models[[n]], input, data=temp)
	})
	names(control_predictions) <- names(control_models)
	
	# Make sure there is nothing weird going on
	sort(unlist(llply(control_predictions, min)))
	sort(unlist(llply(control_predictions, max)))
	
	plot_toxicity_incidence("TADR")
	plot_predictions(control_predictions[["TADR"]], temp)
	
	# Add radiation 
	radiation_models <- dlply(long[subset,], .(variable), function(df){
		formula <- value ~ offset(p) + total_dose - 1
		df      <- df[df$radn %in% c('C', 'G'),]
		df$p    <- logit(control_predictions[[df$variable[1]]])
		model   <- glm(formula, family=binomial, data=df) 
		model
	})
	
	# Make predictions using radiation models
	radiation_predictions <- llply(radiation_models, function(m){
		unlogit(predict(m))
	})	
	
	# Make sure things are okay dokey
	sort(unlist(llply(radiation_predictions, min)))
	sort(unlist(llply(radiation_predictions, max)))
	
	# What is the effect of radiaiton
	rad_effects <- ldply(radiation_models, function(m){data.frame(summary(m)$coefficients)})
	rad_effects[order(rad_effects$Pr...z..),]
	
	tox <- "MET"
	plot_toxicity_incidence(tox)
	plot_predictions(radiation_predictions[[tox]], temp)
	table(data$radn, data[,tox])

	
	llply(radiation_models, summary)
	plot_toxicity_incidence("TOVE")
	
experimental_models <- dlply(long[subset,], .(variable), function(df){
	formula <- value ~ offset(p) + total_dose*sex - sex - total_dose - 1
	rad_model <- radiation_models[[df$variable[1]]]
	df$p <- predict(rad_model, df)
	model <- glm(formula, family=binomial, data=df[df$radn %in% c('C', 'G'),]) 
	model
})

	

	temp    <- data[data$radn %in% c('C', 'G'),]
	temp$p  <- -log(1/predict(model, input, data=temp) - 1)

	formula <- DER ~ offset(p) + total_dose - 1
	model <- glm(formula, family=binomial, data=temp) 
	summary(model)
	
# Notice the much more reasonable conclusion that radiation
# has little effect on DER rates.  This is confirmed by a
# table of radaition vs DER occurance

	daply(data, .(radn), function(df){sum(df$DER) / nrow(df)})
	


temp$p <- predict(model, temp)

formula <- OVE ~ p + total_dose + total_dose*sex - sex
model <- glm(formula, family=binomial, data=temp[temp$radn %in% c('C', 'G'),]) 


# Table 3 - backing off
#
# Having made several attempts at table 3 now.  I find myself fudementally
# dis-satisfied.  The choice of a baseline model seems utterly arbitrary.
# Wouldn't it make far more sense to measure the performance of the baseline
# models using cross validation and bootstrapping in order to determine which
# of various approaches is actually the best?
#
# I will attempt to do this in the following sections in order to produce
# the best models of the control data from a pool of potential 

	# Control variables
	baseline_formulas <- c(
		'value ~ i',
		'value ~ i + age',
		'value ~ i + sex',
		'value ~ i + sex*age',
		'value ~ i + age + I(age^2)',
		'value ~ i + sex*age + sex*I(age^2)',
		'value ~ i + (age<850) + (age<1000) + (age<1150)',
		'value ~ i + sex*(age<850) + sex*(age<1000) + sex*(age<1150)'
	)
	lambda1s <- c(0)
	lambda2s <- c(1, 0.3, 0.1, 0.01)
	bootstraps <- 20
	
	# Parse configuration settings
	control_long <- long[long$radn == "C" & long$type == 'toxicity'
					     ,]
					     #& long$variable %in% c('MET', 'OVE', 'PGL', 'TOVE', 'TUTE', 'UTE', 'PNU'),]
	combinations <- expand.grid(f=baseline_formulas, l1=lambda1s, l2=lambda2s, stringsAsFactors=FALSE)

	# Run models
	performance <- ldply(1:bootstraps, function(b){
		
		# Bootstrap data
		n <- nrow(control_long)
		d <- control_long[sample(1:n, replace=TRUE),]
		
		# Add an intercept column, always 1
		d$i <- 1
		
		# Try our models
		p <- ddply(d, .(variable), function(df){
			likelihoods <- adply(combinations, .(1), function(c){
								
				# debugging tool
				"c  <- combinations[1,]
				df <- d[d$variable == 'TADN',]"
				
				# Train model with cross validation
				f <- as.formula(as.character(c$f[1]))
				m <- cvl(
					f, 
					unpenalized = ~0,
					lambda1=c$l1, 
					lambda2=c$l2, 
					model='logistic', 
					data=df, 
					maxiter=25, 
					fold=5, 
					standardize=TRUE
				)
				
				m$cvl
			})
		})
		p$bootstrap <- b
		p
	})
	
	names(performance) <- c('tox', 'model', 'l1', 'l2', 'log.likelihood', 'bootstrap')
	
	# Make likelihood relative to intercept model
	performance <- ddply(performance, .(tox, bootstrap), function(df){
		baseline <- df$log.likelihood[df$model == 'value ~ i'][1]
		df$relative.likelihood <- df$log.likelihood - baseline
		df
	})
	
	# Aggregate bootstraps
	aggregate <- ddply(performance, .(tox, model, l1, l2), function(df){
		o = df$relative.likelihood
		c(mean=mean(o), sd=sd(o), z=mean(o)/sd(o))
	})
	
	# Choose the best models
	baselines <- ddply(aggregate, .(tox), function(df){
		z.threshold <- 1.0
		df$z[is.na(df$z)] <- z.threshold
		df <- df[df$z >= z.threshold,]
		df[df$mean == max(df$mean, na.rm=T),]
	})
	
# Baseline report (supplemental)
#
# Now we are satisfied that our current baseline models do a reasonable job
# of characterizing the data.  When data is sparse, as in rare outcomes, 
# simpler models reign and when data is plentiful more complex models can
# reasonably be employed.  This approach does the best job of characterizing
# the baseline incidences that we can.
#
# To emphasize this point, I will walk through several examples of models
# where more or less elaborate models are needed.  I will show off the 
# final likelihoods of the models we chose for each, the underlying data 
# for the toxicity and the range of predictions that these models produce
# to show that they seem to be reasonably sane approaches.

	# Show model performances
	plot_performances <- function(toxicities){
		ggplot(
			data=performance[
				(performance$tox %in% toxicities) 
			,], 
			aes(factor(paste(l1, l2)), relative.likelihood)) + 
		geom_boxplot() + facet_grid(tox ~ model, scales="free_y") + 
		geom_hline(yintercept=0, color='red') +
		opts(axis.text.x=theme_text(angle=-90))
	}
		
	# Or in general
	table.supplemental.1 <- merge(baselines, table.2, by.x='tox', by.y='symbol')

	# Size vs model selection
	ggplot(table.supplemental.1, aes(x=model, y=log(n))) + geom_point(alpha=0.3)  +
 	opts(axis.text.x=theme_text(angle=-90))

	# Lets explore some of the exceptional points
	table.supplemental.1[order(table.supplemental.1$n), c("model", "l1", "l2", "tox", "n")]
	
		# Uses the intercept model
		# despite a high n
		plot_performances("TVAS")        # shitty performance
		plot_performances("CLR")         # just shitty performance
		
		# Why does age ever make sense
		plot_performances("ADH")         # its really only trivially better than age^2 model
										 # or age^2*sex
		plot_performances("TADR")        # this genuinely falls apart in sex*age^2

		# Why does sex*age ever make sense
		plot_performances("CRD")         # a little better than baseline
		plot_performances("THGL")        # trivially better than sex*age^2
		
		# Why does cutoff model have a high n
		plot_performances("TOVE")        # its definiately the best fit
		plot_toxicity_incidence("TOVE")  # probably because its female specific and 
		                                 # dependent on puberty and menapause
		
		# Why use anything but intercept for low n (<100)
		plot_toxicity_incidence("MET")         # 35, probably anomolous
		plot_toxicity_incidence("PGL")         # 37, again probably anomolous
		plot_toxicity_incidence("DER")         # 64, probably real, expect maybe the gender thing
		plot_toxicity_incidence("TPYL")        # 72, close call
		plot_toxicity_incidence("TUTE")        # 78, probably real
		
		# Why does sex*age^2 work for only some high n?
		plot_performances("PNC")              # only marginally different than additive models
		plot_performances("BDY")              # seems erratic
		plot_performances("DIV")              # seems legit
		
	# predictions
	#
	# Lets see the range of predictions for these models.
	# Predicted probabilities below 1/1000 should be scrutinized as
	# There are only roughly 1000 training examples it would be unlikely
	# that the estimated odds might be lower than that. (baysian assumption)

	ungrouped_long <- long[long$type == 'toxicity'
						     ,]
					     #& long$variable %in% c('MET', 'OVE', 'PGL', 'TOVE', 'TUTE', 'UTE', 'PNU'),]
	bootstraps <- 20

	baseline_predictions <- ldply(1:bootstraps, function(b){
		
		# Bootstrap data
		n <- nrow(ungrouped_long)
		d <- ungrouped_long[sample(1:n, replace=TRUE),]
		
		# Scale necessary factors
		d$age <- scale(d$age)
		
		# Add an intercept column, always 1
		d$i <- 1
		
		# Try our models
		p <- ddply(d, .(variable), function(df){
			
			# debugging tool
			"df <- d[d$variable == 'CRD',]"
			
			# get the baseline formula
			c <- baselines[baselines$tox == df$variable[1],]
								
			# Train model on control data
			cd <- df[df$radn == 'C',] 
			f <- as.formula(as.character(c$model[1]))
			m <- penalized(
					f, 
					unpenalized = ~0,
					lambda1=c$l1, 
					lambda2=c$l2, 
					model='logistic', 
					data=df, 
					maxiter=25, 
					standardize=TRUE
				)
			
			# Predict all data and return the distribution
			p <- predict(m, f, data=df)
			quantile(p)
		})
		p$bootstrap <- b
		p
	})
	
	l <- melt(baseline_predictions[,!names(baseline_predictions) %in% 'bootstrap'])
	names(l) <- c('tox', 'percentile', 'p')
	ggplot(
			data=l,
			aes(factor(percentile), log(p, 10))) + 
		geom_boxplot() + facet_wrap( ~ tox, scales='free_y') + 
		geom_hline(yintercept=0, color='red') +
		geom_hline(yintercept=-3, color='red') +
		opts(axis.text.x=theme_text(angle=-90))
		
	# see http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202012-09-19%20at%203.38.43%20PM.png
	

	# Several are very odd
	# 
	# Gendered results
	# MET, OVE, PGL, TOVE, TUTE, and UTE were making me worried, because
	# they had really messed up predcition curves.  But upon close reflection
	# it is clear that these are all highly gendered outcomes and it does in
	# fact stand to reason that if a toxicity is common in females, but never
	# seen in males that we might estimate that the odds of an occurance in
	# males is actually less than the number of males we observe.
	#
	# For example if we consider the odds that a tail might come up in a 
	# coin.  If we have never seen the coin flip it would be 50/50.  If
	# we had seen the coin flip 1000 times and it were always tails then 
	# we might say the odds of heads is even lower than 1/1000.  An interesting
	# and somewhat counter-intuitive result.


# supplemental experiment - penalization
#
# Here we explore which modes of penalization work best
# on an artificial dataset.  Our basic conclusion is that
# l2 regularization is sufficiently powerful and that we 
# should try settings of 1.0, 0.3, and 0.1 in our regularization
#
	tempf <- function(n=1000, l1=0, l2=0, f=y ~ x, data=temp, folds=5, iter=25){		

		m.cv <- cvl(
				f, 
				unpenalized = ~0,
				lambda1=l1, 
				lambda2=l2, 
				model='logistic', 
				data=data, 
				maxiter=iter, 
				standardize=TRUE,
				fold=folds
			)
			
		m <- penalized(
			f, 
			unpenalized = ~0,
			lambda1=l1, 
			lambda2=l2, 
			model='logistic', 
			data=data, 
			maxiter=iter, 
			standardize=TRUE
		)
		
		p <- predict(m, f, data=data)
			
		list(m.cv$cvl, quantile(p))
	}
	
	temp <- data.frame(y=rep(0, n), x=rep(1, n))
	temp$y[1] <- 1
	
	tempf()
	tempf(l1=1e-2, l2=1e-1)
	tempf(l1=1e-2, l2=1.5e-1)
	tempf(iter=1000)

	temp <- data.frame(y=rep(0, n), x=rep(1, n))
	tempf(l1=1.0, l2=1.0)  # 0.006
	tempf(l1=0.3, l2=1.0)  # 0.0054
	tempf(l1=0.1, l2=1.0)  # 0.0053
	tempf(l1=0.0, l2=1.0)  # 0.0052
	tempf(l1=1.0, l2=0.3)  # 0.0027
	tempf(l1=0.3, l2=0.3)  # 0.0021
	tempf(l1=0.1, l2=0.3)  # 0.0019
	tempf(l1=0.0, l2=0.3)  # 0.0018
	tempf(l1=1.0, l2=0.1)  # 0.0016
	tempf(l1=0.3, l2=0.1)  # 0.000999
	tempf(l1=0.1, l2=0.1)  # 0.000811
	tempf(l1=0.0, l2=0.1)  # 0.000723
	tempf(l1=1.0, l2=0.0)  # 
	tempf(l1=0.3, l2=0.0)
	tempf(l1=0.1, l2=0.0)
	tempf(l1=0.0, l2=0.0)

	# Predict all data and return the distribution
	#p <- predict(m, f, data=temp)
	#quantile(p)


# Experiment in using prediction to estimate
#
# The approach below works nicely.  Coefficients are consistent.
#   
#  1. Add the p=logit(prediction) to the data frame
#  2. Add ~p to the unpenalized argument in modeling and predicting

 		n    <- 1000
		x1   <- runif(n)
		x2   <- runif(n)
		e    <- runif(n)
		i    <- rep(1, n)
		y    <- (x1 + x2 + e) < 1.5
		temp <- data.frame(y, x1, x2, e, i)
		f    <- y ~ x1 + i
		l1   <- 0
		l2   <- 0
		iter <- 25
		m <- penalized(
			f, 
			unpenalized = ~0,
			lambda1=l1, 
			lambda2=l2, 
			model='logistic', 
			data=temp, 
			maxiter=iter, 
			standardize=TRUE
		)
		
		p <- predict(m, f, data=temp)
		quantile(p); coefficients(m)
		
		temp$p <- logit(p)
		f      <- y ~ i
		u      <- ~ p
		l2     <- 1
		m2 <- penalized(
			f, 
			unpenalized = u,
			lambda1=l1, 
			lambda2=l2, 
			model='logistic', 
			data=temp, 
			maxiter=iter, 
			standardize=TRUE
		)
		p2 <- predict(m2, f, u, data=temp)
		quantile(p2); coefficients(m2)

	
# Table 3 - final showdown
# 
# I think we have actual good estimates for the baseline curves.
# Now its time to add radiation to these baseline models.  Once
# again we will try serveral models and use those which improve
# the overall model fit the most.  Once we have established which
# models these are, we will build confidence intervals for the
# terms of the model.

	# Control variables
	experimental_formulas <- c(
		'value ~ p',
		'value ~ p*total_dose',
		'value ~ p*total_dose + p*I(total_dose^2)'
	)
	lambda1s <- c(0)
	lambda2s <- c(1, 0.3, 0.1, 0.01)
	bootstraps <- 20
	
	# Parse configuration settings
	gamma_long <- long[long$radn %in% c("G", "C") & long$type == 'toxicity'
					     #,]
					     & long$variable %in% c('MET', 'OVE', 'PGL', 'TOVE', 'TUTE', 'UTE', 'PNU'),]
	combinations <- expand.grid(f=experimental_formulas, l1=lambda1s, l2=lambda2s, stringsAsFactors=FALSE)

	# Run models
	performance <- ldply(1:bootstraps, function(b){
		
		# Bootstrap data
		n <- nrow(gamma_long)
		d <- gamma_long[sample(1:n, replace=TRUE),]

		# add and intercept
		d$i <- 1
				
		# Try our models
		p <- ddply(d, .(variable), function(df){
			likelihoods <- adply(combinations, .(1), function(c){
				
				# debugging tool
				"c  <- combinations[1,]
				df <- d[d$variable == 'MET',]"

				# get the baseline formula
				b <- baselines[baselines$tox == df$variable[1],]
									
				# Train model on control data
				cd <- df[df$radn == 'C',] 
				f <- as.formula(as.character(b$model[1]))
				m <- penalized(
						f, 
						unpenalized = ~0,
						lambda1=b$l1, 
						lambda2=b$l2, 
						model='logistic', 
						data=cd, 
						maxiter=25, 
						standardize=TRUE
					)
				
				# Predict all data
				df$p <- predict(m, f, data=df)
					
				
				# Train model with cross validation
				f <- as.formula(as.character(c$f[1]))
				u <- ~ p
				m <- cvl(
					f, 
					unpenalized = u,
					lambda1=c$l1, 
					lambda2=c$l2, 
					model='logistic', 
					data=df, 
					maxiter=25, 
					fold=5, 
					standardize=TRUE
				)
				
				m$cvl
			})
		})
		p$bootstrap <- b
		p
	})
	
	names(performance) <- c('tox', 'model', 'l1', 'l2', 'log.likelihood', 'bootstrap')
	
	# Make likelihood relative to intercept model
	performance <- ddply(performance, .(tox, bootstrap), function(df){
		baseline <- df$log.likelihood[df$model == 'value ~ p'][1]
		df$relative.likelihood <- df$log.likelihood - baseline
		df
	})
	
	# Aggregate bootstraps
	aggregate <- ddply(performance, .(tox, model, l1, l2), function(df){
		o = df$relative.likelihood
		c(mean=mean(o), sd=sd(o), z=mean(o)/sd(o))
	})
	
	# Choose the best models
	experimental_baselines <- ddply(aggregate, .(tox), function(df){
		z.threshold <- 1.0
		df$z[is.na(df$z)] <- z.threshold
		df <- df[df$z >= z.threshold,]
		df[df$mean == max(df$mean, na.rm=T),]
	})


# table 4
table.4.toxicities <- c("ADR", "BDY", "DER", "HRG", "HTX", "JAU", "KID", "LIV", "TADR", "THGL")
table.4.fun <- function(pathology){
	formula <- paste(pathology, "~ treatment + q.age*sex -sex -q.age -1")
	family <- binomial(link = "logit")
	model <- glm( formula=formula, data=data[ALL & !r$NEUTRON ,], family = family )
	coef_ <- coef(model)
	f.coef <- coef_[1:6]
	m.coef <- coef_[7:12]
	f.odds <- odds_ratio$logit2or(f.coef, f.coef[1])
	m.odds <- odds_ratio$logit2or(m.coef, m.coef[1])
	return(rbind(f.odds,m.odds))
}
pathology <- "BDY"
formula <- paste(pathology, "~ treatment + q.age")
family <- binomial(link = "logit")
model <- glm( formula=formula, data=data[ALL & !r$NEUTRON ,], family = family )
odds_ratio$logit2or(coef(model), coef(model)[1])

# @TODO add km-curves
