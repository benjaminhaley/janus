# ML on A-bomb data
# benjamin.haley@gmail.com 
# April 2013 - 
#
# Introduction:
# At the Modeling workshop in Herrsching, Christian Kaiser, 
# christian.kaiser@helmholtz-muenchen.de, recommended
# that the machine learning and cross validation methods that I
# presented in my poster might be applied to data from the atomic
# bomb survivors.
#
# This is a first attempt at that kind of analysis.  I will send him
# what I find and see if we can do anything interesting with it.
# The data that I am using is the lls14 dataset from rerf.jp (see
# acknowledgement).
#
#
# Acknowledgement:
# This report makes use of data obtained from the Radiation Effects Research Foundation (RERF), Hiroshima and Nagasaki, Japan. RERF is a private, non-profit foundation funded by the Japanese Ministry of Health, Labour and Welfare and the U.S. Department of Energy, the latter through the National Academy of Sciences.The conclusions in this report are those of the authors and do not necessarily reflect the scientific judgment of RERF or its funding agencies.
# Please send a copy of any reprints that make use of these data to:
# Archives Unit, Library and Archives Section
# Department of Information Technology
# Radiation Effects Research Foundation
# 5-2 Hijiyama Park
# Minami-ku Hiroshima, 732-0815 JAPAN


# Distributions
#
# Abstract:
# Lets see what the data looks like and how it is distributed.

	# Load data
	setwd('~/janus/')
	data <- read.csv('data/lss14/lss14.csv')
	
	# Libraries
	library(ggplot2)
	library(plyr)
	library(reshape2)
	
	# Definitions
	of_interest <- c(
		"agexcat", "agecat",
		"dosecat", "subjects", "pyr", "colon10",
		"death", "solid"
	)
	
	# Reshape
	small <- data[sample(n)[1:1000],]
	m <- melt(data)
	m2 <- melt(small[,of_interest])
	two_way <- ddply(m2, .(variable), function(df) data.frame(df, m2))

	
	# Graph
	ggplot(m, aes(value)) + 
		geom_density() +
		facet_wrap(~variable, scales='free') +
		theme(axis.text.x = element_text(angle = 90, hjust = 1))
	
	# Two way Graph
	ggplot(two_way, aes(value, value.1)) + 
		geom_point(alpha=0.5, size=1) +
		geom_density2d() +
		facet_wrap(variable~variable.1, scales='free')
	

# Results:
#		
# Basic Distributions
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-04-22%20at%203.26.54%20PM.png
# Two way distributions
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-04-22%20at%203.57.08%20PM.png
 

# Model Comparison
# 
# Abstract:
# I will try to compare a model closely based on the recent lls work
# by Osaza (http://www.rrjournal.org/doi/pdf/10.1667/RR2629.1) with
# a gbm model that uses the same parameters.

	# Load data
	setwd('~/janus/')
	data <- read.csv('data/lss14/lss14.csv')
	
	# Libraries
	library(ggplot2)
	library(plyr)
	library(reshape2)
	library(gbm)
	
	# Shuffle data
	data <- data[sample(nrow(data)),]
	
	# Define hazard
	data$hazard <- data$solid / data$pyr
	
	# Define dose
	data$dose <- data$colon10 / 1000
	
	# Helper functions
	report_err <- function(coef, se) {
		c(
			min=exp(coef - 1.96*se) - 1,
			u=exp(coef) - 1,
			max=exp(coef + 1.96*se) - 1
		)
	}
	
	# Define Standard model
	#
	# A poisson regression
	# using city, sex, birth year*, attained age, and dose
	# *I used age at exposure as a stand in for birth year	
	# based on Ozasa 2012
	# http://www.rrjournal.org/doi/pdf/10.1667/RR2629.1
	#
	standard_model <- function(data) {
		glm(
			solid ~ 
				city + 
				sex*I(log(age)) + 
				sex*agex + 
				dose + 
				offset(log(pyr)),
			family='poisson',
			data = data
		)
	}
		
	# Define Machine Learning model
	#
	# A gradient boosted machine learner
	# using the same factors as the standard model
	#
	ml_model <- function(data){
		gbm(
			solid ~ 
				city + 
				sex +
				age + 
				agex + 
				dose + 
				offset(log(pyr)),
			distribution = 'poisson',
			data=data,
			n.trees=1000,
			interaction.depth=2,
			cv.folds=5,
			shrinkage=0.05,
			verbose=F
		)			
	}
	
	
	# Sanity Check
	#
	# Show that excess relative risk is reasonble for
	# the standard model I used
	m <- standard_model(data)
	s <- summary(m)
	report_err(
		coef=s$coefficients['dose', 'Estimate'], 
		se=s$coefficients['dose', 'Std. Error']
	)
	# We get    0.36 to 0.43 to 0.50
	# Ozasa got 0.32 to 0.42 to 0.53
	# close...
	
	
	# Cross validated predictions
	# the training set will be used to build the model
	# the test set will be predicted
	folds <- 5
	data$set <- sample(1:folds, nrow(data), replace=TRUE)
	for(i in 1:folds){
		
		# Divide data 
		train <- data[data$set != i,]
		test <- data[data$set == i,]
		
		# Build models
		m1 <- standard_model(train)
		m2 <- ml_model(train)

		# Make Predictions
		test$p1 <- exp(predict(m1, newdata=test))
		trees <- gbm.perf(m2, method='cv')
		p2 <- predict(m2, newdata=test, n.trees=trees)
		test$p2 <- exp(p2 + log(test$pyr))
		
		# Add predictions to data
		data$p1[data$set == i] <- test$p1
		data$p2[data$set == i] <- test$p2	
		
		# Update user
		print(paste(i, 'of', folds))
	}
		
	# Sanity Check II
	#
	# Make sure the predictions of both models are similar
	ggplot(data, aes(p1, p2)) + 
		geom_point() + 
		geom_density2d() +
		scale_x_log10() +
		scale_y_log10()
	
	# Sanity Check III
	#
	# Make sure the predictions of the two models are reasonable
	ids <- c('dose', 'age', 'city', 'agex', 'pyr', 'dosecat', 'sex')
	m <- melt(
		data[,c('p1', 'p2', 'p3', 'p4', 'solid', ids)],
		id.vars=ids
	)
	ggplot(m, aes(age, value/pyr, color=dosecat, group=factor(dosecat))) + 
		geom_point(size=1, alpha=0.05) +
		geom_smooth(se=F) +
		facet_grid(sex~variable) +
		ylim(0,0.1)
		
	
	# Measure Likelihood
	log_likelihood <- function(y, u){
		sum(y*log(u) - u - log(factorial(y)), na.rm=T)
	}
	
	log_likelihood(data$solid, data$p1)					# -16772*
	log_likelihood(data$solid, data$p2)					# -16764
	log_likelihood(data$solid, (data$p2 + data$p1) / 2)	# -16724
	
	# *note: this is the same as AIC/2 (see next line)
	abs(s$aic + 2*log_likelihood(data$solid, data$p1)) < 1
	

# Results:
#
# I compared a model closely based on the recent lls work
# by Osaza (http://www.rrjournal.org/doi/pdf/10.1667/RR2629.1) with
# a gbm model that uses the same parameters to predict solid tumor
# incidence.  I found that the mean prediction of both models was
# more accurate than either model on their own.
#
# The attempt to replicate Osaza was reasonable, though the
# confidence interval was wider in Osaza's estimate.  At 1Gy Err was 
# estimated at 0.42 (0.32-0.53) in Osaza's paper and 0.43 (0.36-0.50)
# in my reconstruction.  
#
# The machine learning technique and poisson technique have similar 
# performances (log(L) ~ -16770).  But the mean of their predictions 
# has the best performance (log(L) ~ -16725).  This suggests that the
# best model of radiation risk will be a mix of machine learning 
# models and traditional models.
#
# Notably the estimates of likelihood by cross validation were very
# similar to the estimates of likelihood by aic, with a difference of
# less than one.  However, aic can only be estimated for the poisson
# model while the gbm model requires cross validation.
#
# 
# Future Directions:
#
# This analalysis is very preliminary.  There are several points that
# might be worth exploring.
#
#	1. Other tumor end points
#	2. Uncertainty in risk estimates
#	3. Characterizing differences between the poisson and gbm 
#      predictions
#	4. More accurate estimates of the likelihood of each model
#      by repeated cross validation.
#	5. Improvement in the reproduction of osaza's results.
#
#
# Unsettling questions:
#
# In the begining I thought that I would have to weight my poisson
# regression by the number of person years at risk.  But I found that
# this resulted in estimates of ERR with a 95% confidence interval
# much narrower than that found by Osaza.  I don't understand why
# their is no weighting.  Many cells in this analysis have very few
# person years at risk and yet they all contribute equally to the 
# model fitting.  This seems wrong to me.  Shouldn't we care more
# about accuracy where the number of person years at risk is high
# and therefore risk estimate benefits from a large samples size?