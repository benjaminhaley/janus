##############################################################################
#
# Generalizability of Radiation Lifespan Models
# bmh March 2012

# Overview

  # Determine how well lifespan prediction modeling techniques perform on new 
  # experiments for presentation to the wololab grad students and to prototype
  # my graduate thesis.


# Approach

  # This presentation is a little different than the usual powerpoint.  If you
  # have taken science and society it should seem familiar.  We will take turns
  # reading sections and discussing them.
  
  # I am taking this approach for several reasons.  One, my written proposal is
  # due in one month and its most important that I hone my written communication
  # skills.  Two, I would like to experiment with a code driven lecture.  I
  # have taken classes where a coding terminal is used like a chalkboard and
  # I think that it is fun, both because it allows the group to explore the 
  # data together and because it de-mystifies the world of code.  Last, I just
  # finished this work at 2:30 last night and I have not had a chance to practice
  # a talk on it :).
  
  # I work on stuff that's a bit outside the pale for this lab, that's why I
  # have taken special care to try an move slowly through the concepts and 
  # materials I am presenting and stay focused on the fundementals. Moreover, my 
  # favorite science writing takes complex subjects and makes them accessible to 
  # anyone.  That is my goal when writing, please let me know if I succeeded.
  
  # To that end, and inspired by online courses I have taken, I have interjected
  # quick quiz questions after explaining certain concepts.  Don't worry, these
  # quizes are not meant to test you, they are meant to test me.  If I have done
  # a good job explaining the concepts, the quizes should be easy.  If they are
  # not easy, it is because I have failed in my duty.  When we come to a quiz, 
  # please jot your answer on a piece of scratch paper. I'll show the true answer and,
  # if it is not painfully clear how it was derived, it will give us something to
  # talk about.  My hope is that we all walk away from this presentation having
  # learned something useful.
  
  # Quiz!
    # Why did Ben add quizes to this document?
      # a.) Because he's a masochist.
      # b.) Because he's a sadist.
      # c.) Because he wants to check if he's done a good job relating
          # concepts to the reader.
          
      # Answer:
      	# c("a", "b", "c")[198 %% 7 %% 3 + 1]
  


# Motivation

  # Imagine that you were a decision maker following the Fukushima disaster.
  # You need to decide how many people to evacuate.  Clearly, you would call 
  # on the world's experts and ask them to determine the risk.

  # You would hope that the expert could use all of their knowledge from
  # A lifetime of observing the effects of radiation and use it to estimate 
  # the potential loss of human lifespan and increase in disease owing to
  # radiation.

  # I want my radiation model to play the role of this expert.  I want to develop
  # a model of the health effects of ionizing radiation that can be generalized 
  # to new situations and used to accurately predict the health consequences of 
  # radiation exposure.

  # I think this ideal is a useful one to pursue when developing a statistical
  # model.  I am going to use the word model a good deal in this talk, all it 
  # means is a representation of reality.  A model of the effects of radiation
  # should allow us to look at an an animal and the way that it was exposed and
  # determine how its lifespan and disease patterns will be affected by the 
  # exposure it received.
  

# Justification

  # Clearly, I am not the first one to try and build useful models of the health
  # effects of radiation exposure.  Why should I suspect that I can do any better
  # than previous modeling efforts?

  # There are several reasons.  One, is that the field of statistics and machine 
  # learning is moving at a breakneck speed due the increasing power of computers
  # and sudden availability of massive important datasets.  Some of the models 
  # that I will employ, like gradient boosted machines, have only been recently 
  # invented, yet regularly outperform other methods in machine learning 
  # competitions.  Moreover, many of these statistical methods have never been 
  # employed in radiation health models.

  # A second reason this work might provide novel new models is that I am
  # employing data from multiple species.  Some authors, though not many,
  # have tried to determine if the effects of radiation exposure are comparable
  # between species.  There is some evidence that they are.  If this is true
  # then we may be able to receive a boost of additional predictive force by 
  # using data from multiple species.

  # The other reason this work is potentially novel, is that the concept of
  # generelizability is a rising tide in statistics and machine learning.  To 
  # measure how generalizable a concept is requires a good deal of costly 
  # computation and large datasets to prove.  However, it does not require the
  # user to make some of the dangerous assumptions that traditional statistics 
  # does.  And therefore it is more robust and harder to game.  It will help
  # to start with an example.


# What is generalizability?

  # Generalizability refers to how well a model performs on new data.  Take for
  # instance a series of coin flips.  Say we saw the pattern HHHT and we wanted 
  # to determine how likely heads and tails were.

  # The traditional statistical approach would start with a hypothesis.  For 
  # instance we might guess that the coin has a 50%/50% chance of being heads or 
  # tails.  Then we would ask how likely the observed outcome is, given our 
  # hypothesis.  
  
  # In this case, there are 4 combinations of one tail and three heads
  # and 16 total combinations.  So the odds that we would see one heads
  # and three tails is about 25% if the coin is fair, a 'p value' of 0.25.  
  # Arbitrarily, we say that p values above 0.05 are 'not statistically 
  # significant'.  So in this case we conclude that the coin could be fair.

  # I've never been totally happy with this approach.  It works well enough
  # with coins, in an idealized world, but the 0.05 cutoff is problematic.  It is 
  # all too easy to run hundreds of tests and by chance 5% of them are significant.
  # Many papers publish results that were generated in this way and they 
  # really mean nothing.

  # An alternative approach is to ask how well our model performs after training 
  # it on a subset of the data.  For example, if we look at the first three 
  # flips, HHH, we might develop the model that the coin is always heads.  This is a 
  # good fit for the data we are looking at. We predict the flip correctly 100% 
  # of the time.  However, this model fails miserably when we ask it to predict 
  # the value of the coin we left out.  It will predict heads and it will be 100% 
  # wrong.

  # We can continue in this fashion leaving out different coins.  Next we might
  # leave out the 3rd coin so we are looking at the pattern HHT.  A reasonable
  # model might predict that the coin is heads 66% of the time.  This model is 66%
  # correct for the first coin, 66% correct for the second coin and only 33% correct
  # for the 4th coin (because it is tails).  So on average it is 55% correct.  It
  # also does well on the coin we left out.  It predicts heads with 66% accuracy.

  # Quiz!
    # What will the flip pattern look like when we leave out coin #2?
      # a.) THH, H left out
      # b.) HTT, T left out
      # c.) HHT, H left out

    # Answer:
      # c("a", "b", "c")[219 %% 7 %% 3 + 1]

    # What will our model predict is the likelyhood of heads?
      # a.) 66% probability of heads just like before.
      # b.) 0% probability of heads like the first time.
      # c.) its impossible to know.

    # Answer: 
      # c("a", "b", "c")[314 %% 7 %% 3 + 1]

    # How correct will this model be on the data it was trained on?
      # a.) 45% accurate.
      # b.) 55% accurate.
      # c.) 65% accurate.

    # Answer:
      # c("a", "b", "c")[102 %% 7 %% 3 + 1]
  
    # And how correct will we be when we measure this new model against the left 
    # out coin?
      # a.) 66% correct just like before.
      # b.) 100% false like the first time.
      # c.) its impossible to know.

    # Answer: c("a", "b", "c")[315 %% 7 %% 3 + 1]

  # The last left out coin will perform again exactly the same.  All told,
  # we generate the following results:

         # % correct
      # left-in    left-out   coins (left-out)
      # 100%       0%         HHH(T)
      # 55%        66%        HH(H)T
      # 55%        66%        H(H)HT
      # 55%        66%        (H)HHT

 # Avg. 66%        50%

 # These final numbers are the most interesting.  Our approach performed 66% correct
 # on average using the data that it was trained on.  But only 50% correct on average
 # on the left out data.  That means that we didn't do any better than pure dumb chance!
 # We really have very little to learn from this coin without giving it some extra
 # flips.  The performance on the left out data gives us a sort of validation that our
 # model is not too optimistic.  It provides a safety net to protect us from the chance
 # associations that are common in many papers.

 # The coin example is pretty simple to illustrate the point.  But you have just
 # learned a very powerful statistical concept called cross-validation.  The advantage 
 # it has over traditional statistics it gives us an estimate of how good our statistical 
 # approach is and it tells us how over-optimistic our results are likely to be.  That is, 
 # the 66% number is over-optimistic and the true performance of the model is about 
 # 50% / 50%.

 # So, I will be measuring generalization to test how well my model's perform.
 # I will be trying to predict the lifespan of an animal following radiation 
 # exposure.  I will measure the difference between the actual lifespan and the
 # predicted lifespan to determine how well my model is performing.  And I will
 # compare the performance on the data the model has seen to the data the model
 # has not seen to see if my model is accurately describing the world.

 # Quiz!

   # Which model would you use to predict lifespan following radiation exposure?
     # a.) +/- 30 days on left-in (training) data and +/- 90 days on 
         # left-out (validation) data
     # b.) +/- 60 days on left-in (training) data and +/- 80 days on 
         # left-out (validation) data
     # c.) +/- 100 days on left-in (training) data and +/- 70 days on 
         # left-out (validation) data

   # Answer:
     # c("a", "b", "c")[414 %% 7 %% 3 + 1]

# Intro Conclusion
  # Enough of this introductory material.  Its time to load our data!

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
		"gbm"								# kickin' ass
	))
	
	#
	# define acceptable data
	#     column.name=c(acceptable row values),
	#
	
	acceptable <- list(                        # 10683 excluded total
	    cause_of_death=c(       
	    	"", 							   # beagles have no cause of death
			"Died", "Sacrifice, moribund"      # only natural deaths (6776 excluded)
		),
	
		experiment=c(                          # Exclude 3184 breeders and 
	                                           # injection studies and
	                                           # any incomplete dog studies (most)                         
			"2", "3", "4", "7", "8", "9"
			#, "10"                            # Exclude peromyscus
			, "11", "12", "13", "14"           
			#, "Acute Exposure and Reticuloendothelial System"
			#, "Gamma-Irradiation during Pregnancy"
			#, "Gamma-Irradiation until Death"
			#, "Hematological Changes"
			#, "Leukemogenesis: Whole Life Gamma-Irradiation"
			, "Life-Span: Varioious continous Gamma-Irradiations"
			, "Life-Span: Whole Life Gamma-Irradiation"
			#, "Lithium and Potassium Study"
			#, "RadioIodine Effects on Thyroid and Adrenal Gland"
		),
		species=c(                             # 723 are labled blank
			"beagle", "musculus"               #, "peromyscus"
		),
		sex=c("M", "F")                        # Remove the 25 un-gendered animals 
	                                           # because they mess up function calls
	)
	
	#
	# Validation and test sets
	#
	
	validate.percent    <- 0.2
	test.percent        <- 0.2
	
	# 
	# Column types
	#
	factors <- c( 
		"cause_of_death", "sex", "species", "experiment", "data_set", "set", "group"
	) 
	
	#
	# Add a few little helper scripts that will only be useful in our context
	#
	
	train   <- function(){data[data$set == 'train',]}   # the cv subsets
	val     <- function(){data[data$set == 'val',]}
	test    <- function(){data[data$set == 'test',]}
	
	beagle  <- function(){data[data$species == 'beagle',]}    # various species
	mouse   <- function(){data[data$species == 'musculus',]} 
	
	plot_all <- function(data, x=NULL, y=NULL) {
		
		if(! is.null(y)){ data$p <- y }            # Allow the user to override x and y
		if(! is.null(x)){ data$age_days <- x }
		
		data$sieverts <- 0.01 * data$cgy_total_gamma + 10*0.01*data$cgy_total_neutron
		data$sieverts_rate <- 0.01 * data$cgy_per_min_gamma + 10*0.01*data$cgy_per_min_neutron
		
		min_e        <- min(data$first_exposure_age_days) - 2
		max_e        <- max(data$first_exposure_age_days) + 2
		cuts         <- c(min_e, 50, 150, max_e)
		labels       <- c("< 50 days", "50 to 150 days", "> 150 days")
		data$exposure_age <- cut(data$first_exposure_age_days, cuts, labels=labels)
	
		ggplot() + 
			geom_jitter(
				data = data,
				aes(y=p, x=age_days, shape=exposure_age, color=log10(sieverts),
				size=log(sieverts_rate + 1), alpha=1/log10(sieverts_rate + 1.1))) +
			facet_wrap(~ species, scales="free", ncol=1)	+ scale_colour_gradient2() 
	
	
	}
	#plot_all(val())
	
	plot_p <- function(data) {
		ggplot() + 
			geom_jitter(data = data, aes(y=p, x=age_days), size=0.5 )+
			facet_wrap(~ species, scales="free", ncol=1)
	}
	# plot_p(val())
	
	perform$show.by.sp <- function(){
		# Overall
		print(perform$show(data$age_days, data$p, data$set, perform$r2))
		
		# And by species
		write.table(daply(data, .(species), function(df){
	    	perform$show(df$age_days, df$p, df$set, perform$r2)
	    }))
	}
	# perform$show.by.sp()
	
	perform$get.by <- function(
		outcome=data$age_days, prediction=data$p, 
		species=data$species, group=data$group, experiment=data$experiment,
		set=data$set, measure=perform$r2,
		by.sp=FALSE, by.exp=FALSE, by.group=FALSE){
		
		data <- data.frame(o=outcome, p=prediction, sp=species, 
		                   group=group, exp=experiment, set=set)
				
		# Overall
		result <- perform$get(data$o, data$p, data$set, measure)
		
		# By species
		if(by.sp){
			result <- ddply(data, .(sp), function(df){
		    	perform$get(df$o, df$p, df$set, measure)
		    })
		}
		
		# By experiment
		if(by.exp){
			result <- ddply(data, .(exp), function(df){
		    	perform$get(df$o, df$p, df$set, measure)
		    })	
		}
		
		# By group
		if(by.group){
			result <- ddply(data, .(group), function(df){
		    	perform$get(df$o, df$p, df$set, measure)
		    })
		}
		
		result		
	}	
	# perform$get.by(by.sp=FALSE, by.group=TRUE, by.exp=FALSE)

	
	# get a model summary as a character string
	print.summary <- function(model){
		paste(capture.output(summary(model)), collapse="\r\n")
	}
	# cat(print.summary(model))
	
	#
	# Also save summaries of the models
	#
	summaries <- list()



### preprocessing ############################################################
#
	data <- data$load(from_cache=TRUE)             # load janus data
	                                               
	data <- select$by_acceptable(data, acceptable) # Remove unacceptable animals
	                                               
	set.seed(69)                                   # Make a column that divides
	data <- cv$subset(data, validate.percent,      # data into test, training, and
		                    test.percent)          # validation sets.  Make it stable
	                                               # between runs by fixing the random 
	                                               # seed.
	 
	data$p       <- 0                              # Set up predictions column
	
	for(f in factors){                             # Convert data types where needed 
		data[[f]] <- factor(data[[f]])
	}

	  

##############################################################################
#
# Data Overview
#
#   We have 41K animals total.  Here's what a typical record looks like:
#
#   $ sex                    : "M" 
#   $ species                : "musculus"
#   $ age_days               : 818
#   $ experiment             : 4
#   $ group                  : L5
#   $ first_exposure_age_days: 131
#   $ cgy_per_min_gamma      : 0
#   $ cgy_per_min_neutron    : 0
#   $ cgy_total_gamma        : 0
#   $ cgy_total_neutron      : 0
#   $ fractions_gamma        : 0
#   $ fractions_neutron      : 0
#   $ set                    : "test"
#   $ p                      : 966.7812
#
#   You can see that I've restricted the data to a pretty limited set.  We have
#   a little demographic information and dosimetry information on each animal. 
#
#   We also have a few helper fields at the end to help our analysis.  'p' is a 
#   placeholder for predicted age according to our model and 'set' divides our data 
#   into training validation and test sets which will be used to measure generalization 
#   as discussed before.  I didn't mention the test set before and you don't need 
#   to worry about it, its just an extra set for validating the final result that 
#   we achieve.
#
#   Here is a little more information on the important fields.
#
#   $ sex                    : "F" (20269), "M" (20779)
#   $ species                : "beagle" (624), "musculus" (40424)
#   $ age_days               :   0%  25%  50%  75% 100% 
#                               106  746  899 1040 5656 
#   $ experiment             : 11 experiments (two dogs, rest mice)
#   $ first_exposure_age_days:   0%  25%  50%  75% 100% 
#                                93  108  114  117  817 
#   $ cgy_per_min_gamma      :        0%       25%       50%       75%      100% 
#                               0.000000  0.000000  0.000000  0.370222 37.785000 

#   $ cgy_per_min_neutron    :      0%     25%     50%     75%    100% 
#                               0.0000  0.0000  0.0000  0.1175 11.3040 

#   $ cgy_total_gamma        :    0%   25%   50%   75%  100% 
#                                  0     0     0   206 14745 

#   $ cgy_total_neutron      :     0%    25%    50%    75%   100% 
#                                0.00   0.00   0.00  18.84 323.79 

#   $ fractions_gamma        :   0%  25%  50%  75% 100% 
#                                0    0    0    1  300 

#   $ fractions_neutron      :   0%  25%  50%  75% 100% 
#                                 0    0    0    1  180

#   $ set                    : "test" (8352), "train" (24416), "val" (8280)
#
#   Some things that are worth taking note of:  We have close to an even balance
#   of males and females, though some of the experiments involved only male mice.
#   There are 2 species in the dataset, and the Mouse is by far the most common.  
#   The beagle is an ordinary beagle dog.
#
#   The animals had a wide range of lifespans.  Dogs lived much longer than either
#   of the rodent species, usually around 3000 days.  Mus musculus lived about 1000 
#   days.
#
#   The data is divided into 19 experiments which cover a wide range of treatment
#   conditions.  Doses of 14 Gy gamma or 3 Gy neutron would be quickly lethal, but
#   most of the doses were in a sub-lethal range that would induce long-term toxicities
#   like cancer.  Also, some of these exposures were fractionated and some were not.
#
#   Finally, you can see that the dataset has been split so that about 60% of the
#   animals serve as a training set (left-in) and the other 40% are validation groups 
#   that will be used to explore how these models perform on data they have not yet
#   encountered and hence how well they generalize.
#
#   You might ask why I chose such a limited dataset, for many of the animals
#   there is much more information available, like which toxicities they had and 
#   details of their treatment conditions.
#
#   My reasons are two-fold.  For one, I wanted to keep this analysis simple, 
#   because it is meant to serve as a prototype for an extended analysis.  For 
#   another, I wanted to use information that would be relevant and available
#   to someone who was trying to estimate the health risk of an exposure.  Most
#   of the pathologies can only be determined after an animal dies.  Its not very
#   useful to estimate the lifespan of a dead animal!
#

### analysis #################################################################
#
                                               
#
# A simple model (prototype for a prototype)
#
#   Now that we have the introduction and data characterization out of the way, its time
#   to actually build some models and see how they perform.
#
#   This first model is not a serious attempt. Rather I am just showing
#   how a model is constructed and what it tells us about the training and
#   cross validation sets.
#
#   First, I build a list of parameters that I want to use to build a model.
#   I will use most of the information in the data to predict the age of death.
#   Don't worry about how the model is built. I'm happy to talk about it, but 
#   I don't want to get dragged down by tedious statistics.  For now, it is enough
#   to know that the model is built to optimize its performance on the training
#   set and then used to predict the outcomes in both the training and validation
#   sets.

	model.factors <- c(
		'sex', 'species', 'first_exposure_age_days', 'cgy_per_min_gamma', 
		'cgy_per_min_neutron', 'cgy_total_gamma', 'cgy_total_neutron', 
		'fractions_gamma', 'fractions_neutron'
	)
	formula <- f.builder$get_formula("age_days", model.factors)
	model   <- glm(formula, data=train())
	data$p  <- predict(model, data)
	
#   Archive the results
	data$p.lin <- data$p
	summaries <- c(summaries, lin=print.summary(model))

#   Next we measure the performance
	perform$show.by.sp()
	
#     
#   "           performance  0.515  overfit by -0.0121"
#
#   "beagle"   "performance -0.0123 overfit by -0.0128"
#   "musculus" "performance  0.251  overfit by -0.0042"
#
# The performance function
#
#   It is worth talking about this performance function.  What we are measuring here is the 
#   r2 value.  Basically the % of variance explained by the data.  A bad model that
#   just guesses the average will have a low r2 value, 0.  A perfect model that correctly
#   predicts every outcome will have an r2 value of 1.
#
#   Quiz!
#     Which model is the best?
#       a.) A model with an r2 of 0.7
#       b.) A model with an r2 of -0.2
#       c.) A model with an r2 of 0.5
#
#     Answer:
#       c("a", "b", "c")[769 %% 7 %% 3 + 1]
#
#   In this case we achieved an r2 value of 0.269 or 27%.  This means we explained about
#   27% variance or we are about 27% of the way to a perfect model.
#
#   This number, 0.269, comes from the validation set (the left-out set).  The over-fit
#   value shows how much better the model performed on the training example, for which
#   it was optimized.  In our case it performed marginally worse on training set.  This 
#   indicates that our result is generalizable, that is, it performs equally well on 
#   left-in and left-out data.
#

#   Next plot the results:

    plot_p(val())

# Prediction vs Age
#
#   The performance function gives us a single number to look at, but it is helpful to graph
#   the predictions to get a sense of where the model is correct and where it is misleading.
#
#   The y-axis represents the age that our model predicted while the x axis represents how
#   long the animal actually lived.
#
#   Quiz!
#     Look at the graph to find the age of the oldest mouse and how old it was predicted to be.
#       a.) The oldest mouse lived to ~1400 days and was predicted to live ~1125 days.
#       b.) The oldest mouse lived to ~1450 days and was predicted to live ~950 days.
#       c.) The oldest mouse lived to ~950 days and was predicted to live ~1450 days.
#
#     Answer:
#       c("a", "b", "c")[456 %% 7 %% 3 + 1]
#
#   It is helpful to look at the general shape of these prediction graphs.  Using a perfect
#   model, the predicted age is exactly the same as the observed age.  So the points should
#   line up on a diagonal line.  These fits are not perfect, but their trapezoidal shape 
#   approximates this diagonal line, and confirms that the predictions are partially aligned
#   with the observed ages.
# 
#   Quiz!
#     Which species do you think had the best fit based on how 'trapezoidal' their
#     prediction fit looks.
#       a.) beagle
#       b.) musculus
#
#   Answer:
#     c("a", "b", "c")[981 %% 7 %% 3 + 1]
#
#   It is also useful to look carefully at these prediction graphs to find extreme outliers.
#   For example the beagle chart includes a stray dog that was predicted to live more than
#   3000 days.  Why is this dog so far removed from the rest?  We can look at a more informative
#   graph to get a clue.

    plot_all(val())

# Information Chart
#
#   This chart is intense, but very helpful, because it represents many of the parameters of
#   the model.  So as not to get overwhelmed, let's focus first on the stray dog with the high
#   prediction.  You will notice that this dog is now represented by a large faded purple 
#   square.  
#
#   The size of the square, and its transparency indicate that it received a high dose rate.
#   Sieverts are used to measure dose in this chart, because they combine both gamma and neutron
#   irradiation.  The color of the square indicates its total dose in sieverts which was in the 
#   upper half of the range.  The shape of the point, a square, indicates that the animal
#   was first exposed to irradiation after 150 days of age which puts it on the late end
#   of the spectrum.
#
#   By comparing this point to the others we can observe that it received a higher dose rate
#   than most.  Is is possible that very high dose rates correlate with longer life!?!  
#   No, in fact, if you look at the underlying animal data, the reason this animal is exceptional
#   is because it was not irradiated until it was already over 3000 days old.  It was easy
#   for the model to pick up that it would be long-lived, because it was already so old when 
#   it was first blasted.
#
#   And this brings me to one of my major challenges in analyzing this data, how to
#   account for the bias of animals that weren't irradiated until they were older.  Most of
#   the animals in this study were over 100 days old when they were first irradiated, which
#   means that many younger animals had died and never had a chance to make it into the study.
#   This is a clear source of bias that needs to be accounted for.  I am still thinking about
#   how to do that.  If you have any ideas, I am all ears.
#
#   Anyways, I am going to leave the information chart behind for now and look at each species
#   on its own.


# Performance by study
#
#   Our ultimate goal is to accurately predict the results of a brand new experiment.
#   Therefore it is important that we build a model using data from all of the other 
#   experiments and check its performance on the left out experiment.


	# Build models
	m <- 
	dlply(data, .(experiment), function(df){
		
		formula       <- f.builder$get_formula("age_days", model.factors)
		subset        <- (data$set == 'train') & (data$experiment != df$experiment[1])
		model         <- glm(formula, data = data[subset,])
		
		model
	})
	
	# Make predictions	
	data <- ddply(data, .(experiment), function(df){
		df$p <- predict(m[[df$experiment[1]]], df)
		
		df
	})

	# Summarize
	s <- laply(m, function(model){print.summary(model)})
	names(s) <- paste("lin.by.exp", names(s), sep=".")
	summaries <- c(summaries, s)
	
    # Archive the results
	data$p.lin.by.exp <- data$p
	
	perform$show.by.sp()

#   overall               "performance 0.446 overfit by -0.0106"
#
#   "beagle"              "performance -0.174   over-fit by  -0.0122"
#   "musculus"            "performance  0.158   over-fit by  -0.00724"

	# plotting
	plot_all(val())

# Performance by species
#
#   One basic question I want to address is whether using multiple species in the modeling
#   process improves its overall performance.  For example, does the mouse data tell give us
#   extra information about the response of the dogs.
#
#   Intuitively this seems plausible.  Imagine there are two scientists, Gayle and Bruce, 
#   both of whom know a little about the health effects of radiation in dogs. However, Gayle
#   knows a lot about radiation in mice and Bruce knows nothing.  Its reasonable to think that
#   Gayle is more qualified to talk about radiation in dogs because she can leverage some of 
#   her knowledge about mice.
#
#   This same effect might emerge when building statistical models of the data. However, as 
#   the coming results will show, it is not easy.  Here we show the performance
#   of the model we just tested on each species separately.
#
	write.table(daply(data, .(species), function(df){
		perform$show(df$age_days, df$p, df$set, perform$r2)
	}))
#
#   "beagle"              "performance 0.0147 over-fit by  0.00978"
#   "musculus"            "performance 0.199  over-fit by  0.00862"
#

# Performance by species
#
#   This performance is pretty awful.  The mouse is the only animal who is well
#   fit at all.  This is probably because it has so many more data points that it swayed
#   the model in its direction like a hefty fellow on a teeter-totter.  
#
#   If you are observant you might notice that all of the r2 values are lower than 
#   the r2 we got in the last step.  This is because the r2 value compares the predictions
#   to the averages and because we have split by species we are now using the species average
#   rather than the average of the entire dataset.  So, the average age of the species
#   is nearly as good a predictor of the animals lifespan as the model we just built.  This 
#   is not a good sign for our model.
#
#   Quiz!
#     Which of the following r2 values is impossible?
#       a.) 0.45 overall and (0.5, 0.45, 0.6) by species
#       b.) 0.45 overall and (0.2, 0.4, 0.6) by species
#       c.) 0.45 overall and (0.1, 0.3, 0.4) by species
#
#   Answer:
#     c("a", "b", "c")[1063 %% 7 %% 3 + 1]
#
#   From these r2 values we can infer that our performance will probably improve
#   if we build a model for each species seperately.  We test that inference next.
#

	# Build models
	m <- 
	dlply(data, .(species), function(df){
		
		model.factors <- model.factors[! model.factors %in% c("species")]
		formula       <- f.builder$get_formula("age_days", model.factors)
		model         <- glm( formula, data = df[df$set == 'train',])
		
		model
	})
	
	# Make predictions	
	data <- ddply(data, .(species), function(df){
		df$p <- predict(m[[df$species[1]]], df)
		
		df
	})

	# Summarize
	s <- laply(m, function(model){print.summary(model)})
	names(s) <- paste("lin.by.sp", names(s), sep=".")
	summaries <- c(summaries, s)
	
    # Archive the results
	data$p.lin.by.sp <- data$p
	
    perform$show.by.sp()

#                         "performance 0.552 overfit  by   0.0262"
#
#   "beagle"              "performance 0.134 over-fit by   0.133"
#   "musculus"            "performance 0.256 over-fit by   0.00159"
#
#   This is a pretty substantial improvement.  Each species is modeled between 2% and 7%
#   better than before.  It seems that our previous model was doing a lot of work to 
#   compromise the effect between species.  If we plot the result we can see that they
#   look more 'trapezoidy' indicating that the fit is nicer.

    plot_p(val())

#   At this point it is clear that this model will not benefit from the inclusion
#   of multiple species.  We need a more sophisticated set of models which is what we
#   develop next.


# Linear by species and experiment

	# Build models
	m <- 
	dlply(data, .(species), function(df.s){
		dlply(df.s, .(experiment), function(df){
			model.factors <- model.factors[! model.factors %in% c("species")]
			formula       <- f.builder$get_formula("age_days", model.factors)
			subset        <-(
			                 as.character(data$experiment) != as.character(df$experiment[1])
			                 & data$species == df.s$species[1]
			                 & data$set != "test"
			                 )
			
			print(sum(subset))
			model         <- glm(formula, data = data[subset,])
			
			model
		})	
	})
	m <- c(m[[1]], m[[2]])
	
	# Make predictions	
	data <- ddply(data, .(experiment), function(df){
		df$p <- predict(m[[df$experiment[1]]], df)
		df
	})

	# Summarize
	s <- llply(m, function(model){print.summary(model)})
	names(s) <- paste("lin.by.sp.by.exp", names(s), sep=".")
	summaries <- c(summaries, s)
	
    # Archive the results
	data$p.lin.by.sp.by.exp <- data$p
	
	# Show performance
	perform$show.by.sp()


#                         "performance -87.3  overfit by   13.8"
#
#   "beagle"              "performance -2.1  over-fit by   0.0394"
#   "musculus"            "performance -279  over-fit by   46.8"

# More attrocious behavior when the species are divided.

#
# Interactions model
#
#   The next model does more to allow species to act independently by separating radiation 
#   factors for each species in addition to the total dose that they all share in common.
#   It also includes factors like the total dose squared which might be important if the 
#   relationship between dose and effect is non-linear.
#
#   As before, we will build and run the model without focusing on the statistical details.
#   Instead we will concentrate on the predictions and performance of the results.

    # Build up the formula
    inner.factors <- model.factors[! model.factors %in% c("species", "sex" )]
    outer         <- f.builder$get_outer_interactions(model.factors)
    inner         <- f.builder$get_self_interactions(inner.factors)
    formula       <- f.builder$get_formula("age_days", c(model.factors, outer, inner))

    model         <- glm(formula, data = data[data$set == 'train',])
    data$p        <- predict(model, data)
    
    # archive the results
	data$p.lin.inter <- data$p
	summaries <- c(summaries, lin=print.summary(model))

	perform$show.by.sp()
#
#                         "performance 0.533  overfit by   0.0655"
#
#   "beagle"              "performance 0.0239 over-fit by  0.286"
#   "musculus"            "performance 0.281  over-fit by  0.00114"
#
#    The interactions model performs slightly better than what came before, but not
#    much.  It appears that separating by species already delivered most of the value.
#    But what happens if we model each species independently using this interaction model?
#

	# Make models
	m <- 
	dlply(data, .(species), function(df){
		
		model.factors <- model.factors[! model.factors %in% c("species")]
		outer.factors <- model.factors[! model.factors %in% c("experiment" , "sex")]
		inner.factors <- model.factors[! model.factors %in% c("experiment", "species", "sex")]
		
		outer         <- f.builder$get_outer_interactions(outer.factors)
		inner         <- f.builder$get_self_interactions(inner.factors)
		model.factors <- c(model.factors, outer, inner)
		
		formula       <- f.builder$get_formula("age_days", model.factors)
	
		model         <- glm( formula, data = df[df$set == 'train',])
		
		model
	})
	
	# Make predictions	
	data <- 
	ddply(data, .(species), function(df){
		df$p <- predict(m[[df$species[1]]], df)
		
		df
	})
	
	# archive results
	data$p.lin.inter.by.sp <- data$p	
	s                      <- laply(m, function(model){print.summary(model)})
	names(s)               <- paste("lin.inter.by.sp", names(s), sep=".")
	summaries              <- c(summaries, s)
		
	# show results
	plot_all(val())
	perform$show.by.sp()

#
#   Overall               "performance 0.585 overfit by    0.0247"
#
#   "beagle"              "performance 0.233  over-fit by  0.123"
#   "musculus"            "performance 0.282  over-fit by  0.000557"
#
#   We continue to improve.  This means that
#   there were portions of this model which, even when squared, were still species dependent.
#   The first exposure age squared and the gamma exposure squared were both reasonably
#   significant.  It is likely that these had different rates between species, and this 
#   might help to explain the improvement.
#
    plot_all(val())

#
#   This improved model has exposed new oddities.  For instance, there is a class of mice
#   with an exceptionally high prediction score, above even the control mice.  It turns out
#   that all of these mice share a high number of fractions.  It seems likely that only 
#   long-lived mice had the opportunity to receive so many fractions.  So there is some clear
#   bias that makes the data rather deceiving.
    
    
# Interactions by Experiment
#
#   Now it is time to look at how well these models do when tasked with predicting
#   experiments which they did not have a chance to observe.


	# Build models
	m <- 
	dlply(data, .(experiment), function(df){
		
    	formula       <- f.builder$get_formula("age_days", c(model.factors, outer, inner))
		subset        <-(data$experiment != df$experiment[1] & data$set != "test")
		model         <- glm(formula, data = data[subset,])
		
		model
	})
	
	# Make predictions	
	data <- ddply(data, .(experiment), function(df){
		df$p <- predict(m[[df$experiment[1]]], df)
		df
	})

	# Summarize
	s <- laply(m, function(model){print.summary(model)})
	names(s) <- paste("lin.inter.by.exp", names(s), sep=".")
	summaries <- c(summaries, s)
	
    # Archive the results
	data$p.lin.inter.by.exp <- data$p
	
	perform$show.by.sp()

	# [1] "performance -0.904 overfit by 0.797"

#   This result shows an intense failing of the interactions model.  It is
#   overfitting with respect to experiment.  When the test data was cut from multiple
#   experiments it did just fine, now it is falling to peices.
#
#   What happens when we try seperating by species?


	# Build models
	m <- 
	dlply(data, .(species), function(df.s){
		dlply(df.s, .(experiment), function(df){
			
			model.factors <- model.factors[! model.factors %in% c("species")]
			outer.factors <- model.factors[! model.factors %in% c("experiment" , "sex")]
			inner.factors <- model.factors[! model.factors %in% c("experiment", "species", "sex")]
	
			outer         <- f.builder$get_outer_interactions(outer.factors)
			inner         <- f.builder$get_self_interactions(inner.factors)
			model.factors <- c(model.factors, outer, inner)
			
	    	formula       <- f.builder$get_formula("age_days", c(model.factors, outer, inner))
			subset        <-(
			                 as.character(data$experiment) != as.character(df$experiment[1])
			                 & data$species == df.s$species[1]
			                 & data$set != "test"
			                 )
			
			model         <- glm(formula, data = data[subset,])
			
			model
		})	
	})
	m <- c(m[[1]], m[[2]])
	
	# Make predictions	
	data <- ddply(data, .(experiment), function(df){
		df$p <- predict(m[[df$experiment[1]]], df)
		df
	})

	# Summarize
	s <- llply(m, function(model){print.summary(model)})
	names(s) <- paste("lin.inter.by.sp.by.exp", names(s), sep=".")
	summaries <- c(summaries, s)
	
    # Archive the results
	data$p.lin.inter.by.sp.by.exp <- data$p
	
	# Show performance
	perform$show.by.sp()
    
#   Overall               "performance -53235 overfit by   11158"
#
#   Absurdly aweful performance!


#
# GBM
#   
#   Finally we pull out the biggest gun in our modeling arsenal.  GBM.  At this point
#   we already have identified some underlying problems that we need to fix before we
#   revisit this script again.  But just to show what a strong modeling technique this 
#   is we will run it.


	# build models
	set.seed(69)                          # stabalize results
	data <- data[sample(nrow(data)),]     # gbm requires that data is shuffled
	
	trees <- 500
	model   <- gbm(
		formula      = as.formula(f.builder$get_formula("age_days", model.factors)),
		distribution = "gaussian",
		data         = train(),
		n.trees      = trees,
		interaction.depth = 4,
		n.minobsinnode = 5,
		shrinkage    = 0.1,
		cv.folds     = 5,
		verbose      = TRUE
	)
	
	# Make predictions
	best.iter <- gbm.perf(model, method="cv")
	data$p    <- predict(model, data, best.iter)
	
	# Archive results
	data$p.gbm <- data$p
	summaries  <- c(summaries, gbm=print.summary(model))
	
	# Print results	
	plot_all(val())
	perform$show.by.sp()
	
#   Overall               "performance 0.652 overfit by    0.0675"
#
#   "beagle"              "performance 0.489  over-fit by  0.284"
#   "musculus"            "performance 0.293  over-fit by  0.00276"
#
#   We are gettings a really shocking improvement in the beagle scores.
#   This gives us another thing to look into.  The linear models have a hard
#   time capturing local varaitions, like study to study, so we may be seeing
#   an effect of some sort of study by study bias that we need to work harder
#   to eliminate.
#
#   If we train our models species by species
#

	# build models
	set.seed(69)                          # stabalize results
	data <- data[sample(nrow(data)),]     # gbm requires that data is shuffled

	m <- 
	dlply(data, .(species), function(df){
		
		trees <- 500
		model   <- gbm(
			formula = as.formula(f.builder$get_formula("age_days", model.factors)),
			distribution = "gaussian",
			data = df[df$set == 'train',],
			n.trees = trees,
			interaction.depth = 4,
			n.minobsinnode = 5,
			shrinkage = 0.1,
			cv.folds = 5,
			verbose = TRUE
		)
		
		model
	})
	
	# Make predictions	
	data <- 
	ddply(data, .(species), function(df){
		model <- m[[df$species[1]]]
		best.iter <- gbm.perf(model, method="cv")
		df$p <- predict(model, df, best.iter)

		df
	})
	
	# archive results
	data$p.gbm.by.sp <- data$p	
	s <- laply(m, function(model){print.summary(model)})
	names(s) <- paste("gbm.by.sp", names(s), sep=".")
	summaries <- c(summaries, s)	
	
	# show results	
	plot_all(val())
	perform$show.by.sp()


#   Overall               "performance 0.649 overfit by    0.0962"
#
#   "beagle"              "performance 0.469  over-fit by  0.385"
#   "musculus"            "performance 0.3    over-fit by  0.0125"
#
#   No real improvements over the species independent model.


# GBM by experiment
#
#     Now its time to see if the GBM results hold up.

	# Build models
	set.seed(69)                          # stabalize results
	data <- data[sample(nrow(data)),]     # gbm requires that data is shuffled

	m <- 
	dlply(data, .(experiment), function(df){
		
		subset        <- (data$experiment != df$experiment[1] & data$set != "test")
		
		# build models
		trees <- 500
		model   <- gbm(
			formula = as.formula(f.builder$get_formula("age_days", model.factors)),
			distribution = "gaussian",
			data = data[subset,],
			n.trees = trees,
			interaction.depth = 4,
			n.minobsinnode = 5,
			shrinkage = 0.1,
			cv.folds = 5,
			verbose = TRUE
		)
		model
	})	
	
	# Make predictions	
	data <- ddply(data, .(experiment), function(df){
		model     <- m[[df$experiment[1]]]
		best.iter <- gbm.perf(model, method="cv")
		df$p      <- predict(model, df, best.iter)
		df
	})

	# Summarize
	s <- laply(m, function(model){print.summary(model)})
	names(s) <- paste("gbm.by.exp", names(s), sep=".")
	summaries <- c(summaries, s)
	
    # Archive the results
	data$p.gbm.by.exp <- data$p
	
	# Show results
	perform$show.by.sp()

    
#   Overall               "performance -0.673 overfit by    0.0759"
#
#   omg, falling down.  Its all due to that damn really low dose exp.
#
	
# GBM by experiment by species

	set.seed(69)                          # stabalize results
	data <- data[sample(nrow(data)),]     # gbm requires that data is shuffled

	m <- 
	dlply(data, .(species), function(df.s){
		df.s$experiment <- factor(df.s$experiment)      # prevent model from running
													    # experiments from the other 
													    # species
		dlply(df.s, .(experiment), function(df){
			
	    	formula       <- f.builder$get_formula("age_days", c(model.factors, outer, inner))

			subset        <-(
			                 as.character(data$experiment) != as.character(df$experiment[1])
			                 & data$species == df.s$species[1]
			                 & data$set != "test"
			                 )
					
			# build models
			trees <- 500
			model   <- gbm(
				formula = as.formula(f.builder$get_formula("age_days", model.factors)),
				distribution = "gaussian",
				data = data[subset,],
				n.trees = trees,
				interaction.depth = 4,
				n.minobsinnode = 5,
				shrinkage = 0.1,
				cv.folds = 5,
				verbose = TRUE
			)
			
			model
		})	
	})
	m <- c(m[[1]], m[[2]])
	
	# Make predictions	
	data <- ddply(data, .(experiment), function(df){
		model     <- m[[df$experiment[1]]]
		best.iter <- gbm.perf(model, method="cv")
		df$p      <- predict(model, df, best.iter)
		df
	})

	# Summarize
	s <- llply(m, function(model){print.summary(model)})
	names(s) <- paste("gbm.by.sp.by.exp", names(s), sep=".")
	summaries <- c(summaries, s)
	
    # Archive the results
	data$p.gbm.by.sp.by.exp <- data$p
	
	perform$show.by.sp()


	# performance -9.3 overfit by -0.023"
	#
	# More awefulness.  We can't generalize!
	
	

# Survival
#     Here I want to use cox regressions to predict lifespan.
#
# Hazards
#
#     I will start by building the simplest model that I can and using it to 
#     derive the baseline hazard function.  This will be useful for all 
#     subsequent models.

	hazard <- basehaz(coxph(Surv(data$age_days) ~ 1, data = data))
	ggplot(hazard) + 
		geom_point(aes(time, hazard))

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
	
	ggplot(my.hazard[1:2000,]) + 
		geom_point(aes(time, hazard), size=0.5, alpha=0.5)
	
	head(my.hazard)
	tail(my.hazard)
	
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
#     Lets take another crack, this time at means.

	haz.mean           <- function(time, hazard) {
		                      n        <- length(time)
		                      dt       <- c(time[2:n] - time[1:n - 1], 1e100)
                              survival <- cumprod(1 - hazard)
                              mean     <- sum((dt * survival)[1:length(dt) - 1])
                              
                              mean
                          }
                          
    haz.mean(time=c(0,2,5), hazard=c(1/4, 1/3, 1)) == 2 
    
    haz.mean(my.hazard$time, my.hazard$hazard); mean(data$age_days)
		
# Simple Cox
#
#     Now lets build a simple species based model and calculate the median age
#     for each species 
#
#     * note:
#         We only use validation predictions because predicting the whole set
#         is slow.

	model        <- coxph(Surv(data$age_days) ~ species, data = data)
	val.predict  <- aaply(model$linear.predictors, 1, function(x){
	                        haz.median(my.hazard$time, my.hazard$hazard * exp(x))
	                    })
	data$p       <- val.predict
	                    
	perform$show.by.sp()


# Cox with prediction
#
#     Lets try actually making a prediction
#

	model        <- coxph(Surv(train()$age_days) ~ species, data = train())
	val.predict  <- aaply(predict(model, data), 1, function(x){
	                        haz.median(my.hazard$time, my.hazard$hazard * exp(x))
	                    })
	data$p       <- val.predict
	                    	                    
	perform$show.by.sp()


# Simple Cox - Stim to stern
#
#     The final step here is to add the species stratification and outcome
#     measurements.  We will add sex this time as species is irrelevant.

	data <- ddply(data, .(species), function(df){
			
		model <- coxph(Surv(df$age_days) ~ sex, data = df)

		df$p  <- aaply(model$linear.predictors, 1, function(x){
	                 haz.median(my.hazard$time, my.hazard$hazard * exp(x))
	             })
		df
	})
	
	# more stabalizers
	perform$show(data$age_days, data$p, data$set, perform$r2)      # "performance 0.000042 overfit by -0.000401"
	                                                         # best 0.621
	plot_p(val())
	plot_all(val())
	write.table(daply(data, .(species), function(df){
		perform$show(df$age_days, df$p, df$set, perform$r2)
	}))
	

# Linear Hazards
#
	formula      <- f.builder$get_formula("Surv(train()$age_days)", model.factors)
	model        <- coxph(formula(formula), data = train())
	val.predict  <- aaply(predict(model, data), 1, function(x){
	                        haz.median(my.hazard$time, my.hazard$hazard * exp(x))
	                    })
	data$p       <- val.predict
	
	# archive results
	data$p.cox <- data$p	
	summaries <- c(summaries, cox=print.summary(model))	

	# show results	
	plot_all(val())
	perform$show.by.sp()     
	
#   Overall               "performance -0.0223 overfit by   0.172"
#
#   "beagle"              "performance -2.16  over-fit by  -0.607"
#   "musculus"            "performance  0.237 over-fit by   0.00308"
#
#   Moderate performance on mouse, very poor on the dog.        	                    
	                    	                    	
# Linear Hazards by species
#

	# Make models	
	m <- 
	dlply(data, .(species), function(df){
		
		survive      <- "Surv(df[df$set == 'train',]$age_days)"
		formula      <- f.builder$get_formula(survive, model.factors)
		model        <- coxph(formula(formula), data = df[df$set == 'train',])
		
		model
	})
	
	# Make predictions	
	data <- 
	ddply(data, .(species), function(df){
		model <- m[[df$species[1]]]
		df$p <- aaply(predict(model, df), 1, function(x){
	                 haz.median(my.hazard$time, my.hazard$hazard * exp(x))
	             })

		df
	})
	
	# archive results
	data$p.cox.by.sp <- data$p	
	s <- laply(m, function(model){print.summary(model)})
	names(s) <- paste("cox.by.sp", names(s), sep=".")
	summaries <- c(summaries, s)	

	# show results	
	plot_all(val())
	perform$show.by.sp()

#   Overall               "performance 0.0877 overfit by    0.00439"
#
#   "beagle"              "performance -1.72  over-fit by   0.0814"
#   "musculus"            "performance  0.241 over-fit by   0.00195"
#
#   Moderate performance on mouse, very poor on the dog. 


# Linear hazards by experiment
#
	# Make models	
	m <- 
	dlply(data, .(experiment), function(df){
		
		subset       <- data$set != 'test' & data$experiment != df$experiment[1]
		survive      <- "Surv(data[subset,]$age_days)"
		formula      <- f.builder$get_formula(survive, model.factors)
		model        <- coxph(formula(formula), data = data[subset,])
		
		model
	})
	
	# Make predictions	
	data <- 
	ddply(data, .(experiment), function(df){
		model <- m[[df$experiment[1]]]
		df$p <- aaply(predict(model, df), 1, function(x){
	                 haz.median(my.hazard$time, my.hazard$hazard * exp(x))
	             })

		df
	})
	
	# archive results
	data$p.cox.by.exp <- data$p	
	s <- laply(m, function(model){print.summary(model)})
	names(s) <- paste("cox.by.exp", names(s), sep=".")
	summaries <- c(summaries, s)	

	# show results	
	plot_all(val())
	perform$show.by.sp()


# Interaction Hazards 
#
#     Lets trying getting a decent performance using the interaction models.

		
	my.hazard <- hazards_table(data$age_days)
		
	formula <- f.builder$get_formula("Surv(data$age_days)", model.factors)
	model <- coxph(formula(formula), data = data)
	
	temp <- cox.zph(model)

	data$p  <- aaply(model$linear.predictors, 1, function(x){
                 haz.median(my.hazard$time, my.hazard$hazard * exp(x))
             })
	
	# more stabalizers
	perform$show(data$age_days, data$p, data$set, perform$r2)      # "performance -0.0503 overfit by 0.0482"
	                                                         # best 0.621
	plot_p(val())
	plot_all(val())
	write.table(daply(data, .(species), function(df){
		perform$show(df$age_days, df$p, df$set, perform$r2)
	}))


#   "beagle"              "performance -0.622 over-fit by  0.0768"     (0.589 best)
#   "musculus"            "performance  0.256 over-fit by  0.0107"     (0.311 best)
#
	
# Interaction Hazards by species
#
#     Lets trying getting a decent performance using the interaction models.


	# build models
	m <- 
	dlply(data, .(species), function(df){
					
		formula <- f.builder$get_formula("Surv(df$age_days)", model.factors)
		model <- coxph(formula(formula), data = df)
		
		model
	})

	# Make predictions	
	data <- 
	ddply(data, .(species), function(df){
		model <- m[[df$species[1]]]
		my.hazard <- hazards_table(df$age_days)

		df$p  <- aaply(model$linear.predictors, 1, function(x){
	                 haz.median(my.hazard$time, my.hazard$hazard * exp(x))
	             })
		df
	})
	
	# archive results
	data$p.cox.inter.by.sp <- data$p	
	s <- laply(m, function(model){paste(capture.output(summary(model)), collapse="\r\n")})
	names(s) <- paste("cox.inter.by.sp", names(s), sep=".")
	summaries <- c(summaries, s)

	# show results
	perform$show(data$age_days, data$p, data$set, perform$r2)      # "performance 0.203 overfit by 0.0116"
	                                                         # best 0.621
	plot_p(val())
	plot_all(val())
	write.table(daply(data, .(species), function(df){
		perform$show(df$age_days, df$p, df$set, perform$r2)
	}))

#   "beagle"              "performance 0.0106 over-fit by  0.0345"     (0.589 best)
#   "musculus"            "performance 0.28   over-fit by  0.0162"     (0.311 best)
#

#    It is interesting to learn that the mean measurement is unstable.  I am forced to use
#    the median.

### Summary ##################################################################
#
#     Lets look at what we have found!

# Performances
#
#     What were all the performances again?

	performs <- llply(predictions, function(p){
		perform$show(data$age_days, p, data$set, perform$r2)	
	})
	
	performs
	
	# here are the most important ones
	
	# $lin.inter.by.sp
	# [1] "performance 0.336 overfit by 0.00434"
	
	# $gbm
	# [1] "performance 0.559 overfit by -0.0574"
	
	# $gbm.by.sp
	# [1] "performance 0.577 overfit by -0.0738"
	
	# $cox.inter.by.sp
	# [1] "performance 0.203 overfit by 0.0116"


# Summaries
#
#    What really mattered

	 l_ply(
	     names(summaries), 
	     function(s){cat('\r\n \r\n \r\n'); cat(s); cat('\r\n \r\n'); cat(summaries[[s]])}
	 )
	 
	# Below we highlight the significant parts of the models.  For gbm, everything above 1% relative influence
	 	 
	# lin.inter.by.sp.beagle
	 
	# Deviance Residuals: 
	    # Min       1Q   Median       3Q      Max  
	# -1923.7  -1491.7   -392.3   1305.8   4569.8  
	# Coefficients: (21 not defined because of singularities)
	                                                       # Estimate Std. Error t value Pr(>|t|)    
	# (Intercept)                                           2.373e+03  5.053e+04   0.047 0.962556    
	
	# I(first_exposure_age_days * first_exposure_age_days) -6.498e-04  2.352e-04  -2.763 0.005845 ** 
	# I(cgy_per_min_gamma * cgy_per_min_gamma)              8.744e-01  2.441e-01   3.582 0.000359 ***
	# I(cgy_total_gamma * cgy_total_gamma)                 -1.505e-05  7.230e-06  -2.081 0.037717 *  
 
  
	# lin.inter.by.sp.musculus
	 
	# Deviance Residuals: 
	    # Min       1Q   Median       3Q      Max  
	# -820.58  -120.58     8.77   131.68   555.03  
	# Coefficients: (9 not defined because of singularities)
	                                                       # Estimate Std. Error t value Pr(>|t|)    
	# (Intercept)                                           9.218e+02  1.139e+01  80.958  < 2e-16 ***
	# sexM                                                  2.512e+01  2.525e+00   9.948  < 2e-16 ***
	# first_exposure_age_days                               4.365e-01  1.166e-01   3.743 0.000182 ***
	# cgy_per_min_gamma                                    -2.496e+00  8.860e-01  -2.817 0.004848 ** 
	# cgy_total_gamma                                      -3.094e-01  2.974e-02 -10.401  < 2e-16 ***
	# cgy_total_neutron                                    -2.038e+00  4.799e-01  -4.247 2.18e-05 ***
	# fractions_gamma                                       1.676e+00  3.797e-01   4.414 1.02e-05 ***
	# fractions_neutron                                    -2.372e+00  1.362e+00  -1.741 0.081730 .  
	# I(first_exposure_age_days * first_exposure_age_days) -7.552e-04  1.778e-04  -4.248 2.17e-05 ***
	# I(cgy_per_min_gamma * cgy_per_min_gamma)              8.591e-01  1.243e-01   6.914 4.83e-12 ***
	# I(cgy_per_min_neutron * cgy_per_min_neutron)         -1.716e+00  7.918e-01  -2.167 0.030221 *  
	# I(cgy_total_gamma * cgy_total_gamma)                  6.674e-05  6.230e-06  10.713  < 2e-16 ***
	# I(cgy_total_neutron * cgy_total_neutron)              4.915e-03  4.956e-04   9.918  < 2e-16 ***
	# I(fractions_gamma * fractions_gamma)                 -1.706e-03  4.990e-04  -3.418 0.000631 ***
	# I(fractions_neutron * fractions_neutron)              1.995e-02  2.337e-03   8.537  < 2e-16 ***
	# first_exposure_age_days:cgy_per_min_gamma             1.219e-02  5.237e-03   2.328 0.019921 *  
	# first_exposure_age_days:fractions_gamma              -9.773e-03  3.176e-03  -3.078 0.002089 ** 
	# cgy_per_min_gamma:cgy_total_gamma                    -4.940e-02  6.280e-03  -7.865 3.84e-15 ***
	# cgy_per_min_gamma:fractions_gamma                     7.701e-01  3.223e-01   2.390 0.016876 *  
	# cgy_per_min_neutron:cgy_total_neutron                 1.090e-01  4.180e-02   2.609 0.009097 ** 
	# cgy_per_min_neutron:fractions_neutron                -5.142e+00  1.742e+00  -2.952 0.003161 ** 
	# cgy_total_gamma:fractions_gamma                       2.540e-04  1.080e-04   2.353 0.018650 *  
	 
	 	 
	# gbm
	 
	                      # var     rel.inf
	# 1         cgy_total_gamma 32.15901907
	# 2 first_exposure_age_days 23.06594351
	# 3       cgy_per_min_gamma 19.42900864
	# 4                 species 19.38667725
	# 5       cgy_total_neutron  2.72459269
	# 6                     sex  2.46659372
	 
	 
	# gbm.by.sp.beagle
	 
	                      # var    rel.inf
	# 1         cgy_total_gamma 39.1733240
	# 2 first_exposure_age_days 32.8251058
	# 3       cgy_per_min_gamma 24.1942131
	# 4                     sex  3.4178949
	 
	 
	# gbm.by.sp.musculus
	 
	                      # var   rel.inf
	# 1         cgy_total_gamma 35.401596
	# 2       cgy_total_neutron 30.125410
	# 3       cgy_per_min_gamma 10.650824
	# 4 first_exposure_age_days  8.868773
	# 5         fractions_gamma  5.462032
	# 6       fractions_neutron  4.669025
	# 7     cgy_per_min_neutron  3.536614
	# 8                     sex  1.285726
	 
	 	 
	# cox.inter.by.sp.beagle
	 
	# Call:
	# coxph(formula = formula(formula), data = df)
	  # n= 1523, number of events= 1523 
	                                 # coef  exp(coef)   se(coef)      z Pr(>|z|)    
	# sexM                       -9.456e-02  9.098e-01  5.172e-02 -1.828   0.0675 .  
	# first_exposure_age_days    -4.266e-04  9.996e-01  1.048e-04 -4.073 4.65e-05 ***
	# cgy_per_min_gamma           1.301e-02  1.013e+00  1.615e-03  8.056 7.77e-16 ***
	# cgy_total_gamma            -3.218e-05  1.000e+00  1.420e-05 -2.267   0.0234 *  
	 
	 	 
	# cox.inter.by.sp.musculus
	 
	# Call:
	# coxph(formula = formula(formula), data = df)
	  # n= 40424, number of events= 40424 
	                                 # coef  exp(coef)   se(coef)       z Pr(>|z|)    
	# sexM                       -1.700e-01  8.437e-01  1.027e-02 -16.552  < 2e-16 ***
	# first_exposure_age_days    -1.149e-03  9.989e-01  8.154e-05 -14.087  < 2e-16 ***
	# cgy_per_min_gamma           1.777e-02  1.018e+00  8.308e-04  21.394  < 2e-16 ***
	# cgy_per_min_neutron        -5.814e-02  9.435e-01  3.269e-03 -17.785  < 2e-16 ***
	# cgy_total_gamma             1.014e-03  1.001e+00  1.204e-05  84.269  < 2e-16 ***
	# cgy_total_neutron           8.156e-03  1.008e+00  1.087e-04  75.054  < 2e-16 ***
	# fractions_gamma            -3.620e-03  9.964e-01  1.938e-04 -18.677  < 2e-16 ***
	# fractions_neutron          -1.081e-03  9.989e-01  2.460e-04  -4.394 1.11e-05 ***
	 
	 
# Residuals
#
#    It will be helpful to know who did the best at what

	residuals = predictions - data$age_days
	residuals_sq <- data.frame(residuals ^ 2)

# GBM vs simple linear
#
#     Lets compare our best model, gbm by species, with our worst model, the
#     simple linear model.

	plot_all(val(), 
	    x=residuals_sq$lin[data$set == "val"], 
	    y=residuals_sq$gbm.by.sp[data$set == "val"]
	)

#     From these graphs it is obvious that the simple linear model is failing
#     drammatically when the total dose gets very high.  This is resulting in 
#     a non-linear effect, acute radiation poisioning, which gbm can identify
#     by proper cutoffs.

# GBM vs cox
#
#    A more interesting comparison is the results of the best cox model vs
#    the best gbm model.

	plot_all(val(), 
	    x=residuals_sq$cox.interactions.by.sp[data$set == "val"], 
	    y=residuals_sq$gbm.by.sp[data$set == "val"]
	)
	
	# Surprisingly GBM is not better in most cases, only 4193 / 8883 in the
	# validation set.
	
	gbm_better <- residuals_sq$gbm.by.sp / residuals_sq$cox.interactions.by.sp > 1.0
	sum(gbm_better & data$set == "val"); sum(data$set == "val")  # [1] 4193 [1] 8883
	
	# GBM performs better on studies 4, 14, and 2 but worse on most of the dog studies
	sort(table(data$experiment[gbm_better]) / sum(gbm_better)) / (table(data$experiment[! gbm_better]) / sum(! gbm_better))
	
	# Gender was hardly different
	sort(table(data$sex[gbm_better]) / sum(gbm_better)) / (table(data$sex[! gbm_better]) / sum(! gbm_better))
	
	
	
	


	
#   We can see all sorts of confusion.  The model is trying to 


### Future directions ########################################################
#
#   I certainly have a fair amount of work to do going forwards to find more meaning
#   in the data.  My todo list is as follows:
#
#   Account for age biases
#     The current models are biased because animals had to live to a certain age
#     to receive a given treatment.  This makes the results of the analysis
#     misleading.  Sometimes it looks like heavily irradiated animals are expected
#     to live longer, when in fact they are likely to live shorter than equivalent 
#     animals.
#
#     One way to overcome this bias would be to assign doses based on how much radiation the
#     animal would have received had it lived a certain fixed amount of time.  This
#     might remove a good deal of the bias we see now.
#
#   Look for experiment generalization
#     My current measurements of generalization are based on data that was pulled 
#     equally out of every experiment.  However, a really great radiation model
#     should have the ability to view brand new experiments and perform well
#     on them.  This measure of success is useful, because it prevents models from
#     customizing their results to fit experimental bias and forces them to attack
#     consistent effects of radiation.
#
#   Apply more advanced modeling
#     I have explored linear interactions and gbm modeling which are two powerful
#     techniques.  If I am going to go further down this path I will want to add
#     neural network techniques and use ensembling to blend successful models
#     together.  These are the type of models that win modeling contests.
#
#   Adding new variables
#     Rather than directly attacking lifespan, I should probably attack some transformation
#     of lifespan, like normalized lifespan.  I imagine that the results will be easier 
#     to compare between species after making this change.  I also need to look at poorly
#     fit data points to look for unaccounted variables that might explain their poor fit.
#
#   Accounting for current models
#     The current radiobiology models typically use linear no-threshold models, ddref, and
#     cox hazards regressions to estimate the effect or radiation on lifespan.  I need
#     to apply these models to my data sets and see how well they generalize.
#
#   Communicating the models
#     Modern complex models are often too complex to easily communicate.  To make these
#     models useful to the world I need to 1.) post them on the internet so they can be
#     readily explored.  2.) find useful approximations that can sub for the models we build
#     here.  3.) note exceptions to these approximations which might be of interest for 
#     biological exploration.
#
#   Tackle disease endpoints
#     Lifespan is probably the most important endpoint of radiation, but excess tumorogenesis
#     can ruin a long life.  I need to explore other outcomes, especially the rate of solid
#     tumor cancers and leukemia's as its done in the literature.
#
#   Tackle human data
#     If this effort proves worthwhile it will be of maximal importance to add human data to
#     the analysis to make the outcome human relevant.  It will be much easier to do this if
#     we have attracted some attention with the work and found collaborators who already
#     have access to the human datasets.





# TODO 
#  look at the negative over-fit
#  Dogs are labeled with 1 fraction if they received no radiation where mice are labeled with 0.
#  Also, fix the NA dataset for the mice




