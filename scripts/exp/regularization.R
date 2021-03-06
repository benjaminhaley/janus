##############################################################################
#
# OVERVIEW
#
# To determine if reguarlization improves our ability to model endpoints in 
# the janus dataset.
#
# bmh Nov 2011

# load dependencies
source('scripts/data/data.R')
source('scripts/data/ontology.R')
source('scripts/util/f.builder.R')
source('scripts/util/package.R')
package$load(c("penalized"))
source('scripts/util/select.R')


#############################################################################
#
# START A REPORT
# 
# The output generated by this script is a report object which should contain 
# most of the relevant information for publishing this work.

# Instantiate the report
report <- list()

# Add some meta-info
report$citations <- c(citation(), citation('penalized'))
report$source <- readLines('regularization.R')
report$git <- 'https://github.com/benjaminhaley/janus'
report$author <- c('Benjamin Haley', 'Will Liu', 'Tatjana Paunesku', 'Gayle Woloschak')
report$email <- 'benjamin.haley@gmail.com'

#############################################################################
#
# DEFINE DATA & OUTCOMES
#
# We will try to predict lifespan, presence of lethal tumor, tumor count,
# and miscillaneous pathological outcomes using of the entire janus data
# set and a variety of factors and their interactions.
#
# We will not worry about cause and effect, instead we want to build predictive
# models using any data that would have been available at the time an outcome
# occurred.
#
# For example, we include necropsy_proctor in the analysis even though
# this is likely to correlate with radiation factors and cancle them out.
# However, we do not include necropsy observations in the prediction of
# lifespan because this data would not be available until after an animal
# was dead.
#
# Therefore, it would be inappropriate to dissect the coefficients of these
# models as effects are likely mis-alocated.  Instead the work should be used
# as a experiment to explore which types of outcomes and interactions
# contributed to the measured outcomes.
#
# To prevent overzealous fitting using a naive hypothesis, we will split the
# dataset into training and validation portions.

# Load the janus data
#
data <- data$load('janus', from_cache=TRUE)

# Load the ontologies 
#
c <- ontology$load_columns()
r <- ontology$load_rows(data)

# Define the dataset under analysis
#
validation.percentage = 0.50
validation.vector <- runif(nrow(data)) < validation.percentage
training <- r$HAS_MACRO & !validation.vector
validation <- r$HAS_MACRO & validation.vector

# Define the outcomes, the most common macros, lifespan,
# tumor counts, and animals which died from tumors
#
data["DIED.FROM.TUMOR"] <- r$DIED_FROM_TUMOR
data["TUMOR.COUNT"] <- r$TUMOR_COUNT
frequent.macros <- select$by_count(data, c$MACROS_, min=1, max=3)
lifespan <- c$LIFESPAN_DAYS
tumor.deaths <- "DIED.FROM.TUMOR"
tumor.counts <- "TUMOR.COUNT"

# Define model parameters
#
less_macro_parameters <- c(c$DEMOGRAPHICS, c$TREATMENT)
less_macro_parameters <- less_macro_parameters[less_macro_parameters != "time_min"]
macro_parameters <- c(c$DEMOGRAPHICS, c$TREATMENT, c$AUTOPSY)
macro_parameters <- macro_parameters[macro_parameters != "time_min"]
less_lifespan_parameters <- c(c$SPECIES, c$SEX, c$TREATMENT)
less_lifespan_parameters <- less_lifespan_parameters[less_lifespan_parameters != "time_min"]
lifespan_parameters <- c(c$SPECIES, c$SEX, c$TREATMENT, c$AUTOPSY)
lifespan_parameters <- lifespan_parameters[lifespan_parameters != "time_min"]

# Consolidate Rare proctors because there are too many of them for analysis
#
original.proctors <- table(data[,"necrosopy_proctor"])
lazy.proctors <- c("PD", "JR", "JP", "FG", "BF", "AL", "CZ")
data[data[["necrosopy_proctor"]] %in% lazy.proctors, "necrosopy_proctor"] <- ""
data[["necrosopy_proctor"]] <- factor(data[["necrosopy_proctor"]])

# Add relevant factors to report object
#
report$training <- training
report$validation <- validation 
report$original.proctors <- original.proctors
report$simplified.proctors <- table(data[,"necrosopy_proctor"])


##############################################################################
# 
# MODEL FORMULA
#
# Lifepan and Macro outcomes will be predicted by lm and glm respectively.
# They will use each of two models.  One simple defined by all of the
# model factors defined above and one complex consisting of these same model
# factors and also each interaction between them.
#
# Our hypothosis is that the complex models will always outperform the simple
# models on the training set, but that the unregularized complex model will 
# overfit the data and therefore generalize poorly to the validation set.
#
# We hope to compensate for this difference by regularization which might 
# produce a better model than the simple formula and hold up to the scrutiny
# of validation.

# Simple models, "VOL_N ~ age + sex + species..." no interactions
#
formula_less_macro <- f.builder$get_right_from_parameters(less_macro_parameters)
formula_simple_macro <- f.builder$get_right_from_parameters(macro_parameters)
formula_less_lifespan <- f.builder$get_right_from_parameters(less_lifespan_parameters)
formula_simple_lifespan <- f.builder$get_right_from_parameters(lifespan_parameters)

# Complex models, "VOL_N ~ age + (age * age) + sex + (age * sex)..."
#
macro_interactions <- f.builder$get_self_interactions(macro_parameters)
lifespan_interactions <- f.builder$get_self_interactions(lifespan_parameters)
formula_complex_macro <- f.builder$get_right_from_parameters(c(macro_parameters, macro_interactions))
formula_complex_lifespan <- f.builder$get_right_from_parameters(c(lifespan_parameters, lifespan_interactions))

# Add the formula to our report
#
report$formulas <- c(
	 formula_less_macro, formula_less_lifespan,
	 formula_simple_macro, formula_simple_lifespan, 
	 formula_complex_macro, formula_complex_lifespan
	 )


##############################################################################
#
# RUN MODELS
#
# Now that we have defined out inputs and formulas we will run regressions on 
# them.  We will apply regularization or lasso regularization to the complex
# models to see if we can improve their performance.
#
# Because the regularization steps have to be hand tuned, they will be written
# out sepperately.  These were tuned using the training data so as not to bias
# the quality of the validation.

# Some helper functions
#
macro.glm <- function(macro, right_formula){
	formula <- as.formula(paste(macro, "~", right_formula))
	family <- binomial(link = "logit")
	model <- glm(formula=formula, data=data[training,], family=family)
	return(model)
}
macro.reg <- function(macro, right_formula, lambda2){
	formula <- as.formula(paste(macro, "~", right_formula))
	model <- penalized(
					formula, data=data[training,], 
					lambda2=lambda2, standardize=TRUE, model="logistic"
				)
	return(model)
}
lifespan.lm <- function(right_formula){
	formula <- as.formula(paste(lifespan, "~", right_formula))
	model <- lm(formula=formula, data=data[training,])
	return(model)
}
lifespan.reg <- function(right_formula, lambda){
	formula <- as.formula(paste(lifespan, "~", right_formula))
	model <- penalized(
					formula, data=data[training,], 
					lambda2=lambda, standardize=TRUE, model="linear"
				)
	return(model)
}


# Run macro models 
#
all_macro <- c(frequent.macros, tumor.deaths)

less.macro.models <- lapply(all_macro, FUN=macro.glm, formula_less_macro)
simple.macro.models <- lapply(all_macro, FUN=macro.glm, formula_simple_macro)
complex.macro.models <- lapply(all_macro, FUN=macro.glm, formula_complex_macro)
under.regularized.macro.models <- lapply(all_macro, FUN=macro.reg, formula_complex_macro, 100)
over.regularized.macro.models <- lapply(all_macro, FUN=macro.reg, formula_complex_macro, 300)

# Run life models
#
less.life.model <- lifespan.lm(formula_less_lifespan)
simple.life.model <- lifespan.lm(formula_simple_lifespan)
complex.life.model <- lifespan.lm(formula_complex_lifespan)
regularized.life.model <- lifespan.reg(formula_complex_lifespan, 30)



##############################################################################
#
# ANALYZE MODELS
#
# Once all the models are run we can use them to calculate their 'goodness of
# fit' by auc or R2.  
#
# By comparing the training and the validation results we can determine if the
# model is overfit.  If it is, then the training set should be more fit than
# the validation.
# 
# Finally we can compare between models by comparing thier preformance on the 
# validation sets. A better model will do better on these validation sets 
# regardless of whether it is over fit.

# Helper functions
#
get.auc <- function(macro, macro.model, subset){
	is.penfit <- inherits(macro.model, "penfit")
	if(is.penfit){
		predictions <- predict(macro.model, data=data[subset,])
	} else {
		predictions <- predict(macro.model, data[subset,])
	}
	positive <- predictions[data[subset,macro]==TRUE]
	negative <- predictions[data[subset,macro]==FALSE]
	auc <- mean(sample(positive,1e6,replace=T) > sample(negative,1e6,replace=T))
	return(auc)
}
get.R2 <- function(model, subset){
	is.penfit <- inherits(model, "penfit")
	if(is.penfit){
		predictions <- predict(model, data = data[subset, ])[,1]
	} else {
		predictions <- predict(model, data[subset,])
	}
	life <- data[subset, lifespan]
	ss <- sum((life - mean(life))**2)
	sr <- sum((predictions - life)**2)
	r2 <- 1 - (sr/ss)
	return(r2)
}

# Get Macro AUCs
# 
report$less.macro.training.auc <- mapply(FUN=get.auc, all_macro, less.macro.models, MoreArgs=list(training))
report$less.macro.validation.auc <- mapply(FUN=get.auc, all_macro, less.macro.models, MoreArgs=list(validation))
report$simple.macro.training.auc <- mapply(FUN=get.auc, all_macro, simple.macro.models, MoreArgs=list(training))
report$simple.macro.validation.auc <- mapply(FUN=get.auc, all_macro, simple.macro.models, MoreArgs=list(validation))
report$complex.macro.training.auc <- mapply(FUN=get.auc, all_macro, complex.macro.models, MoreArgs=list(training))
report$complex.macro.validation.auc <- mapply(FUN=get.auc, all_macro, complex.macro.models, MoreArgs=list(validation))
report$under.regularized.macro.training.auc <- mapply(FUN=get.auc, all_macro, under.regularized.macro.models, MoreArgs=list(training))
report$under.regularized.macro.validation.auc <- mapply(FUN=get.auc, all_macro, under.regularized.macro.models, MoreArgs=list(validation))
report$over.regularized.macro.training.auc <- mapply(FUN=get.auc, all_macro, over.regularized.macro.models, MoreArgs=list(training))
report$over.regularized.macro.validation.auc <- mapply(FUN=get.auc, all_macro, over.regularized.macro.models, MoreArgs=list(validation))

# Get Lifespan R2s
#
report$less.lifespan.training.r2 <- get.R2(less.life.model, training)
report$less.lifespan.validation.r2 <- get.R2(less.life.model, validation)
report$simple.lifespan.training.r2 <- get.R2(simple.life.model, training)
report$simple.lifespan.validation.r2 <- get.R2(simple.life.model, validation)
report$complex.lifespan.training.r2 <- get.R2(complex.life.model, training)
report$complex.lifespan.validation.r2 <- get.R2(complex.life.model, validation)
report$regularized.lifespan.training.r2 <- get.R2(regularized.life.model, training)
report$regularized.lifespan.validation.r2 <- get.R2(regularized.life.model, validation)



