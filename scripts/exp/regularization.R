##############################################################################
#
# OVERVIEW
#
# To determine if reguarlization improves our
# ability to model endpoints in the janus dataset
# bmh Nov 2011

# load dependencies
source('../data/data.R')
source('../data/ontology.R')
source('../util/f.builder.R')
source('../util/package.R')
package$load(c("penalized"))

#TODO
# Be sure to cite the packages and language I am using

#############################################################################
#
# DEFINE DATA & OUTCOMES
#
# We will try to predict outcomes like lifespan
# and various pathologies using all of the janus
# data and using the factors and their interaction
# terms as data.

# Load the janus data
data <- data$load('janus', from_cache=TRUE)

# Load the ontologies 
c <- ontology$load_columns()
r <- ontology$load_rows(data)

# Define the dataset under analysis
subset.2.analyze <- r$HAS_MACRO 

# Define the outcomes
macros <- c$MACROS_
lifespan <- c$LIFESPAN_DAYS 

# Define model parameters
macro_parameters <- c(c$DEMOGRAPHICS, c$TREATMENT, c$AUTOPSY)
lifespan_parameters <- c(c$SPECIES, c$SEX, c$TREATMENT, c$AUTOPSY)

# TODO move this logic into ontology or data load
# TODO remove the redudancy between lifespan and macro parameters
macro_parameters <- macro_parameters[macro_parameters != "age"]
macro_parameters <- macro_parameters[macro_parameters != "time_min"]
lifespan_parameters <- lifespan_parameters[lifespan_parameters != "age"]
lifespan_parameters <- lifespan_parameters[lifespan_parameters != "time_min"]
data[data[["necrosopy_proctor"]] == "PD", "necrosopy_proctor"] <- ""
data[data[["necrosopy_proctor"]] == "JR", "necrosopy_proctor"] <- ""
data[data[["necrosopy_proctor"]] == "JP", "necrosopy_proctor"] <- ""
data[data[["necrosopy_proctor"]] == "FG", "necrosopy_proctor"] <- ""
data[data[["necrosopy_proctor"]] == "BF", "necrosopy_proctor"] <- ""
data[data[["necrosopy_proctor"]] == "AL", "necrosopy_proctor"] <- ""
data[data[["necrosopy_proctor"]] == "CZ", "necrosopy_proctor"] <- ""
data[is.na(data["was_control_mock_treated"]), "was_control_mock_treated"] <- 0

# Define the sampling size for validation
validation.percentage = 0.50


##############################################################################
# For each outcomes of interest we will build
# three models.  The first will be simple, only 
# demographic terms with no interactions.  No
# regularliation will be applies to this model.
#
# The other two models will be complex consisting
# of all of the demographic terms and their interactions
#
# One of these complex models will use
# regularization and the other will not.

# Simple models, "VOL_N ~ age + sex + species..." no interactions
formula_simple_macro <- f.builder$get_formula(macros, macro_parameters)
formula_simple_lifespan <- f.builder$get_formula(lifespan, lifespan_parameters)

# Complex models, "VOL_N ~ age + (age * age) + sex + (age * sex)..."
formula_complex_macro <- f.builder$get_formula(macros, macro_parameters, self.interaction=TRUE)
formula_complex_lifespan <- f.builder$get_formula(lifespan, lifespan_parameters, self.interaction=TRUE)


data[["sex"]] <- as.factor(data[["sex"]])
data[["species"]] <- as.factor(data[["species"]])
data[["radn"]] <- as.factor(data[["radn"]])
data[["necrosopy_proctor"]] <- as.factor(data[["necrosopy_proctor"]])

##############################################################################
# To determine which model performs the best we
# will remove a random 10% sample of the data
# and use it to measure our prediction accuracy
# or R^2 value depending on whether we are 
# looking at lifespan or macro patholgies

# split data set
m.total <- nrow(data)
m.validation <- floor(m.total * validation.percentage)
m.training <- m.total - m.validation
validation.vector <- sample(c(rep(TRUE, m.validation), rep(FALSE, m.training)))
training.vector <- ! validation.vector

# run models
formula <- formula_simple_macro[120]
family <- binomial(link = "logit")
model <- glm( formula=formula, data=data[training.vector & subset.2.analyze ,], family = family )

predictions <- predict(model, data)
positive <- data[["TLIV_L"]]==TRUE
negative <- data[["TLIV_L"]]==FALSE
positive.training.predictions <- predictions[positive & training.vector & subset.2.analyze]
positive.validation.predictions <- predictions[positive & validation.vector & subset.2.analyze]
negative.training.predictions <- predictions[negative & training.vector & subset.2.analyze]
negative.validation.predictions <- predictions[negative & validation.vector & subset.2.analyze]
simple.training.auc <- mean(
	sample(positive.training.predictions,1e6,replace=T) 
	> 
	sample(negative.training.predictions,1e6,replace=T)
	)
simple.validation.auc <- mean(
	sample(positive.validation.predictions,1e6,replace=T) 
	> 
	sample(negative.validation.predictions,1e6,replace=T)
	)




# Complex model
formula <- formula_complex_macro[120]
family <- binomial(link = "logit")
model <- glm( formula=formula, data=data[training.vector & subset.2.analyze ,], family = family )

predictions <- predict(model, data)
positive <- data[["TLIV_L"]]==TRUE
negative <- data[["TLIV_L"]]==FALSE
positive.training.predictions <- predictions[positive & training.vector & subset.2.analyze]
positive.validation.predictions <- predictions[positive & validation.vector & subset.2.analyze]
negative.training.predictions <- predictions[negative & training.vector & subset.2.analyze]
negative.validation.predictions <- predictions[negative & validation.vector & subset.2.analyze]
complex.training.auc <- mean(
	sample(positive.training.predictions,1e6,replace=T) 
	> 
	sample(negative.training.predictions,1e6,replace=T)
	)
complex.validation.auc <- mean(
	sample(positive.validation.predictions,1e6,replace=T) 
	> 
	sample(negative.validation.predictions,1e6,replace=T)
	)

# Complex model with regularization
formula <- as.formula(formula_complex_macro[120])
fit <- penalized(
					formula, data = data[training.vector & subset.2.analyze,], 
					lambda2 = 10000, standardize=TRUE, model="logistic"
				)
positive.training.predictions <-
	 predict(fit, data=data[subset.2.analyze & training.vector & data[,"TLIV_L"]==TRUE,])
positive.validation.predictions <-
	 predict(fit, data=data[subset.2.analyze & validation.vector & data[,"TLIV_L"]==TRUE,])
negative.training.predictions <-
	 predict(fit, data=data[subset.2.analyze & training.vector & data[,"TLIV_L"]==FALSE,])
negative.validation.predictions <-
	 predict(fit, data=data[subset.2.analyze & validation.vector & data[,"TLIV_L"]==FALSE,])
regularized.training.auc <- mean(
    sample(positive.training.predictions,1e6,replace=T)
	>
	sample(negative.training.predictions,1e6,replace=T)
	)
regularized.validation.auc <- mean(
    sample(positive.validation.predictions,1e6,replace=T)
	>
	sample(negative.validation.predictions,1e6,replace=T)
	)

# Using lasso 
formula <- as.formula(formula_complex_macro[120])
fit <- penalized(
					formula, data = data[training.vector & subset.2.analyze,], 
					lambda1 = 25, standardize=TRUE, model="logistic"
				)
positive.training.predictions <-
	 predict(fit, data=data[subset.2.analyze & training.vector & data[,"TLIV_L"]==TRUE,])
positive.validation.predictions <-
	 predict(fit, data=data[subset.2.analyze & validation.vector & data[,"TLIV_L"]==TRUE,])
negative.training.predictions <-
	 predict(fit, data=data[subset.2.analyze & training.vector & data[,"TLIV_L"]==FALSE,])
negative.validation.predictions <-
	 predict(fit, data=data[subset.2.analyze & validation.vector & data[,"TLIV_L"]==FALSE,])
lasso.training.auc <- mean(
    sample(positive.training.predictions,1e6,replace=T)
	>
	sample(negative.training.predictions,1e6,replace=T)
	)
lasso.validation.auc <- mean(
    sample(positive.validation.predictions,1e6,replace=T)
	>
	sample(negative.validation.predictions,1e6,replace=T)
	)

simple.training.auc
simple.validation.auc 
complex.training.auc
complex.validation.auc
regularized.training.auc
regularized.validation.auc
lasso.training.auc
lasso.validation.auc


################################################################################
# LIFESPAN
# @TODO refactor this mess!

training.set <- training.vector & subset.2.analyze
validation.set <- validation.vector & subset.2.analyze
validation.outcome <- data[validation.set,lifespan]
training.outcome <- data[training.set,lifespan]
training.ss <- sum( (training.outcome - mean(training.outcome) )**2 )
validation.ss <- sum( (validation.outcome - mean(validation.outcome) )**2 )

# simple model
formula <- as.formula(formula_simple_lifespan)
model <- lm( formula=formula, data=data[training.vector & subset.2.analyze ,])

training.predictions <- predict(model, newdata=data[training.set,])
validation.predictions <- predict(model, newdata=data[validation.set,])

training.sr <- sum( (training.predictions - training.outcome)**2 )
validation.sr <- sum( (validation.predictions - validation.outcome)**2 )

simple.training.r2 <- 1 - (training.sr / training.ss) 
simple.validation.r2 <- 1 - (validation.sr / validation.ss) 


# Complex model
# @TODO shouldn't the formula package output formulas rather than strings?
formula <- as.formula(formula_complex_lifespan)
model <- lm( formula=formula, data=data[training.vector & subset.2.analyze ,])

training.predictions <- predict(model, newdata=data[training.set,])
validation.predictions <- predict(model, newdata=data[validation.set,])

training.sr <- sum( (training.predictions - training.outcome)**2 )
validation.sr <- sum( (validation.predictions - validation.outcome)**2 )

complex.training.r2 <- 1 - (training.sr / training.ss) 
complex.validation.r2 <- 1 - (validation.sr / validation.ss) 


# Complex model with regularization
formula <- as.formula(formula_complex_lifespan)
model <- penalized(
					formula, data = data[training.vector & subset.2.analyze,], 
					lambda2 = 0.000001, standardize=TRUE, model="linear"
				)

training.predictions <- predict(model, data=data[training.set,])[,1]
validation.predictions <- predict(model, data=data[validation.set,])[,1]

training.sr <- sum( (training.predictions - training.outcome)**2 )
validation.sr <- sum( (validation.predictions - validation.outcome)**2 )

regularized.training.r2 <- 1 - (training.sr / training.ss) 
regularized.validation.r2 <- 1 - (validation.sr / validation.ss) 


# Using lasso 
formula <- as.formula(formula_complex_lifespan)
model <- penalized(
					formula, data = data[training.vector & subset.2.analyze,], 
					lambda1 = 2, standardize=TRUE, model="linear"
				)

training.predictions <- predict(model, data=data[training.set,])[,1]
validation.predictions <- predict(model, data=data[validation.set,])[,1]

training.sr <- sum( (training.predictions - training.outcome)**2 )
validation.sr <- sum( (validation.predictions - validation.outcome)**2 )

lasso.training.r2 <- 1 - (training.sr / training.ss) 
lasso.validation.r2 <- 1 - (validation.sr / validation.ss) 


simple.training.r2
simple.validation.r2
complex.training.r2
complex.validation.r2
regularized.training.r2
regularized.validation.r2
lasso.training.r2
lasso.validation.r2

# Finally we will build a table listing each
# Outcome by each model for accuracy or R^2 value

# We will compare all models to the simple 
# model by subtracting the accuracy achieved under
# simple conditions from the accuracy achieved 
# under complex conditions.  Then we will ask 
# which is performing better by comparing the 
# histograms of these values.




