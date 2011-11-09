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


##############################################################################
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

# Define the sampling size for validation
validation.percentage = 0.10


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
formula_simple_lifespan <- f.builder$get_formula(macros, lifespan_parameters)

# Complex models, "VOL_N ~ age + (age * age) + sex + (age * sex)..."
formula_complex_macro <- f.builder$get_formula(macros, macro_parameters, self.interaction=TRUE)
formula_complex_lifespan <- f.builder$get_formula(macros, lifespan_parameters, self.interaction=TRUE)


##############################################################################
# To determine which model performs the best we
# will remove a random 10% sample of the data
# and use it to measure our prediction accuracy
# or R^2 value depending on whether we are 
# looking at lifespan or macro patholgies

# split data set
m <- sum(subset.2.analyze)
validation.size <- floor(m * validation.percentage)
permutation <- sample(which(subset.2.analyze==TRUE))
validation <- permutation[1:validation.size]
training <- permutation[validation.size:length(permutation)]

# run models
formula <- "TLIV_L ~ age + sex + species + first_irrad + total_dose + radn 
            + was_control_mock_treated + fractions + time_min + dose_rate 
			+ necroscopy_date + necrosopy_proctor"
family <- binomial(link = "logit")
model <- glm( formula=formula, data=data[training ,], family = family )

predictions <- predict(model, data, type="response")
positive <- which(data[["TLIV_L"]]==TRUE)
negative <- which(data[["TLIV_L"]]==FALSE)
positive.training.predictions <- predictions[positive[positive %in% training]]
positive.validation.predictions <- predictions[positive[positive %in% validation]]
negative.training.predictions <- predictions[negative[negative %in% training]]
negative.validation.predictions <- predictions[negative[negative %in% validation]]
training.auc <- mean(
	sample(positive.training.predictions,100000,replace=T) 
	> 
	sample(negative.training.predictions,100000,replace=T)
	)
validation.auc <- mean(
	sample(positive.validation.predictions,100000,replace=T) 
	> 
	sample(negative.validation.predictions,100000,replace=T)
	)
training.auc
validation.auc

# Complex model
formula <- "
	TLIV_L ~ age + sex + species + first_irrad + total_dose + radn + 
	was_control_mock_treated + fractions + time_min + dose_rate + necroscopy_date + 
	necrosopy_proctor + age*age + sex*sex + species*species + first_irrad*first_irrad + 
	total_dose*total_dose + radn*radn + was_control_mock_treated*was_control_mock_treated + 
	dose_rate*necrosopy_proctor + necroscopy_date*necrosopy_proctor"
family <- binomial(link = "logit")
model <- glm( formula=formula, data=data[training ,], family = family )

predictions <- predict(model, data, type="response")
positive <- which(data[["TLIV_L"]]==TRUE)
negative <- which(data[["TLIV_L"]]==FALSE)
positive.training.predictions <- predictions[positive[positive %in% training]]
positive.validation.predictions <- predictions[positive[positive %in% validation]]
negative.training.predictions <- predictions[negative[negative %in% training]]
negative.validation.predictions <- predictions[negative[negative %in% validation]]
training.auc <- mean(
	sample(positive.training.predictions,100000,replace=T) 
	> 
	sample(negative.training.predictions,100000,replace=T)
	)
validation.auc <- mean(
	sample(positive.validation.predictions,100000,replace=T) 
	> 
	sample(negative.validation.predictions,100000,replace=T)
	)
training.auc
validation.auc



# Finally we will build a table listing each
# Outcome by each model for accuracy or R^2 value

# We will compare all models to the simple 
# model by subtracting the accuracy achieved under
# simple conditions from the accuracy achieved 
# under complex conditions.  Then we will ask 
# which is performing better by comparing the 
# histograms of these values.




