# Needles in the Haystack

# The most pressing concern of data scientists is features.  What features will I need to observe or generate to predict an outcome?  For example if I am to predict lifespan I will need to know all sorts of information, gender, disease history, bmi, etc.  If I have enough features and have seen enough people, then I should be able to predict lifespan quite well.  So there are two major questions, how can I get more observations and where should I hunt for relevant features.  I will mull on the latter, how do we find great features?  How does the data we already have inform us?  Could we guess that a feature like bmi might be relevant?

# The method I propose to test here is based on the idea of generalization.  If we can determine what kind of observations are hard to generalize to, then we will be pointed in the direction of new useful features.  For example, perhaps we are trying to predict the lifespan of lab mice.  We find that each experiment has shifts in the average life expectancy.  We don't know why these shifts are occurring, but its reasonable that they might be due to experimental factors like diet, cage conditions, strain and so on.  These unobserved variables cause big shifts in the lifespan outcome and make it difficult for us to predict the results of a new experiment.  However, once we are aware of this fact we know that we should look carefully at experimental factors that might explain the data.

# But how do we become aware of this fact?  At first it seems trivial, build a model without experiment and compare it to a model built with experiment.  If the second model performs better, then we have solid evidence that experiment is biasing our outcome and that we should look for underlying features which are causing these shifts.  But this approach fails when our remaining data is informed by experiment.  For instance, we may have data about total dose of radiation.  Clearly dose will affect lifespan, but it is also tied to experiment because different experiments will include different total doses.  As long as dose is in our model it will represent both itself and the experiment and thereby mask the effect of experiment on the outcome.

# Instead we must ask the problem of generalizability.  How well does our model produced using all the other experiments predict the outcome of an experiment it did not observe?  To avoid the trouble of conducting a brand new experiment we will simply leave each experiment out of the model one at a time and try to predict the results of the left-out experiment using the ones left in.  We compare our predictions made by this method to cross validation done one observation at a time.  In general we should expect that performance will often be worse and when it is, it suggests that the variable in question includes some hidden features that need to be tested.

# To test this hypothesis we will conduct an experiment.  Our hypothesis is that cross validation by feature value will often perform worse than more simple cross validation with said feature left out and where it does will indicate a good place to search for new features.  The second part of this hypothesis is rather subjective and much be approached as a narrative as unbiased as possible (for now).  The first part is easy to test.  We expect that performance will be worse by this method of cross validation except by chance.


# Libaries
	library(ggplot2)
	library(penalized)
	library(gbm)

# Functions

	 gbm_wrap <- function(formula, distribution="gaussian", data=small, cv.folds=5, shrinkage=0.1, n.trees=200){
	 	gbm(formula, distribution=distribution, data=data, cv.folds=cv.folds, shrinkage=shrinkage, n.trees=n.trees)
	 }
	 
	 glm_wrap <- function(formula, distribution="linear", data=small){
	 	penalized(formula, lambda2=0.1, standardize=TRUE, data=small, model="linear")
	 }
	  
	 print_comparison <- function(p1, p2){
	 	print(paste(round(100 * p1 / p2), '% more error'))
	 }
	 
	cv_by <- function(formula, cv_column, method='gbm_wrap'){
		ddply(small, cv_column, function(df){
			#cv_column <- 'color'
			#formula <- price ~ carat
			#df <- small
			value <- df[1, cv_column]
			train <- small[small[,cv_column] != value,]
			test <- df
			model <- eval(call(method, formula, data=train))
			if(method=='gbm_wrap'){
				trees <- gbm.perf(model, method="cv")
				df$p <- predict(model, test, trees)
			} else {
				df$p <- data.frame(predict(model, formula, data=test))$mu
			}
			df
		})	
	}
	
	cost <- function(predicted=small$p, actual=small$price){
		plot(predicted,  actual)
		sum((predicted - actual)^2) / length(predicted)
	}
	
	compose_formula <- function(y, x){
		formula(paste(y, paste(x, collapse='+'), sep='~'))
	}
	
	scrambled_name <- function(string){
		paste('r', string, sep='.')
	}
	
	run_comparision <- function(
		feature, 
		exclude = c(),
		y = 'price',
		x = c('carat', 'cut',  'color', 'clarity', 'depth', 'table', 'x', 'y', 'z'),
		data=small,
		method='gbm_wrap'
		){
		small <- cv_by(compose_formula(y, x), scrambled_name(feature), method=method)
		p_full <- cost(small$p, small$price)
		small <- cv_by(compose_formula(y, x[! x %in% c(feature, exclude)]), scrambled_name(feature), method=method)
		p_cv_by_cv <- cost(small$p, small$price)
		small <-  cv_by(compose_formula(y, x[! x %in% c(feature, exclude)]), feature, method=method)
		p_cv_by_feature <- cost(small$p, small$price)
		print('the cost of our best model is')
		print(p_full)
		print('by excluding the feature we have')
		print_comparison(p_cv_by_cv, p_full)  
		print('during normal cross validation.  And')
		print_comparison(p_cv_by_feature, p_full)  # 144% more error when cross validating by color
		print('when cross validating by the feature')
	}

# Data
#
# We will use a subset of the diamond data (small).
# Each variable will be scrambled for use as controls.
# These scramled variables will be designated r.name
# 
# We will also add quantiled forms of numeric varialbes
# to facilitate cross validation by sections of them.

	small <- diamonds[sample(1:nrow(diamonds), 5000),]
	
	# quantile numeric columns (q.xxx)
	numerics <- c('carat', 'depth', 'table', 'price', 'x', 'y', 'z')
	cuts <- data.frame(llply(small[numerics], function(column){cut(column, quantile(column), include.lowest=TRUE)}))
	names(cuts) <- paste('q', names(cuts), sep='.')
	small <- cbind(small, cuts)

	# scramble all columns (r.xxx)
	scramble <- data.frame(llply(small, function(column){sample(column)}))
	names(scramble) <- paste('r', names(scramble), sep='.')
	small <- cbind(small, scramble)


# Removal vs. Cross validation

# Now we will show that removing varaibles from the model
# results in better performance than removing them and cross
# validating by them.
#
# overall performance = 1,199,111



	run_comparision('color')
	# [1] "by excluding the feature we have"
	# [1] "129 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "150 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('cut')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "105 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('clarity')
	# [1] "by excluding the feature we have"
	# [1] "149 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "180 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.carat', 'carat')
	# [1] "by excluding the feature we have"
	# [1] "101 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "751 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.depth', 'depth')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "106 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.table', 'table')
	# [1] "by excluding the feature we have"
	# [1] "101 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "100 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.price', 'price')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "827 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.x', 'x')
	# [1] "by excluding the feature we have"
	# [1] "98 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "707 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.y', 'y')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "750 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.z', 'z')
	# [1] "by excluding the feature we have"
	# [1] "99 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "552 % more error"
	# [1] "when cross validating by the feature"

# GBM conclusions
#
# So the results conform to our expectations, models always do worse when cross validated 
# by the feature we removed then when performing cross validation by obveration.  

# We learn more that's interesting.  Color and Clarity reduce performance both when taken
# out of the models and when cross validated by.  This implies that there is little redundancy
# in this dataset between those features and others.

# By contrast clarity, price, x, y, z, and carat are massively redundant (probably with one another).
# We loose very little by keeping one of these out of the model, but when we cross validate by
# them our performance goes to shit.  Our models are incapable of extrapolating to these new
# and unknown conditions.  Perhaps another model (like a linear model) would do better.  All we
# can say for now is that if we were to see a diamond of a size we had never seen before then
# these models would do a very bad job of predicting its real price, and this is surely a valuable
# thing to know.

# Finally there are a number of traits that make little difference in any case, table, depth, cut are fine
# to loose and easy to predict even if we do lose them.  We can be fairly confident that if we were
# to encounter a diamond with a new value for one of these features that we could predict its price
# rather accurately using our current models.


# GLM
# 
# What if we try a more generalizable model (penalized logistic regression)
#
# Overall
# 1,225,506 (2% worse than the gbm model)

	run_comparision('color', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "127 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "127 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('cut', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "101 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "101 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('clarity', method='glm_wrap')	
	# [1] "by excluding the feature we have"
	# [1] "156 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "156 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.carat', 'carat', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "215 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "215 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.depth', 'depth', method='glm_wrap')
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "100 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.table', 'table', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "100 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.price', 'price', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "100 % more error"
	# [1] "when cross validating by the feature"

	run_comparision('q.x', 'x', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "100 % more error"
	# [1] "when cross validating by the feature"

	run_comparision('q.y', 'y', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "100 % more error"
	# [1] "when cross validating by the feature"
	
	run_comparision('q.z', 'z', method='glm_wrap')
	# [1] "by excluding the feature we have"
	# [1] "100 % more error"
	# [1] "during normal cross validation.  And"
	# [1] "100 % more error"
	# [1] "when cross validating by the feature"
	
# GLM Conclusions
#
# Wow look how much better this generalized!  Its performance was only 2% worse
# than gbm to begin with and after cross validating by caret it was only 2x worse
# whereas gbm was 9 times worse.  In this dataset glm's propensity to generalize
# really shines!