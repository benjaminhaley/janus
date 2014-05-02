library('caret')

#### Model data ####
subset     <- small(4000)                                         # working data
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
tmp        <- createDataPartition(y, p=0.8, times=10)             # cv by observation
by_obs     <- trainControl(method = "LGOCV", index = tmp)
exp        <- subset$experiment                                   # cv by experiment
tmp        <- llply(unique(exp), function(e){which(exp != e)})
names(tmp) <- paste('Resample', 1:9, sep='')
by_exp     <- trainControl(method = "LGOCV", index = tmp )               

                           
#### Build Models ####
models_by_exp <- list(
	 gbm=train(X, y, "gbm", trControl = by_exp)
	,glm=train(X, y, "glm", trControl = by_exp)
	,nnet=train(X, y01, "nnet", trControl = by_exp, preProcess = c("center", "scale"))
)
models_by_obs <- list(
	 gbm=train(X, y, "gbm", trControl = by_obs)
	,glm=train(X, y, "glm", trControl = by_obs)
	,nnet=train(X, y01, "nnet", trControl = by_obs, preProcess = c("center", "scale"))
)

#summary(models_by_exp$gbm$finalModel)
#plot(nnet_by_exp)


#### Evaluate Models ####
perform_graph <- function(m){bwplot(resamples(m), metric = "Rsquared")}
perform_graph(models_by_obs)
perform_graph(models_by_exp)

rel_perform_graph <- function(m){densityplot(diff(resamples(m)), metric = "Rsquared")}
rel_perform_graph(models_by_obs)
rel_perform_graph(models_by_exp)

model_significance <- function(m){ summary(diff(resamples(m))); }
model_significance(models_by_obs)
model_significance(models_by_exp)



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


#### Test Points ####

# Gamma test set
n <- 20
max_dose <- 800
delta <- max_dose / n
testG <- matrix(rep(c(
	2, 120, 0, 0,  0, 60,  0,  1,  0,
	1, 120, 0, 0,  0, 60,  0,  1,  0,
	2, 120, 0, 0,  0,  1,  0,  1,  0,
	1, 120, 0, 0,  0,  1,  0,  1,  0,
	2, 120, 0, 0,  0, 60,  0, 10,  0,
	1, 120, 0, 0,  0, 60,  0, 10,  0,
	2, 120, 0, 0,  0,  1,  0, 10,  0,
	1, 120, 0, 0,  0,  1,  0, 10,  0), n),
	dimnames = list(c(),X_factors),
	ncol = 9,
	byrow = TRUE
)
testG[,'cgy_total_gamma_assigned'] <- sort(rep((1:(max_dose/delta))*delta, 8))

# Neutron test set
n <- 20
max_dose <- 160
delta <- max_dose / n
testN <- matrix(rep(c(
	2, 120, 0,  0, 30,  0, 60,  0, .1,       # Neutrons
	1, 120, 0,  0, 30,  0, 60,  0, .1,
	2, 120, 0,  0, 30,  0,  1,  0, .1,       # less fractions
	1, 120, 0,  0, 30,  0,  1,  0, .1,
	2, 120, 0,  0, 30,  0, 60,  0,  1,       # higher rate
	1, 120, 0,  0, 30,  0, 60,  0,  1,
	2, 120, 0,  0, 30,  0,  1,  0,  1,
	1, 120, 0,  0, 30,  0,  1,  0,  1), n),
	dimnames = list(c(),X_factors),
	ncol = 9,
	byrow = TRUE
)
testN[,'cgy_total_neutron_assigned'] <- sort(rep((1:(max_dose/delta))*delta, 8))

# Full test set
testX <- rbind(testN, testG)


#### Get Predictions ####

get_pre <- function(m, data=X){ 
	p <- data.frame(predict(m, data))                             # predict
	p$nnet <- p$nnet * 2000 - 1000                                # adjust nnet
	p$median <- apply(p, 1, median)                               # ensemble
	p 
}
p_by_obs <- get_pre(models_by_obs)
p_by_exp <- get_pre(models_by_exp)
t_by_obs <- get_pre(models_by_obs, testX)
t_by_exp <- get_pre(models_by_exp, testX)


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
	#"gbm", 
	"glm", 
	#"nnet", 
	"median"
)
plot_reps <- function(reps, m=model_names){
	long = reshape(reps, varying=m, direction="long", v.names='p', times=m, timevar="model")
	ggplot(long, aes(cgy_total_assigned, p, color=model)) + 
	geom_point(alpha=0.5) +
	geom_path() +
	facet_wrap(~ treatment, scales = "free")
}

# Test points
reps_by_obs <- cbind(test_to_full(), t_by_obs)
reps_by_exp <- cbind(test_to_full(), t_by_exp)
reps_by_obs$treatment <- get_treatment_names(reps_by_obs)
reps_by_exp$treatment <- get_treatment_names(reps_by_exp)
plot_reps(reps_by_obs)
plot_reps(reps_by_exp)
