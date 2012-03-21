# An attempt to recreate Marissa's analysis in A Retrospective Analysis...
# should exercise the new janus system
# bmh Oct 2011

# Load the janus data
setwd('janus')
source('scripts/data/data.R')
data <- data$load('janus', from_cache=TRUE)

# Some helper functions
source('scripts/util/freq.R')
source('scripts/util/odds_ratio.R')
source("scripts/util/package.R")

# Load the annotations
source('scripts/data/ontology.R')
c <- ontology$load_columns()
r <- ontology$load_rows(data)

# Extra packages
package$load(c("aod"))

# Define the dataset under analysis
data["treatment"] <- NA
data[r$FRACTIONS_60 & r$HAS_MACRO & r$CONTROL,                "treatment"] <- "CONTROL"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_100,   "treatment"] <- "G_100cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_200,   "treatment"] <- "G_200cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_300,   "treatment"] <- "G_300cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_450,   "treatment"] <- "G_450cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_600,   "treatment"] <- "G_600cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_2.05,  "treatment"] <- "N_2.05cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_7.69,  "treatment"] <- "N_7.69cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_13.85, "treatment"] <- "N_13.85cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_21.54, "treatment"] <- "N_21.54cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_30.78, "treatment"] <- "N_30.78cGY"
data[r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_40.04, "treatment"] <- "N_40.04cGY"
ALL <- !is.na(data["treatment"])
CON <- data[["treatment"]] %in% "CONTROL"
EXP <- ALL & !CON

# Sometimes we will want to lump control genders info
data['mod_gender'] <- data["sex"]
data[CON, 'mod_gender'] <- 'N' 

# combine lethal and non-lethal pathologies 
merged_macros <- ontology$merge_macros(data)
data <- cbind(data, merged_macros)
COMMON_TOXICITIES <- freq$get_columns(data[ALL,], c$MACROS, minimum_sum=30)

# Age is quantile by gender
f.age <- data[ALL & r$FEMALE, "age"]
m.age <- data[ALL & r$MALE, "age"]
data[ALL & r$FEMALE, "q.age"] <- cut(f.age, quantile(f.age), labels=c("q1","q2","q3","q4"))
data[ALL & r$MALE, "q.age"] <- cut(m.age, quantile(m.age), labels=c("q1","q2","q3","q4"))

# Get counts of the number of animals in each treatment group
table.1 <- freq$get_table(data[
			ALL,
			c("radn", "fractions", "total_dose", "dose_rate")
		])

# Get toxicity descriptions
table.2 <- ontology$macro2janus_description(COMMON_TOXICITIES)

# Start some model building
table.3.toxicities <- c("CLR","CYS","HGL","OVE", "TOVE", "TADN","TPYL","TSEC")
table.3.fun <- function(pathology){
	formula <- paste(pathology, "~ treatment*sex + q.age*sex -treatment -sex -q.age -1")
	family <- binomial(link = "logit")
	model <- glm( formula=formula, data=data[ALL & !r$NEUTRON ,], family = family )
	coef_ <- coef(model)
	f.coef <- coef_[1:6]
	m.coef <- coef_[7:12]
	f.odds <- odds_ratio$logit2or(f.coef, f.coef[1])
	m.odds <- odds_ratio$logit2or(m.coef, m.coef[1])
	return(rbind(f.odds,m.odds))
}
table.3.fun("CLR")

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
