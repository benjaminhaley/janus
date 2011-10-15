# An attempt to recreate Marissa's analysis in A Retrospective Analysis...
# should exercise the new janus system
# bmh Oct 2011

# Load the janus data
source('../data/data.R')
data <- data$load('janus', from_cache=TRUE)

# Some helper functions
source('../util/table_freq.R')

# Load the annotations
source('../data/ontology.R')
o <- ontology$load(data)

# Load translations
source('../data/translations.R')
t <- translations$load('janus', from_cache=TRUE)

# Define the dataset under analysis
CONTROL     <- (o$FRACTIONS_60 & o$HAS_MACRO & o$CONTROL)
G_100cGY    <- (o$FRACTIONS_60 & o$HAS_MACRO & o$GAMMA    & o$cGY_100)
G_200cGY    <- (o$FRACTIONS_60 & o$HAS_MACRO & o$GAMMA    & o$cGY_200)
G_300cGY    <- (o$FRACTIONS_60 & o$HAS_MACRO & o$GAMMA    & o$cGY_300)
G_450cGY    <- (o$FRACTIONS_60 & o$HAS_MACRO & o$GAMMA    & o$cGY_450)
G_600cGY    <- (o$FRACTIONS_60 & o$HAS_MACRO & o$GAMMA    & o$cGY_600)
N_2.05cGY   <- (o$FRACTIONS_60 & o$HAS_MACRO & o$NEUTRON  & o$cGY_2.05)
N_7.69cGY   <- (o$FRACTIONS_60 & o$HAS_MACRO & o$NEUTRON  & o$cGY_7.69)
N_13.85cGY  <- (o$FRACTIONS_60 & o$HAS_MACRO & o$NEUTRON  & o$cGY_13.85)
N_21.54cGY  <- (o$FRACTIONS_60 & o$HAS_MACRO & o$NEUTRON  & o$cGY_21.54)
N_30.78cGY  <- (o$FRACTIONS_60 & o$HAS_MACRO & o$NEUTRON  & o$cGY_30.78)
N_40.04cGY  <- (o$FRACTIONS_60 & o$HAS_MACRO & o$NEUTRON  & o$cGY_40.04)
ALL_TREATMENTS <- c(
	CONTROL |
	G_100cGY | G_200cGY | G_300cGY | G_450cGY | G_600cGY |
	N_2.05cGY | N_7.69cGY | N_13.85cGY | N_21.54cGY | N_30.78cGY | N_40.04cGY
	)

# Get counts of the treatment groups
counts <- table_freq(data[
			ALL_TREATMENTS,
			c("radn", "fractions", "total_dose", "dose_rate")
			])




))

