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
c <- ontology$load_columns()
r <- ontology$load_rows(data)

# Load translations
source('../data/translations.R')
t <- translations$load('janus', from_cache=TRUE)

# Define the dataset under analysis
CONTROL     <- (r$FRACTIONS_60 & r$HAS_MACRO & r$CONTROL)
G_100cGY    <- (r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_100)
G_200cGY    <- (r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_200)
G_300cGY    <- (r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_300)
G_450cGY    <- (r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_450)
G_600cGY    <- (r$FRACTIONS_60 & r$HAS_MACRO & r$GAMMA    & r$cGY_600)
N_2.05cGY   <- (r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_2.05)
N_7.69cGY   <- (r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_7.69)
N_13.85cGY  <- (r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_13.85)
N_21.54cGY  <- (r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_21.54)
N_30.78cGY  <- (r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_30.78)
N_40.04cGY  <- (r$FRACTIONS_60 & r$HAS_MACRO & r$NEUTRON  & r$cGY_40.04)
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

