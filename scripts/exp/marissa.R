# An attempt to recreate Marissa's analysis in A Retrospective Analysis...
# should exercise the new janus system
# bmh Oct 2011

# Load the janus data
source('../data/data.R')
data <- data$load('janus', from_cache=TRUE)

# Some helper functions
source('../util/freq.R')

# Load the annotations
source('../data/ontology.R')
c <- ontology$load_columns()
r <- ontology$load_rows(data)

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
CON <- data["treatment"] == "CONTROL"
EXP <- ALL & !CON

# We are not discriminating between Lethal and Non-lethal
MERGED_MACROS <- ontology$merge_macros(data)
COMMON_TOXICITIES <- freq$get_columns(MERGED_MACROS[ALL,], c$MACROS, minimum_sum=30)

# Get counts of the treatment groups
table.1 <- freq$get_table(data[
			ALL,
			c("radn", "fractions", "total_dose", "dose_rate")
		])

# Get toxicity descriptions
table.2 <- ontology$macro2description(COMMON_TOXICITIES)

# Start some model building
MERGED_MACROS[ALL,"OVE"]

# @TODO add km-curves
