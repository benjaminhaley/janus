# A list of terms and translations we can use in Janus data
# Ultimately these should derive from some central source/s
# But some may be locally hardcoded in the meanwhile.
# bmh oct 2011

# USAGE
# *note - assumes you have loaded 'data' object that fits this ontology
# source('../data/ontology.R')
# c <- ontology$load_columns()
# r <- ontology$load_rows(data)
# data[r$NEUTRON,c$MACROS]
# 
# ...there are many other selectors shown below

# Load translations
source('../data/translations.R')
j.t <- translations$load('janus', from_cache=TRUE)

# A namespace
ontology <- list()

ontology$load_columns <- function(){
	c <- list()
    #column cateogories
	c$MACROS <- ontology$.__get_macros(j.t)
	c$RADIATION_TYPE = "radn"
	c$FRACTIONS = "fractions"
	c$cGY_PER_MINUTE = "dose_rate"
	c$cGY_TOTAL = "total_dose"
	return(c)
}

ontology$load_rows <- function(data){
	r <- list()
	c <- ontology$load_columns()

    #types of radiation
	r$NEUTRON <- (data["radn"] == "N")
	r$GAMMA <- (data["radn"] == "G")
	r$CONTROL <- (data["radn"] == "C")

	# Doses
	r$cGY_0        <- (data["total_dose"] == 0)
	r$cGY_2.05     <- (data["total_dose"] == 2.05) 
	r$cGY_7.69     <- (data["total_dose"] == 7.69)
	r$cGY_13.85    <- (data["total_dose"] == 13.85)
	r$cGY_21.54    <- (data["total_dose"] == 21.54)
	r$cGY_30.78    <- (data["total_dose"] == 30.78)
	r$cGY_37.68    <- (data["total_dose"] == 37.68)
	r$cGY_40.04    <- (data["total_dose"] == 40.04)
	r$cGY_100      <- (data["total_dose"] == 100)
	r$cGY_150.72   <- (data["total_dose"] == 150.72) 
	r$cGY_200      <- (data["total_dose"] == 200)
	r$cGY_300      <- (data["total_dose"] == 300)
	r$cGY_399.99   <- (data["total_dose"] == 399.99)
	r$cGY_450      <- (data["total_dose"] == 450)
	r$cGY_600      <- (data["total_dose"] == 600)
	r$cGY_1839.4   <- (data["total_dose"] == 1839.4)

    #dosing fractions
	r$FRACTIONS_60 <- (data["fractions"] == 60)

	# Autopsy results
	r$MACRO_COUNT <- rowSums(data[c$MACROS])
	r$HAS_MACRO <- (r$MACRO_COUNT != 0)
	return(r)
}

ontology$__get_macros(janus_translation_table){
	macro_rows <- janus_translation_table["observation_category"]=="macro"
	# though (confusinginly) labeled 'micro_code' these are actually 'macros'
	# in this case
	prefixes <- janus_translation_table[macro_rows, "micro_code"]
	macros <- c(
		paste(prefixes, "L", sep="_"),
		paste(prefixes, "N", sep="_")
		)
	return(macros)
}
	
