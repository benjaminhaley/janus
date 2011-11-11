# A list of terms and translations we can use in Janus data
# Ultimately these should derive from some central source/s
# But some may be locally hardcoded in the meanwhile.
# bmh oct 2011

# USAGE
# *note - assumes you have loaded 'data' object that fits this ontology
# source('../data/ontology.R')
# c <- ontology$load_columns()
# r <- ontology$load_rows(data)
# data[r$NEUTRON,c$MACROS_]
# 
# ...there are many other selectors shown below

# Load translations
source('../data/translations.R')

# TODO: this should include an option to turn off caching
#       in general caching should be controlled by the calling
#       script

# A namespace
ontology <- list()
ontology$j.t <- translations$load('janus', from_cache=FALSE)

ontology$load_columns <- function(){
	c <- list()
    #column cateogories
	c$MACROS <- ontology$.__get_macros(ontology$j.t)
	c$MACROS_ <- ontology$.__get_macros(ontology$j.t, with_lethality=TRUE)
	c$TUMORS <- ontology$.__tumors_from_macros(c$MACROS_)
	c$LETHAL_MACROS <- ontology$.__lethals_from_macros(c$MACROS_)
	c$LETHAL_TUMORS <- ontology$.__tumors_from_macros(c$LETHAL_MACROS)
	c$RADIATION_TYPE = "radn"
	c$FRACTIONS = "fractions"
	c$cGY_PER_MINUTE = "dose_rate"
	c$cGY_TOTAL = "total_dose"
	c$LIFESPAN_DAYS = "age"
	c$SPECIES = "species"
	c$SEX = "sex"
	c$MOCK_TREATED = "was_control_mock_treated"
	c$DEMOGRAPHICS = c(c$LIFESPAN_DAYS, c$SEX, c$SPECIES)
	c$TREATMENT = c("first_irrad", "total_dose", "radn", c$MOCK_TREATED, 
	 			    "fractions", "time_min", "dose_rate")
	c$AUTOPSY = c("necroscopy_date", "necrosopy_proctor")
	return(c)
}

ontology$load_rows <- function(data){
	r <- list()
	c <- ontology$load_columns()

    #types of radiation
	r$NEUTRON <- (data["radn"] == "N")
	r$GAMMA <- (data["radn"] == "G")
	r$CONTROL <- (data["radn"] == "C")

	#species
	r$MUS.M <- (data['species'] == "Mus musculus")
	r$PER.L <- (data['species'] == "Peromyscus leucopus")

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
	r$MACRO_COUNT <- rowSums(data[c$MACROS_])
	r$HAS_MACRO <- (r$MACRO_COUNT != 0)
	r$DIED_FROM_TUMOR <- rowSums(data[c$LETHAL_TUMORS])
	r$TUMOR_COUNT <- rowSums(data[c$TUMORS])

	# Genders
	r$MALE <- (data["sex"] == "M")
	r$FEMALE <- (data["sex"] == "F")
	return(r)
}

ontology$macro2snomed_description <- function(janus_codes, t=ontology$j.t){
	descriptions <- sapply(janus_codes, ontology$.__one_macro2snomed_description, t)
	return(descriptions)
}

ontology$macro2janus_description <- function(janus_codes, t=ontology$j.t){
	descriptions <- sapply(janus_codes, ontology$.__one_macro2janus_description, t)
	return(descriptions)
}

# Will take the specific terminology, macros with lethality
# codes and merge it into simple macros
ontology$merge_macros <- function(data, macros=ontology$.__get_macros(ontology$j.t)){
	macros_ <- ontology$.__add_lethalities(macros)
	macros_found <- macros_ %in% colnames(data)
	stopifnot(all(macros_found))
	merge <- data.frame(lapply(macros, ontology$.__merge_macro, data))
	return(merge)
}

# In janus cage numbers are in the ids
# for example id=10.1 cage=10 
ontology$id2cage <- function(ids){
	cage_number <- sub('\\..*', '', ids)
	return(cage_number)
}
# Test
stopifnot(identical(ontology$id2cage(c("10.2", "59.6")), c("10", "59")))

ontology$.__merge_macro <- function(macro, data){
	macros_ <- ontology$.__add_lethalities(macro)
	merge <- data[macros_[1]] | data[macros_[2]]
	colnames(merge) <- macro
	return(merge)
}

ontology$.__one_macro2snomed_description <- function(janus_code, t){
	janus_code <- ontology$.__strip_suffix(janus_code)
	macro_rows <- t[["observation_category"]] == "macro"
	code_rows <- t[["micro_code"]] %in% janus_code
	description_row <- macro_rows & code_rows
	is_found <- sum(description_row) == 1
	if(is_found){
		location <- as.character(t[description_row, "location"])
		description <- as.character(t[description_row, "description"])
		description <- paste(location, description, sep=" - ")
	} else {
		description <- "not found"
	}
	return(description)
}

ontology$.__one_macro2janus_description <- function(janus_code, t){
	janus_code <- ontology$.__strip_suffix(janus_code)
	macro_rows <- t[["observation_category"]] == "macro"
	code_rows <- t[["micro_code"]] %in% janus_code
	description_row <- macro_rows & code_rows
	is_found <- sum(description_row) == 1
	if(is_found){
		description <- as.character(t[description_row, "janus_description"])
	} else {
		description <- "not found"
	}
	return(description)
}

ontology$.__get_macros <- function(janus_translation_table, with_lethality=FALSE){
	macro_rows <- janus_translation_table["observation_category"]=="macro"
	# though (confusinginly) labeled 'micro_code' these are actually 'macros'
	# in this case
	macros <- janus_translation_table[macro_rows, "micro_code"]
	if(with_lethality==TRUE){
		macros <- ontology$.__add_lethalities(macros)
	}
	return(macros)
}

# If the first letter of a macro is a T, it is a tumor
ontology$.__tumors_from_macros <- function(macros){
	first_letter <- substr(macros, 1, 1)
	tumors <- macros[first_letter == "T"]
	return(tumors)
}

# Test
ontology$.__input <- c("TCOOL", "NCOL")
ontology$.__expected <- c("TCOOL")
ontology$.__result <- ontology$.__tumors_from_macros(ontology$.__input)
stopifnot(identical(ontology$.__result, ontology$.__expected))


# If the last letter is a L, it is lethal
ontology$.__lethals_from_macros <- function(macros){
	nchar <- nchar(macros)
	last_letter <- substr(macros, nchar, nchar + 1)
	tumors <- macros[last_letter == "L"]
	return(tumors)
}

# Test
ontology$.__input <- c("TCOO_L", "NCO_N")
ontology$.__expected <- c("TCOO_L")
ontology$.__result <- ontology$.__lethals_from_macros(ontology$.__input)
stopifnot(identical(ontology$.__result, ontology$.__expected))


ontology$.__add_lethalities <- function(macros){
	macros <- c(
		paste(macros, "L", sep="_"),
		paste(macros, "N", sep="_")
		)
	return(macros)
}

ontology$.__strip_suffix <- function(janus_code){
	prefix <- strsplit(janus_code, "_")[[1]][1]
	return(prefix)
}

# macro2descrptions Test
ontology$.__t <- data.frame(
	observation_category=c("micro", "macro", "macro", "macro"),
	micro_code=c("ABS", "ABS", "ADH", "COD"),
	location=c("lung", "heart", "brain", "membrane"),
	description=c("micro abs", "macro abs", "macro adh", "anything after the first _ should not count, i.e. _L")
	)
ontology$.__janus_codes <- c("ABS", "ADH", "MISSING", "COD_L")
ontology$.__expected <- c(
	ABS="heart - macro abs", ADH="brain - macro adh", MISSING="not found",
	COD_L="membrane - anything after the first _ should not count, i.e. _L")
ontology$.__result <- ontology$macro2snomed_description(ontology$.__janus_codes, ontology$.__t)
stopifnot(identical(ontology$.__expected, ontology$.__result))

# merge_macros Test
ontology$.__data <- data.frame(
	macro_L = c(TRUE, FALSE, FALSE),
	macro_N = c(FALSE, TRUE, FALSE),
	m_L = c(TRUE, FALSE, FALSE),
	m_N = c(FALSE, TRUE, FALSE)
	)
ontology$.__macros <- c("macro", "m")
ontology$.__expected <- data.frame(
	macro = c(TRUE, TRUE, FALSE),
	m = c(TRUE, TRUE, FALSE)
	)
ontology$.__result <- ontology$merge_macros(ontology$.__data, ontology$.__macros)
stopifnot(identical(ontology$.__expected, ontology$.__result))

