# The one stop shop where you go to get data.
# This script will contain confirgution settings to download a suite of data
# It is not very functional yet, just a namespace filler
# bmh Oct 2011

data <- list()

data$load <- function(datasets, from_cache=FALSE){
	source('scripts/data/load_janus_data.R')
	source('scripts/data/load_beagle_data.R')
	j.data <- j.data$load(from_cache)
	b.data <- b.data$load(from_cache)
	
	j.data <- data$j.normalize(j.data)
	b.data <- data$b.normalize(b.data)
	
	data <- merge(j.data, b.data, all=TRUE)

	return(data)
}
	
data$b.normalize <- function(b.data) {
	# replace NAs as useful
	na.to.zeros <- c(
		"fractions", "exposure_time", "first_exposure_age", "dose_per_minute", 
		"dose_per_fraction", "total_dose", "first_exposure_age", "daily_dose",
		"days_exposed", "first_exposure_age.1", "total_dose.1"
	)
	for (n.2.z in na.to.zeros) {
		b.data[is.na(b.data[,n.2.z]), n.2.z] <- 0
	}
	b.data[b.data$fractions == 0, 'fractions'] <- 1      # fraction default is 1
	
	# add/convert names
	b.data[['id']]                      <- '??'
	b.data[['species']]                 <- 'beagle'
	b.data[['cause_of_death']]          <- ''
	b.data[['data_set']]                <- 'beagle'
	b.data[['experiment']]              <- b.data[['abridged_Name']]
	b.data[['first_exposure_age_days']] <- b.data[['first_exposure_age']] + 
								           b.data[['first_exposure_age.1']]
	b.data[['cgy_per_min_neutron']]     <- 0
	b.data[['cgy_per_min_gamma']]       <- b.data[['dose_per_minute']] + 
								           b.data[['daily_dose']] / (22 * 60)
	b.data[['cgy_total_neutron']]       <- 0
	b.data[['cgy_total_gamma']]         <- b.data[['total_dose']] + 
								           b.data[['total_dose.1']]
	b.data[['fractions_neutron']]       <- 1
	b.data[['fractions_gamma']]         <- b.data[['fractions']]
	b.data[['age_days']]                <- b.data[['age']]
								 				 
	# remove unneeded names
	drops   <- c(
		"abridged_Name", "birth_date", "daily_dose", "days_exposed", "dose_per_fraction",
		"dose_per_minute", "exposure_time", "first_exposure_age", "first_exposure_age.1",
		"total_dose.1", "X", "total_dose", "fractions", "age", "interrupts"
	)
	b.data  <- b.data[,!(names(b.data) %in% drops)]

	return(b.data)
}

# Convert types where needed
#
data$j.normalize <- function (j.data) {
	# Dump pathologies
	j.data <- j.data[1:18]
	
	# add/convert names
	j.data[['id']]                      <- rownames(j.data)
	j.data[['age_days']]                <- j.data[['age']]
	j.data[['experiment']]              <- j.data[['expt']]
	j.data[['group']]                   <- j.data[['tmt']]
	j.data[['first_exposure_age_days']] <- j.data[['first_irrad']]
	b.data[['data_set']]                <- 'janus'

	j.data[['cgy_per_min_gamma']]                     <- 0
	j.data[j.data$radn == "G", 'cgy_per_min_gamma']   <- j.data[j.data$radn == "G", 'dose_rate']
	j.data[['cgy_per_min_neutron']]                   <- 0
	j.data[j.data$radn == "N", 'cgy_per_min_neutron'] <- j.data[j.data$radn == "N", 'dose_rate']
	j.data[['cgy_total_gamma']]                       <- 0
	j.data[j.data$radn == "G", 'cgy_total_gamma']     <- j.data[j.data$radn == "G", 'total_dose']
	j.data[['cgy_total_neutron']]                     <- 0
	j.data[j.data$radn == "N", 'cgy_total_neutron']   <- j.data[j.data$radn == "N", 'total_dose']
	j.data[['fractions_gamma']]                       <- 0
	j.data[j.data$radn == "G", 'fractions_gamma']     <- j.data[j.data$radn == "G", 'fractions']
	j.data[['fractions_neutron']]                     <- 0
	j.data[j.data$radn == "N", 'fractions_neutron']   <- j.data[j.data$radn == "N", 'fractions']
	
	j.data[j.data$species == "Mus musculus", 'species']         <- 'musculus'
	j.data[j.data$species == "Peromyscus leucopus", 'species']  <- 'peromyscus'

	# remove unneeded names
	drops   <- c(
		"autopsy_type", "Comment", "has_micro", "necroscopy_date", "necrosopy_proctor", 
		"tmt", "was_control_mock_treated", "age", "dose_rate", "expt", "first_irrad",
		"fractions", "total_dose", "radn", "time_min"
	)
	j.data  <- j.data[,!(names(j.data) %in% drops)]
	
	return(j.data)
}