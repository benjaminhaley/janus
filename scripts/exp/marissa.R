# An attempt to recreate Marissa's analysis in A Retrospective Analysis...
# should exercise the new janus system
# bmh Oct 2011

# Load the janus data
source('../data/load_janus_data.R')
data <- j.data$load(from_cache=TRUE)

# Load the annotations
source('../data/ontology.R')
o <- ontology$load(data)


