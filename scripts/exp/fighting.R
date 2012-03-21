# Is fighting a contagious disease?
# bmh 2011


# load the janus data
source('scripts/data/data.R')
source('scripts/data/ontology.R')

# Define the dataset under analysis
data <- data$load('janus', from_cache=TRUE)
merged_macros <- ontology$merge_macros(data)
data <- cbind(data, merged_macros)
r <- ontology$load_rows(data)
p.fit <- data[r$PER.L, "FIT"]
id <- rownames(data)
cage <- ontology$id2cage(id)
p.cage <- cage[r$PER.L]


# Generate histogram of the count of fighting
# deaths by cage
fit.count.by.cage <- aggregate(p.fit, by=list(p.cage), FUN=sum)[[2]]
pdf("analysis/fighting_deaths_by_cage.pdf")
hist(fit.count.by.cage)
dev.off()

# Generate a permuted graph for comparison
p2.fit <- sample(p.fit)
fit.count.by.cage <- aggregate(p2.fit, by=list(p.cage), FUN=sum)[[2]]
pdf("analysis/fighting_deaths_by_cage_permute.pdf")
hist(fit.count.by.cage)
dev.off()

# Note that fighting deaths are clustered by cages
# there are more cages with multiple fighting deaths
# than would be expected by chance (the permutation)
# this does not prove that fighting is contagious, but
# it fits with that model.  
# However, it is possible that one individual, inclined
# to fight, drives the clustering.
