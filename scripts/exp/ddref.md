[DDREF][public link]
========================================================
Measure ddref, for my thesis.  
benjamin.haley@gmail.com  
*last update: April 2014*

[public link]: http://rpubs.com/benjaminhaley/ddref


# Abstract
TODO(ben)

<a name="contents"></a>

# Table of contents

- Background 
  - [Defining DDREF](#defining_ddref) - What is the equation?
  - [Log Likelihood](#loglike) - How is log likelihood calculated?
  - [Metaregression](#metaregression) - Show the principals of metaregression.
- Data
  - [Data Funnel](#data_funnel) - Which data will we analyze?
  - [Data Cleaning](#cleaning) - Damn, data, you look good!
  - [Concordance](#concordance) - Tables describing the data in detail.
  - [Visualize](#visual_concordance) - Show what the data looks like.
  - [Atomic Bomb Survivors](#lss) - Load data from atomic bomb survivors as well.
- Analysis
  - [Reproduce BEIR 10B3](#10B3) - Show that we can fit the same model of
    lifespan vs. dose as oak ridge.
  - [Reproduce BEIR 10B4](#10B4) - Show that we can reproduce the
    likelihood profiles from BEIR VII.
  - [10B3 on all data](#10B3-all-data) - Fit 1/lifespan models on all of the
    data.
  - [10B4 on all data](#10B4-all-data) - 1/lifespan models profile likelihoods 
    on all of the data.
  - [10B3 with metaregression](#10B3-meta) - Reproduce dose response figure
    applying the principals of meta-regression
  - [10B4 with metaregression](#10B4-meta) - Reproduce profile likelihood figure
    applying the principals of meta-regression.
  - [10B3 metareression on all data](#10B3-meta-all) Apply meta regression to
    all of the datasets.
  - [10B4 metareression on all data](#10B4-meta-all) Apply meta regression to
    all of the datasets to generate profiles.

_____________________________________________________________________________  





<a name="defining_ddref"></a>

Defining DDREF
========================================================
*Last update: April 2013*

##### What is DDREF?
DDREF is defined ambiguosly.  It can be derived from acute
exposures as 1 + Dβ/α  where response ~  D*α + D*β^2.  But for 
the purposes of radiation protection, we need a functional 
definition, that distinguishes a cutoff of dose and doserate
beyond which the DDREF correction ought to be used.  What are 
these cutoffs?  I will do a literature search to find out.

#### Notes
*ICRP 2007 3.2.1 (70-73)*  
DDREF is 2 based on LLS dose response curves, 'experimental
data', and 'probabilistic uncerainty analysis' conducted by others
(NCRP, 1997, EPA, 1999, NCI/CDC, 2003, Annex A).

*ICRP 2007 A.3.1 (A 62)*  
When dose rates are lower than around 0.1 Gy/hour there is repair 
of cellular radiation injury during the irradiation. This causes 
the b component to decrease and to reach zero at very low dose 
rates. The a component is not modiﬁable by changing dose rate. 

*BEIR VII Chapter 2*
- How B decreases with dose rate (Edwards and others 1989)
- Estimate DDREF from animal data (Tucker and others 1998)
- More DDREF from animals data (Lorenz and others 1994)
- More DDREF in animals studies (Ullrich and Storer 1979a; Ullrich and others 1987)

*BEIR VII Chapter 10*  
DREF is the term for dose rate reduction, as opposed to both
dose and dose rate reduction

- pg 246 claims that up to 24 hours seems to be required for
       full repair from a single dose
- pg 248 Says LSS DDREF are roughly equivilant to UNSCEAR DDREF
       estimated at 1 Sv of exposure.
- pg 250 used a cutoff of 1.5 - 2 Gy for animal data to avoid
       leveling off effects!!!!!!
- pg 254 when fractionated, dose response can be described as
       a*D + B*(D^2/K) where K is the number of fractions
- pg 255 they were forced to use mean survival times, instead of
       lifespan, because they did not have individual level data!
       They admit this is problematic, something I can address!
- pg 255 importantly they ignore data that does not account for 
       competing sources of risk.  I do not know if I can do this!
         But perhaps this is rather irrelevant in the case of lifespan
         most of these studies 
- pg 255 They only used the accute exposures from Edwards (1992)
       excluding tables 1 and 2

#### References

- Edwards 1989, Chromosome aberrations in human lymphocytes
- Edwards 1992, Low Dose and Low Dose Rate Effects in Laboratory Animals
- Lorenz 1994, Dose and dose-rate dependence of the frequency of hprt deficient 
  T lymphocytes in the spleen of the 137Cs gamma-irradiated mouse
- Tucker 1998, The accumulation of chromosome aberrations and Dlb-1 mutations in 
  mice with highly fractionated exposure to gamma radiation
- Ullrich 1979a, Influence of gamma irradiation on the development of neoplastic 
  disease in mice. I. Reticular tissue tumors
- Ullrich 1987, Myeloid leukemia in male RFM mice following irradiation with 
  fission spectrum neutrons or gamma rays.

#### Conclusions

1. Survival hazard was not used in BEIR VII, only mean lifespan.
2. BEIR VII used 1.5 Sv as an upper threshold, because response
   falls off above this level, I should too.
3. When doses are fractionated apply the equation:
   `a*D + B*(D^2/K)`, where K is the number of fractions
4. Dealing with doserate changes is hard, pg 246 suggests that
   repair processes take up to 24 hours, so this might be a
   natural break point.
5. Estimate DDREF at 1 Sv to make it compatible with lss estimates

_____________________________________________________________________________  
^ back to [table of contents](#contents)






<a name="data_funnel"></a>

DATA: Data Funnel
========================================================
*Last update: April 2013*

### Introduction:
Create a data set that might be used for DDREF analysis and
make a description of this data.


```r
# LIBRARIES
library(plyr)
library(dplyr)
library(ggplot2)
library(survival)

# DATA
setwd("~/janus/scripts")
d <- readRDS("../data/external5.rds")

# HELPERS Make survival data amenable to ggplot
table0 <- function(...) table(..., useNA = "ifany")

# Report for funnel graph
count <- function(data) {
    count_unique <- function(x) length(unique(x))
    with(data, c(studies = count_unique(file), clusters = count_unique(cluster), 
        treatments = count_unique(group_id), animals = count_unique(id), `not vetted` = count_unique(id) - 
            count_unique(id[is_vetted]), `to exclude` = count_unique(id[exclude])))
}

filter_by_n_groups <- function(data, threshold = 3) {
    ddply(data, .(cluster), function(df) {
        n_groups = length(unique(paste(df$dose, df$dose_rate, df$fractions)))
        if (n_groups >= threshold) {
            return(df)
        } else {
            return(NULL)
        }
    })
}

# Define
bad_qualities <- c("accel. alpha local", "accel. alpha whole body", "accel. neutrons 0.1-10 MeV", 
    "neutrons 1-10 MeV", "neutrons C-252", "neutrons fission", "neutrons>10 MeV", 
    "X-rays local", "gamma-rays local", "Bremsstrahlung > 3MeV.")

# Aliases Allow a more concise representation of a name.  For instace ♂ is
# preferable to Male
aliases <- list(quality = c(`gamma-rays Co-60` = "γ-ray", `gamma-rays Co-60, gamma-rays Co-60` = "γ-ray", 
    `gamma-rays Co-60, gamma-rays Co-60, gamma-rays Co-60` = "γ-ray", `gamma-rays Cs-137` = "γ-ray", 
    `gamma-rays whole body` = "γ-ray", `gamma-rays` = "γ-ray", `X-rays whole body` = "X-ray"), 
    sex = c(Both = "♂/♀", Female = "♀", Male = "♂"), lab = c(`2` = "CEN-FAR", 
        `3` = "ENEA", `9` = "SCK/CEN", `11` = "TNO", `1002` = "DAVIS", `1003` = "ANL", 
        `1005` = "ITRI", `1007` = "ORNL", `1008` = "CSU"), strain = c(beagle = "Beagle"))


threshold_dose <- 1.5


# Fix TODO(later) move these fixes to radiation.R

# Stray fixes Some of the B6CF1 mice are missing their species
d[d$strain == "B6CF1", "species"] <- "Mouse"
## One animal is listed as a control despite having a dose, remove her
contradictory_dose_and_quality <- (d$quality == "none (controls)" & d$dose != 
    0 & !is.na(d$dose) & !is.na(d$quality))
d <- d[!contradictory_dose_and_quality, ]

# NA doses
d$dose[is.na(d$dose)] <- 0
d$dose_rate[is.na(d$dose_rate)] <- 0
d$fractions[d$dose == 0] <- 0
d$dose_rate[d$dose == 0] <- 0

# NA quality
d$quality[is.na(d$quality)] <- "none (controls)"

# Add missing fractions
d$fractions[is.na(d$fractions)] <- 1
d$fractions[d$fractions == 0] <- 1

# Add fractions seperated by days
d$day_fractions <- d$fractions
s <- d$fraction_interval < 1 & !is.na(d$fraction_interval)
d$day_fractions[s] <- d$fractions[s] * d$fraction_interval[s]

# Add lab
d$lab <- sub("(^[0-9]*).*$", "\\1", d$study_id)

# If assignment age is not listed, assume it is zero
d$assignment_age[is.na(d$assignment_age)] <- 0

# If an animal is a control, it should have no age at treatment Nor an age
# at last treatment
d$age_at_treatment[d$dose == 0] <- NA
d$age_at_last_treatment[d$dose == 0] <- NA

# Age at last treatment
d$age_at_last_treatment <- d$age_at_treatment
s <- !is.na(d$fraction_interval)
d$age_at_last_treatment[s] <- with(d[s, ], age_at_treatment + fraction_interval * 
    (fractions - 1), )

# Assign aliases Replace all values in a given column with their aliases
# e.g. replace gamma-rays with γ
for (column in names(aliases)) {
    for (name in names(aliases[[column]])) {
        alias = aliases[[column]][name]
        d[d[column] == name & !is.na(d[column]), column] <- alias
    }
}
```


#### Define Clusters
In general we want to cluster on:

  `lab, species, strain, and sex`

```r
d$cluster = with(d, paste(sex, strain, species, lab, sep = "--"))
```

But also

    `assignment_age and quality`

Which require special consideration.
 
##### Intended Assignment Age
Assignment age was usually recorded 'as intended'.  So that all mice in a group have an assignment age of 56 days old.  Such precision is dubious/impossible and most likely represents a reconstruction based on the methods described about the experiment.

By contrast, argonne data recorded true age at treatment assignment so that animals vary by up to 50 days within a single cluster.  These animals should not be divided into seperate clusters, because they are all adults.  By contrast animals irradiated at -4 days and 7 days old should be put into seperate age clusters because they represent very different stages of development.

The most complete way to handle this situation is do define a lifestage by age for each species and use this for clustering.  But this approach is arbitrary, contrived, and needlessly complex.

Instead we will define a new feild, `approximate_assignment_age`.  For most groups this will be the reported `assignment_age`.  For argonne groups we will define it by the median `assignment_age`.


```r
# TODO: Cluster concordance should be labeled with the unique studies or
# labs that the cluster represents TODO: Group listings within a cluster
# should be in a good order TODO: dose estimates within cluster groups
# should have two signficant digits
d$intended_assignment_age <- d$assignment_age
labs_that_recorded_true_age_at_assignment <- c("ANL")
clusters_that_recorded_true_age_at_assignment = unique(d$cluster[d$lab %in% 
    labs_that_recorded_true_age_at_assignment])
for (c in clusters_that_recorded_true_age_at_assignment) {
    d$intended_assignment_age[d$cluster == c] <- median(d$assignment_age[d$cluster == 
        c])
}
d$cluster <- paste(d$cluster, d$intended_assignment_age, sep = "--")
```


##### Duplicate controls
Control animals may control for multiple clusters.  For example the same mouse could control for a group exposed to gamma rays and others exposed to x-rays.  Therefore control groups ought to be duplicated and included in each cluster that they might control for.

Concretely, control animals should match sex, species, strain, assignment age, and lab.  Controls should be duplicated for each unique quality and age of first exposure provided the aforementioned criteria are met.


```r
d <- ddply(d, .(cluster), function(df) {
    control <- df[df$quality == "none (controls)", ]
    treatment <- df[df$quality != "none (controls)", ]
    
    # Create a cluster for each non control intended age of treatment
    ddply(treatment, .(quality), function(treatment_group) {
        
        # Add control to each treatment group
        df2 <- rbind(treatment_group, control)
        
        # Define quality by the treatment group i.e.  quality = 'none (control)'
        # -> quality = 'gamma'
        df2$quality <- treatment_group$quality[1]
        
        # Add quality to the cluster name
        df2$cluster <- with(df2, paste(cluster, quality, sep = "--"))
        df2
    })
})

# Label the duplicates
d$duplicates <- duplicated(d$id)

# Show that all of them recieved 0 dose
with(d, all(dose[duplicates] == 0))
```

```
## [1] TRUE
```

```r

# Count the number of duplicates
table(d$duplicates)  # 23657
```

```
## 
##  FALSE   TRUE 
## 116542  23657
```




```r

# FILTER

# Initial counts
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         24         82        827     116542      60118      15465
```

```r

# Only low-LET, whole body
d <- d[!d$quality %in% bad_qualities, ]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         24         43        457      76096      23213      15465
```

```r

# Dose below threshold (as in BEIR VII)
d <- d[!(d$dose > threshold_dose), ]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         23         35        230      45730       6220       9662
```

```r

# Lifespan not NA
d <- d[!is.na(d$lifespan), ]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         23         35        230      45722       6220       9662
```

```r

# No other treatments
d <- d[d$other_treatments == "none", ]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         21         34        175      43043       4832       8516
```

```r

# Died before their 'assignment age' TODO(later) why should there be any
# mice that died before their assignemtn age?
d <- d[d$lifespan > d$assignment_age, ]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         21         34        175      42948       4797       8509
```

```r

# Remove those that should be excluded
exclusions <- sort(unique(d$reason))
exclusions <- exclusions[exclusions != ""]
for (ex in exclusions) {
    d <- d[!d$reason == ex, ]
    print(ex)
    print(count(d))
}
```

```
## [1] "see exclusion-1 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         21         34        174      42941       4797       8502 
## [1] "see exclusion-10 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         21         33        142      40906       4797       6467 
## [1] "see exclusion-11 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         20         32        138      39803       4797       5364 
## [1] "see exclusion-12 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         20         32        137      39506       4797       5067 
## [1] "see exclusion-13 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         19         32        136      39440       4797       5001 
## [1] "see exclusion-5 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         18         31        133      39419       4797       4980 
## [1] "see exclusion-7 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         18         31        132      35706       4797       1267 
## [1] "see exclusion-8 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         17         31        129      34997       4797        558 
## [1] "see exclusion-9 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##         16         27        119      34439       4797          0
```

```r

# Remove cases with few treatment groups
d <- filter_by_n_groups(d)
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##          8         17         95      29072          0          0
```

```r

# studies clusters treatments animals not vetted to exclude 8 17 95 29072
# 0 0

# How many duplicates after filtering?
table0(d$duplicates)  # 19462 FALSE 9610 TRUE
```

```
## 
## FALSE  TRUE 
## 19462  9610
```

```r

# Warnings show, but do not remove
warnings <- sort(unique(d$warning_reason))
warnings <- warnings[warnings != ""]
d_wo_warnings <- d
for (w in warnings) {
    d_wo_warnings <- d_wo_warnings[!d_wo_warnings$warning_reason == w, ]
    print(w)
    print(count(d_wo_warnings))
}
```

```
## [1] "see warning-1 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##          7         15         83      20639          0          0 
## [1] "see warning-2 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##          7         15         82      20209          0          0 
## [1] "see warning-3 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##          7         15         80      19905          0          0 
## [1] "see warning-4 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##          6         14         77      19658          0          0 
## [1] "see warning-5 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##          6         14         76      19305          0          0 
## [1] "see warning-6 in radiation.R"
##    studies   clusters treatments    animals not vetted to exclude 
##          6         14         75      18995          0          0
```

```r
d_wo_warnings <- filter_by_n_groups(d_wo_warnings)
count(d_wo_warnings)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##          6         13         73      18903          0          0
```

```r
# studies clusters treatments animals not vetted to exclude 6 13 73 18903
# 0 0

# Save
setwd("~/janus/scripts")
saveRDS(d, "../data/funneled.rds")
```

___________________________________________________________________
^ back to [table of contents](#contents)





<a name="cleaning"></a>

Clean
========================================================
DDREF data has been filtered.  Now its time to prettify it.


```r
# Libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(survival)

# Data
setwd("~/janus/scripts")
d <- readRDS("../data/funneled.rds")

# Helpers
pluralize <- function(x) {
    c(Mouse = "Mice", Rat = "Rats", Dog = "Dogs", Peromyscus = "Peromyscus")[x]
}

# Is acute?
d$acute <- d$fractions == 1 | d$dose == 0
d$protracted <- d$fractions > 1 & d$dose > 0

# Observations per cluster
d = d %.% group_by(cluster) %.% mutate(n_in_cluster = length(cluster))

# Give the clusters pretty names
for (c in unique(d$cluster)) {
    elements = as.list(strsplit(c, "--")[[1]])
    names(elements) <- c("sex", "strain", "species", "lab", "age", "quality")
    pretty_cluster = with(elements, paste(sex, strain, pluralize(species), lab, 
        "\n", quality, "at", age, "days old"))
    d$cluster[d$cluster == c] <- pretty_cluster
}

# Define Acute
chronic <- d$fractions > 1
d$type <- "A"
d$type[chronic] <- "C"

# Order clusters By number of observations.  This will put the cluster
# with the most observations first in ggplots
sort_by_n <- function(x) {
    factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}
d$cluster = sort_by_n(d$cluster)
d$cluster_id <- as.numeric(d$cluster)
d$cluster <- with(d, paste0(cluster_id, " - ", cluster))
d$cluster = sort_by_n(d$cluster)

# Save Data for later use
saveRDS(d, "data/ddref.rds")
write.csv(d, file = "data/ddref.csv")
```


### Results
Data is so fresh and so clean.



<a name="concordance"></a>

Concordance
========================================================
*Last update: April 2013*

Give a detailed description of the dataset for those that want to make a close inspection.



```r

# Libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(survival)

# Data
setwd('~/janus/scripts')
d <- readRDS('data/ddref.rds')

# Helpers
group_summary <- function(data){
    .all <- function(x, c=' ') paste(unique(x), collapse=c)
    summary <- with(data, c(
        ' ♂ '          = sum(sex == '♂'),
        ' ♀ '          = sum(sex == '♀'),
        'avg. age'      = round(mean(lifespan)),
        'dose'          = .all(signif(dose, 2)),
        'rate'          = .all(signif(dose_rate, 1)),
        '# fractions'   = .all(fractions),
        warnings        = .all(gsub('[^0-9]', '', warning_reason))
    ))
    summary[summary == 0] <- '-'
    summary[is.na(summary)] <- '-'  
    summary
}

find_in_file <- function(pattern, file='exp/radiation.R'){
		lines <- readLines(file)
		lines[grepl(pattern, lines)]
}
```


#### Group details
Show the details of each treatment group in the dataset organized by cluster


```r
# TODO Make sure these are in the correct order
for (id in sort(unique(d$cluster_id))) {
    df <- d[d$cluster_id == id, ]
    cluster <- as.character(df$cluster[1])
    cat("\n", cluster, "\n------------------------------------------------\n")
    s <- ddply(df, .(group_id), function(df) {
        group_summary(df)
    })
    s <- s %.% mutate(study = sub("^[0-9]*-", "", group_id), group = as.numeric(sub("^[0-9]*-", 
        "", study)), study = as.numeric(sub("-[0-9]*$", "", study))) %.% arrange(study, 
        group) %.% select(-study, -group)
    print(s, row.names = FALSE)
}
```

```
## 
##  1 - ♀ RFM/Un Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##   group_id  ♂    ♀  avg. age dose rate # fractions warnings
##   1007-3-9   - 2696      632  0.1  0.4           1        1
##  1007-3-10   -  930      614 0.25  0.4           1        1
##  1007-3-11   - 1064      553  0.5  0.4           1        1
##  1007-3-12   -  237      541 0.75  0.4           1        1
##  1007-3-13   - 1045      538    1  0.4           1        1
##  1007-3-14   - 1005      487  1.5  0.4           1        1
## 
##  2 - ♀ B6CF1 Mice ANL 
##  γ-ray at 114 days old 
## ------------------------------------------------
##   group_id  ♂    ♀  avg. age dose  rate # fractions warnings
##  1003-20-2   -  857      945    -     -           1         
##  1003-20-4   -  397      903 0.86  0.04           1         
##  1003-21-2   -  185      932    -     -           1         
##  1003-21-4   -  200      936 0.86  0.04           1         
##  1003-22-2   -  464      970    -     -           1         
##  1003-24-2   -  175      991    -     -           1         
##  1003-25-2   -   50      960    -     -           1         
##  1003-26-2   - 1138      978    -     -           1         
##  1003-26-3   -  497      963 0.22  0.01           1         
##  1003-26-4   -  346      968 0.43  0.02           1         
##  1003-26-5   -  194      935 0.86  0.04           1         
##  1003-29-2   -  584      986    -     -           1         
##  1003-29-4   -  598      957    1 8e-04          60         
##  1003-30-2   -  399      977    -     -           1         
## 
##  3 - ♂ B6CF1 Mice ANL 
##  γ-ray at 113 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose  rate # fractions warnings
##  1003-20-1 843   -      952    -     -           1         
##  1003-20-3 386   -      922 0.86  0.04           1         
##  1003-21-1 200   -      985    -     -           1         
##  1003-21-3 199   -      970 0.86  0.04           1         
##  1003-21-5 160   -      939  1.4  0.07           1         
##  1003-22-1 557   -      985    -     -           1         
##  1003-24-1 310   -      987    -     -           1        6
##  1003-25-1  60   -     1011    -     -           1         
##  1003-26-1 200   -     1043    -     -           1         
##  1003-28-1 120   -     1020    -     -           1         
##  1003-29-1 592   -      993    -     -           1         
##  1003-29-3 594   -      971    1 8e-04          60         
##  1003-30-1 393   -     1007    -     -           1         
## 
##  4 - ♂ C57BL/Cnb Mice SCK/CEN 
##  γ-ray at 84 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##     9-6-1 467   -      613    -    -           1         
##     9-6-2 241   -      581 0.25  0.3           1         
##     9-6-3 236   -      564  0.5  0.3           1         
##     9-6-4 241   -      550    1  0.3           1         
##     9-6-8 107   -      605 0.25  0.3          10         
##     9-6-9 109   -      604  0.5  0.3          10         
##    9-6-10 115   -      615    1  0.3          10         
##    9-6-14 104   -      622    1  0.3           8         
## 
##  5 - ♂ RFM/Un Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##  1007-3-1 430   -      711    -    -           1        1
##  1007-3-2 256   -      720  0.1  0.4           1        1
##  1007-3-3  94   -      711 0.25  0.4           1        1
##  1007-3-4 247   -      680  0.5  0.4           1        1
##  1007-3-5 230   -      673    1  0.4           1        1
##  1007-3-6 199   -      651  1.5  0.4           1        1
## 
##  6 - ♂ BALB/c/Cnb Mice SCK/CEN 
##  γ-ray at 84 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##     9-5-1 322   -      766    -    -           1         
##     9-5-2 191   -      745 0.25    4           1        3
##     9-5-3 194   -      736  0.5    4           1         
##     9-5-4 191   -      732    1    4           1         
##     9-5-8 111   -      778 0.25    4          10         
##     9-5-9 110   -      740  0.5    4          10         
##    9-5-10 113   -      751    1    4          10        3
## 
##  7 - ♀ BC3F1 Mice ENEA 
##  X-ray at 91 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##     3-1-1   - 353      889    -    -           1        5
##     3-1-2   - 100      912 0.04 0.06           1         
##     3-1-3   -  84      893 0.08 0.06           1         
##     3-1-4   -  53      854 0.16 0.06           1         
##     3-1-5   -  58      874 0.32 0.06           1         
##     3-1-6   -  57      833 0.64  0.6           1         
##     3-1-7   -  60      707  1.3  0.6           1         
##     3-1-9   - 279      865    -    -           1         
## 
##  8 - ♂ C57BL/6Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##  1007-2-2 502   -      906    -    -           1         
##  1007-2-4 254   -      909  0.5  0.4           1         
##  1007-2-6 260   -      922    1  0.4           1         
## 
##  9 - ♀ C3Hf/Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
##   1007-2-9   - 501      778    -    -           1         
##  1007-2-11   - 249      727  0.5  0.4           1         
##  1007-2-13   - 250      693    1  0.4           1         
## 
##  10 - ♀ C57BL/6Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##  1007-2-1   - 491      858    -    -           1         
##  1007-2-3   - 253      855  0.5  0.4           1         
##  1007-2-5   - 251      865    1  0.4           1         
## 
##  11 - ♂ C3Hf/Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
##  1007-2-10 502   -      732    -    -           1         
##  1007-2-12 244   -      713  0.5  0.4           1         
##  1007-2-14 248   -      721    1  0.4           1         
## 
##  12 - ♂ leucopus Peromyscus ANL 
##  γ-ray at 137 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age   dose  rate # fractions warnings
##  1003-27-1 210   -     1388      -     -           1         
##  1003-27-2 203   -     1461      -     -           1         
##  1003-27-3 189   -     1358 0.0086 4e-04           1         
##  1003-27-4 181   -     1350  0.014 7e-04           1         
## 
##  13 - ♂ BC3F1 Mice ENEA 
##  X-ray at 92 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##    3-5-19 430   -      824    -    -           1        2
##    3-5-20  44   -      828  0.5  0.1           1         
##    3-5-21  48   -      797    1  0.1           1         
## 
##  14 - ♂ C57BL/Cnb Mice SCK/CEN 
##  X-ray at 7 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##     9-7-1 105   -      757    -    -           1        4
##    9-7-10  72   -      777  0.5    1           1        4
##    9-7-11  70   -      810    1    1           1        4
## 
##  15 - ♂ BC3F1 Mice ENEA 
##  X-ray at -4 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##     3-5-1  34   -      853    -    -           1         
##     3-5-3  48   -      799  0.3  0.1           1         
##     3-5-5  61   -      822  0.9  0.1           1         
##     3-5-7  46   -      897  1.5  0.1           1         
## 
##  16 - ♀ BC3F1 Mice ENEA 
##  X-ray at -4 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##     3-5-2   -  39      866    -    -           1         
##     3-5-4   -  40      883  0.3  0.1           1         
##     3-5-6   -  44      850  0.9  0.1           1         
##     3-5-8   -  50      872  1.5  0.1           1         
## 
##  17 - ♂ BC3F1 Mice ENEA 
##  X-ray at 580 days old 
## ------------------------------------------------
##  group_id  ♂   ♀  avg. age dose rate # fractions warnings
##    3-5-35  41   -      886    -    -           1         
##    3-5-36  42   -      901  0.5  0.1           1         
##    3-5-37  43   -      874    1  0.1           1
```


#### Warnings
Some issues were found when digging through the input data that were not judged to be severe enough to exclude the data, but do exemplify deviations from our expectations.  These are listed as follows.


```r
# Warnings are listed in radiation.R on lines that start with the
# following prefix
prefix <- "# warning-"

# Get a list of all the warnings relevant to this dataset
warnings <- gsub("[^0-9]", "", unique(d$warning_reason))
warnings <- sort(as.numeric(warnings[warnings != ""]))

# Find the corresponding warning definition in radiation.R
warning_prefixes <- paste0(prefix, warnings)
for (p in warning_prefixes) cat(find_in_file(p), "\n")
```

```
## Error: cannot open the connection
```


#### Study Details
Here are a description of each of the original studies used in this analysis as provided by the 'Gray Book' ([Gerber et al. 1996](#gerber_1996)).  Studies can easily be found by lab and study id which are the first two parts of the group id.  Concretely, a group id of 1007-3-6 is the sixth groups from the third study conducted at lab 1007, Oak Ridge National Laboratory.

TODO(later) summarize this information in a table

##### 3-1

![][3-1]  
More detail in ([Covelli 1988][Covelli 1988])

[3-1]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-24%2020.59.22.png
[Covelli 1988]: http://dl.dropbox.com/u/1131693/bloodrop/3577210.pdf


##### 3-5

![][3-5]  
More detail in ([Covelli 1984][Covelli 1984])

[3-5]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.25.17.png
[Covelli 1984]: http://dl.dropbox.com/u/1131693/bloodrop/3576356.pdf 


##### 9-5

![][9-5]  
More detail in ([Maisin 1983][Maisin 1983])

[9-5]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.28.32.png
[Maisin 1983]: http://dl.dropbox.com/u/1131693/bloodrop/3575970.pdf 


##### 9-6

![][9-6]  
![][9-6-2]  
More detail in ([Author Date][Author Date])

[9-6]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.29.07.png
[9-6-2]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.29.42.png
[Author Date]: http://dl.dropbox.com/u/1131693/bloodrop/3577205.pdf 


##### 9-7

![][9-7]  
More detail in ([Maisin 1988][Maisin 1988])

[9-7]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.30.19.png
[Maisin 1988]: Reference 


##### 1003-20

![][1003-20]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-20]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-22%2016.36.24.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-21

![][1003-21]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-21]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.20.41.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf 


##### 1003-22 (only controls)

![][1003-22]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-22]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.21.26.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-24 (only controls)

![][1003-24]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-24]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.22.00.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf 


##### 1003-25 (only controls)

![][1003-25]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-25]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.22.34.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf 


##### 1003-26

![][1003-26]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-26]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.23.02.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf 


##### 1003-27

![][1003-27]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-27]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.26.08.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-28 (only controls)

![][1003-28]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-28]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.23.34.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-29

![][1003-29]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-29]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.23.59.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-30 (only controls)

![][1003-30]  
More detail in ([Grahn 1995][Grahn 1995])

[1003-30]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.24.25.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf



##### 1007-2

![][1007-2]  
More detail in ([Storer 1988][Storer 1988])

[1007-2]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.31.08.png
[Storer 1988]: http://dl.dropbox.com/u/1131693/bloodrop/3577229.pdf 


##### 1007-3

![][1007-3]  
More detail in ([Ullrich 1979][Ullrich 1979])

[1007-3]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.31.26.png
[Ullrich 1979]: http://dl.dropbox.com/u/1131693/bloodrop/3575012.pdf 




##### References
<a name="gerber_1996"></a>
Gerber, Watson, Sugahara, Okada. International Radiobiology Archives of Long-Term Animal Studies I. Descriptions of Participating Institutions and Studies. 1996. [link](http://www.ustur.wsu.edu/nra/pdf/ira.pdf)



____________________________________________________________________
^ back to [table of contents](#contents)






<a name="visual_concordance"></a>

Visualize
========================================================
*Last Update: April 2014*

Now that the data is reasonably clean, show what it looks
like.


```r
# Libraries
library(plyr)
library(ggplot2)
library(survival)
library(dplyr)
library(directlabels)

# Data
setwd("~/janus/scripts")
d <- readRDS("data/ddref.rds")
```





```r
ggplot(d, aes(lifespan, color = dose, group = factor(paste(dose, dose_rate, 
    fractions)), y = ..scaled..)) + geom_density(adjust = 2) + facet_wrap(~cluster, 
    scales = "free") + scale_colour_gradient(guide = guide_legend(title = "Dose (gy)"), 
    trans = "sqrt") + geom_vline(aes(xintercept = intended_assignment_age), 
    alpha = 0.5) + expand_limits(x = -4)
```

![plot of chunk unnamed-chunk-11](Figs/unnamed-chunk-11.png) 

#### Figure: Lifespan by dose and cluster
Density plots show the frequency, y-axis, of a given lifespan (in days), x-axis.  Sepperate density curves are shown for each distinct dose and dose rate.  The total dose delivered is labeled by color as shown in the figure legend.  The mean age at first exposure is denoted by a gray vertical line on each graph.

Clusters are sepperated by facets and labeled with sex, strain, species, lab, quality of radiation, and mean age at first exposure (in days).  The clusters are ordered by the number of animals in the cluster.  The cluster with the most animals, Male RFM/Un Mice from ORNL, are at the top left and the cluster with the least animals is furthest to the right on the bottom row.


```r
# TODO(later): graph that shows only directly comparable protracted v not
# TODO(later): make survival curves that show the difference in % survival
# between a group and the cluster average TODO(later): figure out what is
# the cause of early effects in ORNL data based on the pathology codes
# released with that dataset.  TODO: Add figure legends

```


A few observations:
- **Different clusters show very different responses to the same total dose.**  For example, compare Female RFM/UN Mice from ORNL (1) to Female C57Bl/6Bd mice from the same institution (10).  The only difference between these experiments is the strain of mice used.  The former show a strong response to gamma rays, the latter a weak response or no response.  These differences could reflect strain-specific differences in radiosensitivity or methodological errors in the way that the studies were conducted.
- **The cluster used to estimate DDREF shows a particularly strong and early response**.  RFM/Un mice from ORNL were used as the acute condition in the estimate of DDREF from BEIR VII (chronic exposure is not shown here because individual level data is not available).  Notably, this is one of the stronger radiation responses seen in any cluster, including other similar ones from ORNL.  Moreover, the response is bimodal, with an unusual early effect.  This suggests that the acute effects are being over-estmimate in BEIR VII and that DDREF should be closer to one, that is the difference between acute and protracted exposure is smaller than the BEIR VII estimate.


```r
comparable_doses <- d %.% ungroup() %.% group_by(cluster_id, dose) %.% summarize(n_groups = n_unique(fractions, 
    dose_rate)) %.% filter(n_groups > 1)
```

```
## Error: could not find function "n_unique"
```

```r

g <- d %.% ungroup() %.% filter(cluster_id %in% comparable_doses$cluster_id, 
    dose %in% c(comparable_doses$dose, 0))
```

```
## Error: object 'comparable_doses' not found
```

```r

ggplot(g, aes(lifespan, color = dose, group = factor(paste(dose, dose_rate, 
    fractions)), linetype = protracted, y = ..scaled..)) + geom_density(adjust = 2) + 
    facet_grid(dose ~ cluster, scales = "free") + scale_colour_gradient(guide = guide_legend(title = "Dose (gy)")) + 
    geom_vline(aes(xintercept = intended_assignment_age), alpha = 0.5) + expand_limits(x = -4)
```

```
## Error: object 'g' not found
```

```r

# TODO: Why doesn't ANL have comparable groups that recieved the same dose
# at different fractions.  I am guessing they do, but the way that we are
# defining 'intended age at first assignment' is removing these results
```



#### Label treatment groups

Show the same graphs with individual treatement groups labeled, this is not a figure for papers or presentations, but is handy for those who want to inspect the data very closely (me).


```r

# TODO(later) remove this graph from the presented materials

# g <- ggplot(d %.% filter(cluster_id == 15), aes(lifespan, y =
# ..scaled.., color=group_id, group=group_id )) + geom_density(adjust=2) +
# facet_wrap(~ cluster, scales='free') + geom_vline(
# aes(xintercept=intended_assignment_age, color=group_id) ) + geom_vline(
# aes(xintercept=age_at_treatment, color=group_id) ) + geom_vline(
# aes(xintercept=age_at_last_treatment, color=group_id) ) +
# expand_limits(x = -4, y = 1.3)
# 
# direct.label(g, list('top.bumptwice', cex=0.6))

```


#### Survival plots
Survival vs time by cluster with indications for dose and protraction.


```r
# TODO this graph should include age_at_last treatment too How many
# animals were alive after X days in each treatment group?
d <- d %.% group_by(cluster, dose, dose_rate, fractions) %.% arrange(lifespan) %.% 
    mutate(survival = rank(-lifespan)/length(lifespan))

ggplot(d, aes(lifespan, survival, color = dose, group = factor(paste(dose, dose_rate, 
    fractions)))) + geom_path() + facet_wrap(~cluster, scales = "free") + scale_color_continuous(guide = guide_legend(title = "Dose (gy)"), 
    trans = "sqrt") + geom_vline(aes(xintercept = intended_assignment_age), 
    alpha = 0.5) + expand_limits(x = -4)
```

![plot of chunk unnamed-chunk-15](Figs/unnamed-chunk-15.png) 




```r
# TODO this graph should include age_at_last treatment too How many
# animals were alive after X days in each treatment group?
d <- d %.% ungroup() %.% group_by(cluster, dose, dose_rate, fractions) %.% arrange(lifespan) %.% 
    mutate(group_survival = rank(-lifespan)/length(lifespan))

# How many animals were alive after X days in each cluster?
d <- d %.% ungroup() %.% group_by(cluster) %.% arrange(lifespan) %.% mutate(cluster_survival = rank(-lifespan)/length(lifespan))

ggplot(d, aes(lifespan, group_survival - cluster_survival, color = dose, group = factor(paste(dose, 
    dose_rate, fractions)))) + geom_path() + facet_wrap(~cluster, scales = "free_x") + 
    scale_color_continuous(guide = guide_legend(title = "Dose (gy)"), trans = "sqrt") + 
    geom_vline(aes(xintercept = intended_assignment_age), alpha = 0.5)
```

![plot of chunk unnamed-chunk-16](Figs/unnamed-chunk-16.png) 


__________________________________________________________________
^ back to [table of contents](#contents)




<a name="lss"></a>

Atomic bomb survivor data
========================================================
*Last update: April 2014*
For comparison, let's load data from the atomic bomb survivors, [lss14][lss], and see how lifespan changes as a function of dose in these populations using similar visualizations.

Acknowledgement:
This report makes use of data obtained from the Radiation Effects Research Foundation (RERF), Hiroshima and Nagasaki, Japan. RERF is a private, non-profit foundation funded by the Japanese Ministry of Health, Labour and Welfare and the U.S. Department of Energy, the latter through the National Academy of Sciences.The conclusions in this report are those of the authors and do not necessarily reflect the scientific judgment of RERF or its funding agencies.
Please send a copy of any reprints that make use of these data to:
Archives Unit, Library and Archives Section
Department of Information Technology
Radiation Effects Research Foundation
5-2 Hijiyama Park
Minami-ku Hiroshima, 732-0815 JAPAN

[lss]: http://rerf.jp/library/dl_e/lss14_document.pdf


```r

# Libraries
library(dplyr)
library(plyr)

# Helper functions
get_max_map <- function(min_map, max = Inf) {
    max_map <- c(min_map[2:length(min_map)], max)
    names(max_map) <- names(min_map)
    max_map
}
get_mean_map <- function(min_map, max = Inf) {
    mean_map <- (min_map + get_max_map(min_map, max))/2
    mean_map
}

# Load data
setwd("~/janus/")
data <- read.csv("data/lss14/lss14.csv")

# Define Terms
sex_map <- c(`1` = "♂", `2` = "♀")
agecat_min_map <- c(`1` = 0, `2` = 5, `3` = 10, `4` = 15, `5` = 20, `6` = 25, 
    `7` = 30, `8` = 35, `9` = 40, `10` = 45, `11` = 50, `12` = 55, `13` = 60, 
    `14` = 65, `15` = 70, `16` = 75, `17` = 80, `18` = 85, `19` = 90, `20` = 95, 
    `21` = 100)

agecat_mean_map <- get_mean_map(agecat_min_map, 120)
dose_min_map <- c(`1` = 0, `2` = 0.005, `3` = 0.02, `4` = 0.04, `5` = 0.06, 
    `6` = 0.08, `7` = 0.1, `8` = 0.125, `9` = 0.15, `10` = 0.175, `11` = 0.2, 
    `12` = 0.25, `13` = 0.3, `14` = 0.5, `15` = 0.75, `16` = 1, `17` = 1.25, 
    `18` = 1.5, `19` = 1.75, `20` = 2, `21` = 2.5, `22` = 3)

dose_mean_map = get_mean_map(dose_min_map, 4)

threshold = 1.5001

# Fix data
data <- data %.% # Thin Only select interesting columns
select(death, city, sex, agexcat, agecat, dosecat, agex, age) %.% # Translate Values into different units
mutate(sex = sex_map[sex], agex = agecat_mean_map[agexcat], age = agecat_mean_map[agecat], 
    dose = dose_mean_map[dosecat]) %.% # Shorten Remove those with a dose above threshold
filter(dose < threshold)

# Define values

# One row per death
long <- ldply(unique(data$death), function(n) {
    d <- data[data$death == n, ]
    d[rep(1:nrow(d), n), ]
})

# Prove it was actually done
sum(data$death) == 49879
```

```
## [1] TRUE
```

```r
nrow(long) == 49879
```

```
## [1] TRUE
```

```r

# Update data
long <- long %.% select(-death)

# Reduce resolution (as it is there are too many categories for graphing)
g <- long %.% mutate(agex = round(agex/20) * 20, age_string = paste0(agex, "+ years"), 
    lifespan = age)

# Show it off
ggplot(g, aes(x = lifespan, color = dose, group = factor(dose), y = ..scaled..)) + 
    geom_density(adjust = 2) + scale_colour_gradient(guide = guide_legend(title = "Dose (gy)"), 
    trans = "sqrt", breaks = c(0, 0.5, 1, 1.5), limits = c(0, 1.5)) + geom_vline(aes(xintercept = agex), 
    alpha = 0.5) + facet_wrap(~age_string + sex)
```

![plot of chunk unnamed-chunk-17](Figs/unnamed-chunk-17.png) 

```r

# TODO: Currently this ignores persons who are still alive (mostly in the
# 0-40 year range at the time of exposure), I should fix the data set so
# that these people are represented too.  TODO: It would be nice to have a
# survival curve version of this graph.  TODO: reduce the number of dose
# categories, probably < 5 per facet TODO: Tanja wants to see these
# results seperated by city
```

__________________________________________________________________
^ back to [table of contents](#contents)






<a name="10B3"></a>

Reproduce BEIR 10B3
========================================================
*Last update: June 2013*

Reproduce the BEIR estimates on the oak ridge lifespan data
from storer 1979 (3575012.pdf).


```r
# Libraries
library(ggplot2)
library(plyr)

# Data
setwd('~/janus/scripts')
source('util.R')
data <- read.csv('data/storer_1979.csv', sep='\t')  
    

model_10B3 <- function(data){
    glm(
        I(1/age) ~ dose + I(dose^2 / (fractions)),
        data=data,
        weights=n
    )
}   
show <- function(g){
    original <- g[is.na(g$p_10B3),] 
    predictions <- g[!is.na(g$p_10B3),]
    suppressWarnings(print(
    ggplot(original, aes(
        dose, 
        1/age, 
        label=type,
        group=type
    )) +
        geom_text(size=5) + 
        geom_smooth(
            data=predictions,
            aes(dose, p_10B3),
            color='black'
        ) # + 
        # geom_smooth(
            # data=predictions,
            # aes(dose, p_my_analysis), 
            # color='red'
        # ) 
    ))
}

    
# Constants
threshold = 1.5001

# Prediction Intervals
to_predict <- expand.grid(
    fractions = c(1, 1000),
    dose = seq(0, 1.5, 0.1)
)
to_predict$type <- 'A'
to_predict$type[to_predict$fractions > 1] <- 'C'

# Clean
data$fractions <- 1
data$fractions[data$rate < 0.1] <- Inf

# Define Acute
chronic <- data$rate < 0.1
data$type <- 'A'
data$type[chronic] <- 'C'

# Subset
data$modeled_in_10B3 <- with(data, 
    dose < threshold &
    strain == 'RFM'  &
    sex == 'F' &
    rate != 0.4
)
data$in_my_analysis <- data$type == 'A'

# prediction matrix
predictions <- to_predict
addin <- names(data)[!names(data) %in% names(predictions)]
predictions <- merge(data[1, addin], predictions, all=TRUE)

# 10B3 Model
m <- model_10B3(data[data$modeled_in_10B3,])
predictions$p_10B3 <- predict(m, newdata=predictions)

# My Model
s <- data$modeled_in_10B3 & 
     data$in_my_analysis
m <- model_10B3(data[s,])
predictions$p_my_analysis <- predict(m, newdata=predictions)

# Merge
data$p_10B3 <- NA
data$p_my_analysis <- NA

data <- rbind(predictions, data)
```

#### Show
##### 10B3
This is the original [10B3 figure][10B3-citation] from the beir VII report [beir-10b3].

![10B3-image]

[10B3-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=257
[10B3-image]: http://dl.dropbox.com/u/1131693/bloodrop/10B3.jpg


##### Reproduce 10B3
This is my reproduction of 10B3 to prove that I am faithfully applying their methodology.

```r
g <- data[with(data, strain == "RFM" & sex == "F" & rate != 0.4), ]
show(g)
```

![plot of chunk unnamed-chunk-19](Figs/unnamed-chunk-19.png) 

```r
ggsave_for_ppt("beir_10B3_reproduction.png")
```


### Results
I am capable of reproducing their results.  There are two tricks

  1. They stratified by strain and gender
  2. They weighted by n

Suspiciously they do not include data from table 2, but when I add this in, it does not make a huge difference, so I assume they are just not being careful.

_____________________________________________________________________________  
^ back to [table of contents](#contents)








<a name="loglike"></a>

Log likelihood
========================================================

I need to know how log likelihood is calculated.  Basically
it should be proportional to 

  sum((actual - predicted)^2 / variance)

And variance is measured as

  variance = sum((actual - predicted)^2)) / (n - p - 1)

If variance is estimated at zero, this goes to infinity, so
the overfit regression model has problems.


```r

data <- data.frame(y = 1:100, x = rnorm(100, 1:100))
n <- nrow(data)
m <- glm(y ~ x, data = data)
data$p <- predict(m)
data$e <- data$y - data$p
o2 = with(data, sum(e^2)/(n))
o2
```

```
## [1] 1.08
```

```r
l <- with(data, -(n/2) * log(2 * pi) + -(n/2) * log(o2) + -(1/(2 * o2)) * sum(e^2))
l - as.numeric(logLik(m))
```

```
## [1] 0
```


_____________________________________________________________________________  
^ back to [table of contents](#contents)







<a name="10B4"></a>

Reproduce BEIR 10B4
========================================================
*Last update: June 2013*
Reproduce the BEIR estimates on the oak ridge lifespan data
from storer 1979 (3575012.pdf).


```r

# Libraries
library(ggplot2)
library(plyr)

# Data
setwd('~/janus/scripts')
data <- read.csv('data/storer_1979.csv', sep='\t')  

model_10B4 <- function(data, o){    
    glm(
        I(1/age) ~ I(dose + o*dose^2 / (fractions)),
        data=data,
        weights=n
    )
}       
normalize_likelihood <- function(l, delta){
    l <- l - max(l)
    l <- exp(l)
    l <- l / sum(l)
    l <- l / delta
    
    l       
}
        
# Constants
threshold = 1.5001

# Clean
data$fractions <- 1
data$fractions[data$rate < 0.1] <- Inf

# Define Acute
chronic <- data$rate < 0.1
data$type <- 'A'
data$type[chronic] <- 'C'

# Subset
data$modeled_in_10B4 <- with(data, 
    dose < threshold &
    strain == 'RFM'  &
    sex == 'F' # &
    # rate != 0.4
)
data$in_my_analysis <- data$type == 'A'

# Model
low = -2
high = 6
delta = .01
o_range = (low/delta):(high/delta) * delta

get_likelihoods <- function(o_range, data){
    r <- ldply(o_range, function(o){
        m <- model_10B4(data, o)
        l = logLik(m)
        
        data.frame(o, l)
    })
    
    r$l <- normalize_likelihood(r$l, delta)
    
    r
}

beir_r <- get_likelihoods(o_range, data[data$modeled_in_10B4,])
my_r <- get_likelihoods(o_range, data[data$in_my_analysis,])
```


#### Show
##### 10B4
This is the original [10B4 figure][10B4-citation] from the beir VII report [beir-10b4].

![10B4-image]

[10B4-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=258
[10B4-image]: http://dl.dropbox.com/u/1131693/bloodrop/10B4.jpg


##### Reproduce 10B4
This is my reproduction of 10B4 to prove that I am faithfully applying their methodology.

```r

ggplot(beir_r, aes(o, l)) + geom_path() + # geom_path(data=my_r, color='red') +
scale_y_continuous(breaks = c(0:5)/5, limits = c(0, 1))
```

![plot of chunk unnamed-chunk-22](Figs/unnamed-chunk-22.png) 

```r
ggsave_for_ppt("beir_10B4_reproduction.png")
```


#### Results

I was able to reproduce figure 10B4 near perfectly.  Importantly it becomes nearly straight if we only include the acute exposures.

It could be that the shoulder is very narrow, though this would be surprising given what we know about tissue level effects.

It could also be that there is some systematic bias between chronic and high dose experiments.  For instance the high dose rate was even higher than the experimenters thought that it was.

More reason for meta-analysis.

_____________________________________________________________________________  
^ back to [table of contents](#contents)





<a name="10B3-all-data"></a>

1/lifespan on all data
========================================================
*Last update: June 2013*
  
Show graphs like Storer 1979 10-B3 but for all data.


```r

# Libraries
setwd("~/janus/scripts")
source("util.R")
library(ggplot2)
library(plyr)

# Data
data <- readRDS("data/ddref.rds")

# Helpers
model_10B3 <- function(data) {
    glm(I(1/age) ~ dose + I(dose^2/(fractions)), data = data, weights = n)
}
show <- function(g) {
    f <- function(x) round(x, 2)
    suppressWarnings(print(ggplot(g[is.na(g$p), ], aes(dose, 1/age, label = type, 
        group = type)) + geom_text(size = 4) + geom_smooth(data = g[!is.na(g$p), 
        ], aes(dose, p, color = type), method = "lm", formula = "y ~ x + I(x^2)", 
        se = FALSE) + facet_wrap(~cluster, scales = "free_y")))
}

# Prediction Intervals
to_predict <- expand.grid(fractions = c(1, 1000), dose = seq(0, 1.5, 0.1))
to_predict$type <- "A"
to_predict$type[to_predict$fractions > 1] <- "C"


# Mean Lifespans
aggregate <- ddply(data, .(cluster, group_id, sex), function(df) {
    u <- function(x) paste(unique(x), collapse = " ")
    dont_aggregate <- c("lifespan", "id", "X", "n")
    data.frame(llply(df[, !names(df) %in% dont_aggregate], u), age = mean(df$lifespan), 
        age_sd = sd(df$lifespan), n = nrow(df))
})

# Restore Sanity
numerics <- names(data)[laply(data, is.numeric)]
for (n in numerics) {
    if (n %in% names(aggregate)) {
        aggregate[, n] <- as.numeric(as.character(aggregate[, n]))
    }
}

# Model
m <- c("cluster", "sex", "cluster")
df <- aggregate[aggregate$cluster == aggregate$cluster[1] & aggregate$sex == 
    aggregate$sex[1] & aggregate$cluster == aggregate$cluster[1], ]
aggregate <- ddply(aggregate, .(cluster, sex, cluster), function(df) {
    # Extract Coefficients
    m <- model_10B3(df)
    c <- m$coefficients
    df$a <- c["dose"]
    df$B <- c["I(dose^2/(fractions))"]
    
    # Extract Predictions
    predictions <- to_predict
    addin <- names(df)[!names(df) %in% names(predictions)]
    predictions <- merge(df[1, addin], predictions, all = TRUE)
    predictions$p <- predict(m, newdata = predictions)
    
    # merge
    df$p <- NA
    out <- rbind(df, predictions)
    
    out
})

# TODO(ben) This methodology should look similar to the one we used for
# the meta analysis TODO(ben) change the colors in order to make the theme
# consistent throughout the presentation.  I recommend black for chronic
# effects and red for accute.  Use transparency to represent my new
# results when I over-lay them so that the old results can still be seen
# (or visa-versa).


# Show As in 10B3 http://www.nap.edu/openbook.php?record_id=11340&page=257
a <- aggregate
g <- a
show(g)
```

![plot of chunk unnamed-chunk-23](Figs/unnamed-chunk-23.png) 

```r
ggsave_for_ppt("inverse_lifespan.png")
```


#### Results

This data is far from well behaved!  

Chronic effects may appear better or worse than projected acute effects.  Sometimes hormesis like respsonses appear.  Its not obvious from this graph because the y-axis is stretched to fit the data, but the magnitude of the effect is changing wildly too.

It is no wonder that radiobiology is full of debate!  

At this point we should be a bit skeptical of organizing the data in this, the BEIR VII manner.  While that approach seemed reasonable given the ORNL data that they worked with, it clearly does not generalize well.  This may be because the underlying statitical approach is flawed, or simply that these graphs a very robust way of displaying the effect.  In any case we question the 'intuitive appeal' of graph 10B3.  While it seemed quite difinitive in isolation, the effect is lost when we try to repeat it on new datasets.

_____________________________________________________________________________  
^ back to [table of contents](#contents)






<a name="10B4-all-data"></a>

1/lifespan profiles on all data
========================================================
*Last update: June 2013*

Show graphs like Storer 1979 10B4 but for all data.


```r

# Libraries
library(ggplot2)
library(plyr)

# Data
setwd("~/janus/scripts")
data <- readRDS("data/ddref.rds")


# Helpers
model_10B4 <- function(data, o) {
    glm(I(1/age) ~ I(dose + o * dose^2/(fractions)), data = data, weights = n)
}
normalize_likelihood <- function(l, delta) {
    l <- l - max(l)
    l <- exp(l)
    l <- l/sum(l)
    l <- l/delta
    
    l
}


# Mean Lifespans
aggregate <- ddply(data, .(cluster, group_id, sex), function(df) {
    u <- function(x) paste(unique(x), collapse = " ")
    dont_aggregate <- c("lifespan", "id", "X", "n")
    data.frame(llply(df[, !names(df) %in% dont_aggregate], u), age = mean(df$lifespan), 
        age_sd = sd(df$lifespan), n = nrow(df))
})

# Restore Sanity
numerics <- names(data)[laply(data, is.numeric)]
for (n in numerics) {
    if (n %in% names(aggregate)) {
        aggregate[, n] <- as.numeric(as.character(aggregate[, n]))
    }
}


# Model
low = -2
high = 6
delta = 0.01  # decrease for higher resolution
o_range = (low/delta):(high/delta) * delta


# Model
aggregate <- ldply(o_range, function(o) {
    ddply(aggregate, .(cluster, sex), function(df) {
        
        m <- model_10B4(df, o)
        df$l <- logLik(m)
        df$o <- o
        df
    })
})

# Summarize Effect
summary <- ddply(aggregate, .(o), function(df) {
    sum(df$l, na.rm = TRUE)
})
names(summary) <- c("o", "l")
summary$l <- normalize_likelihood(summary$l, delta)


# Normalize
aggregate <- ddply(aggregate, .(cluster, sex), function(df) {
    df$l <- normalize_likelihood(df$l, delta)
    df
})

# Show As in 10B4 http://www.nap.edu/openbook.php?record_id=11340&page=257
a <- aggregate

show <- function(g) {
    g$cluster <- as.factor(as.character(g$cluster))
    g$l <- pmin(g$l, 1)
    suppressWarnings(print(ggplot(g, aes(o, l)) + geom_path() + ylim(0, 1) + 
        facet_wrap(~cluster)))
}
g <- a
show(g)
```

![plot of chunk unnamed-chunk-24](Figs/unnamed-chunk-24.png) 

```r
ggsave_for_ppt("inverse_lifespan_profile.png")
```

    
#### Results
Looks bad, we are way too confident!

_____________________________________________________________________________  
^ back to [table of contents](#contents)







<a name="metaregression"></a>

Meta Regression Figure
========================================================
*Last update: June 2013*

A figure that shows off the principal of meta-regression.


```r
# Libraries
library(ggplot2)
library(plyr)
library(metafor)

# Helpers
ggsave_for_ppt <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
    height = 7.68, units = "in"))


# Fake data
set.seed(1)
sd <- 1
n <- 5
x <- 1:n
v <- rnorm(n)^2
data <- data.frame(yi = rnorm(n, 1:n, sd), vi = v, x = x)

# An outlider
data$yi[3] <- 0
data$vi[3] <- 0.01

# Model
m <- rma(yi, vi, mods = cbind(x), data = data, method = "ML")
data$p <- predict(m)$pred
data$tau2 <- m$tau2

# Predict likelihood
data$o2 <- (data$vi + data$tau2)
data$e <- (data$p - data$yi) * 0.3
n <- nrow(data)
l <- with(data, sum(-(1/2) * sum(e^2/o2) + -(1/2) * log(o2) + -(1/2) * log(2 * 
    pi)))
logLik(m)
```

```
## 'log Lik.' -8.707 (df=3)
```

```r
l
```

```
## [1] -7.493
```

```r


df <- 3
Q <- with(data, sum(yi^2/vi))
tau2 = (Q - df)/C
```

```
## Error: non-numeric argument to binary operator
```

```r


# Show
ggplot(data, aes(x, yi)) + geom_point() + geom_errorbar(aes(ymin = yi - vi^0.5, 
    ymax = yi + vi^0.5, ), width = 0.1, alpha = 0.5) + geom_errorbar(aes(ymin = yi - 
    (vi + tau2)^0.5, ymax = yi + (vi + tau2)^0.5, ), width = 0.1, alpha = 0.5, 
    color = "red") + geom_path(aes(x, p), color = "black")
```

![plot of chunk unnamed-chunk-25](Figs/unnamed-chunk-25.png) 

```r
ggsave_for_ppt("meta_regression_example.png")
```


_____________________________________________________________________________  
^ back to [table of contents](#contents)









<a name="10B3-meta"></a>

Meta Regression of BEIR 10B3
========================================================
*Last update: June 2013*

BEIR fits oak ridge data as if they are points, but actually each point represents many samples and we know the standard deviation of this estimate.  Therefore meta-regression is a more appropriate form of analysis.  Here we show the fit by meta-regression.

We will also run a heterogeneity test.


```r

# Libraries
library(ggplot2)
library(plyr)
library(metafor)

# Data
setwd("~/janus/scripts")
data <- read.csv("data/storer_1979.csv", sep = "\t")

# Helpers
ggsave_for_ppt <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
    height = 7.68, units = "in"))

model_10B3 <- function(data) {
    glm(I(1/age) ~ dose + I(dose^2/(fractions)), data = data, weights = n)
}
model_meta <- function(data) {
    data$a <- data$dose
    data$B <- with(data, dose^2/(fractions))
    rma(yi, vi, mods = cbind(a, B), data = data, method = "ML")
}
predict_meta <- function(m, newdata) {
    newdata$a <- newdata$dose
    newdata$B <- with(newdata, dose^2/(fractions))
    predict(m, newmods = with(newdata, cbind(a, B)))$pred
}
show <- function(g) {
    original <- g[is.na(g$p_10B3), ]
    predictions <- g[!is.na(g$p_10B3), ]
    
    suppressWarnings(print(ggplot(original, aes(dose, 1/age, label = type, group = type)) + 
        geom_errorbar(aes(ymin = 1/age + (vi + tau2)^0.5, ymax = 1/age - (vi + 
            tau2)^0.5), alpha = 0.5, width = 0.05, color = "red") + geom_errorbar(aes(ymin = 1/age + 
        vi^0.5, ymax = 1/age - vi^0.5), alpha = 0.5, width = 0.1) + geom_text(size = 4) + 
        geom_line(data = predictions, aes(dose, p_10B3)) + geom_line(data = predictions, 
        aes(dose, p_my_analysis), color = "red") + annotate("text", x = 1, y = 0.0024, 
        label = paste("p heterogeneity < ", format(p_heterogeneity, scientific = TRUE, 
            digits = 1)))))
}

# Constants
threshold = 1.5001

# Prediction Intervals
to_predict <- expand.grid(fractions = c(1, 1000), dose = seq(0, 1.5, 0.1))
to_predict$type <- "A"
to_predict$type[to_predict$fractions > 1] <- "C"

# Clean
data$fractions <- 1
data$fractions[data$rate < 0.1] <- Inf

# Define Acute
chronic <- data$rate < 0.1
data$type <- "A"
data$type[chronic] <- "C"

# Subset
data$modeled_in_10B3 <- with(data, dose < threshold & strain == "RFM" & sex == 
    "F" & rate != 0.4)
data$in_my_analysis <- data$type == "A"

# Prepare for Meta
data$yi <- with(data, 1/age)
data$vi <- with(data, (1/age - 1/(age + sd))^2)

# Predictions
predictions <- to_predict
addin <- names(data)[!names(data) %in% names(predictions)]
predictions <- merge(data[1, addin], predictions, all = TRUE)

# 10B3 Model
s <- data$modeled_in_10B3
m <- model_10B3(data[s, ])
predictions$p_10B3 <- predict(m, newdata = predictions)

# Meta Model
s <- data$modeled_in_10B3
m <- model_meta(data[s, ])
predictions$p_my_analysis <- predict_meta(m, newdata = predictions)
data$tau2 <- m$tau2
p_heterogeneity <- m$QEp

# Merge
data$p_my_analysis <- NA
data$p_10B3 <- NA
predictions$tau2 <- NA
data <- rbind(data, predictions)

# Show Reproduce 10B3
# http://www.nap.edu/openbook.php?record_id=11340&page=257
g <- data[with(data, strain == "RFM" & sex == "F" & rate != 0.4), ]
show(g)
```

![plot of chunk unnamed-chunk-26](Figs/unnamed-chunk-26.png) 

```r
ggsave_for_ppt("beir_10B3_meta_regression.png")
```


#### Results

The a/B ratio goes down a bit.  Standard error bars are much larger than they were originally.

Heterogeneity is highly significant as measured by restricted maximum likelihood.  More on that measurement from the metafor paper (http://www.jstatsoft.org/v36/i03/paper) and they cite

Q-test  
Hedges LV, Olkin I (1985). Statistical Methods for Meta-Analysis. Academic Press, San Diego, CA.

REML  
Viechtbauer W (2005). Bias and Eciency of Meta-Analytic Variance 

Estimators in the Random-Eects Model." Journal of Educational  and Behavioral Statistics, 30(3), 261-293.

_____________________________________________________________________________  
^ back to [table of contents](#contents)








<a name="10B4-meta"></a>

Meta Regression of BEIR 10B4
========================================================
*Last update: June 2013*

BEIR VII estimates are based on ordinary linary regresssion
of mean lifespans per group ignoring the fact that these means
have a standard error.

This affects the likelihood estimate as an exact fit of the
data is estimated as much more likely than a very near fit of
the data.  This becomes painfully obvious when we run the
profile analysis on data containing only 3 groups in which case
one particular curvature fits the data exactly and produces an
estimate considered to be infinitely likely.

Here I will add standard error into the BEIR analysis both in
the graphs and in the likelihood analysis.  As before the data
will come from storer 1979 (3575012.pdf).


```r

# Libraries
library(ggplot2)
library(plyr)
library(metafor)

# Data
setwd('~/janus/scripts')
data <- read.csv('data/storer_1979.csv', sep='\t')  

# Helpers
ggsave_for_ppt <- function(...) suppressWarnings(ggsave(..., 
                                dpi=100, 
                                width=10.24, 
                                height=7.68, 
                                units='in'))

model_10B4 <- function(data, o){    
    glm(
        I(1/age) ~ I(dose + o*dose^2 / (fractions)),
        data=data,
        weights=n
    )
}       
model_meta <- function(data, o){    
    data$curved_dose <- with(data, 
        dose + o*dose^2 / (fractions)
    )
    rma(
        1/age, 
        (1/age - 1/(age + sd))^2, 
        mods = cbind(curved_dose), 
        data = data,
        method='ML'
    )
}

normalize_likelihood <- function(l, delta){
    l <- l - max(l)
    l <- exp(l)
    l <- l / sum(l)
    l <- l / delta
    
    l       
}
        
# Constants
threshold = 1.5001

# Clean
data$fractions <- 1
data$fractions[data$rate < 0.1] <- Inf

# Define Acute
chronic <- data$rate < 0.1
data$type <- 'A'
data$type[chronic] <- 'C'

# Subset
data$modeled_in_10B4 <- with(data, 
    dose < threshold &
    strain == 'RFM'  &
    sex == 'F' # &
    # rate != 0.4
)
data$in_my_analysis <- data$type == 'A'

# Model
low = -2
high = 6
delta = .01   # Reduce to increase resolution
o_range = (low/delta):(high/delta) * delta

get_likelihoods <- function(
    o_range, 
    modeling_function=model_10B4,
    d=data[data$modeled_in_10B4,]
){
    r <- ldply(o_range, function(o){
        m <- modeling_function(d, o)
        l = logLik(m)
        
        data.frame(o, l)
    })
    
    r$l <- normalize_likelihood(r$l, delta)
    
    r
}


beir_r <- get_likelihoods(o_range)
my_r <- get_likelihoods(o_range, model_meta)


# Reproduce 10B4
# http://www.nap.edu/openbook.php?record_id=11340&page=258
ggplot(beir_r, aes(o, l)) + 
    geom_path() + 
    geom_path(data=my_r, color='red') + 
    scale_y_continuous(breaks = c(0:5)/5, limits=c(0,1))
```

![plot of chunk unnamed-chunk-27](Figs/unnamed-chunk-27.png) 

```r
ggsave_for_ppt('beir_10B4_meta_reression.png')    
```


#### Results

I was able to reproduce figure 10B4 near perfectly.  Importantly
it becomes nearly straight if we only include the acute
exposures.

It could be that the shoulder is very narrow, though this 
would be surprising given what we know about tissue level 
effects.

It could also be that there is some systematic bias between
chronic and high dose experiments.  For instance the high
dose rate was even higher than the experimenters thought that
it was.

More reason for meta-analysis.

_____________________________________________________________________________  
^ back to [table of contents](#contents)









<a name="10B3-meta-all"></a>

Meta-regression on all data
========================================================
*Last update: June 2013*

Show graphs like Storer 1979 10B3 but for all data using random effects meta-regression.


```r

# Libraries
library(ggplot2)
library(plyr)
library(metafor)

# Data
setwd("~/janus/scripts")
source("util.R")
data <- readRDS("data/ddref.rds")
```

#### Modeling functions
Specify modeling functions.  

`model_10B3` will fit a linear quadratic model exactly as in the BEIR VII report, without accounting for within or between group error.


```r
model_10B3 <- function(data) {
    glm(I(1/age) ~ dose * cluster + I(dose^2/fractions) * cluster, data = data, 
        weights = n)
}
```


`model_meta` will fit an identical model except that within group and between group error will be accounted for.


```r
model_meta <- function(data){   
  data$a <- data$dose
  data$B <- with(data, dose^2 / (fractions))
  
  rma(
    yi, 
    vi, 
    mods = ~ a*cluster + B*cluster -a -B -1, 
    data = data,
    method="ML"
  )
}

        
# Mean Lifespans
aggregate <- ddply(data, .(cluster, group_id, sex), function(df){
    u <- function(x) paste(unique(x), collapse=' ')
    dont_aggregate <- c('lifespan', 'id', 'X', 'n')
    n <- nrow(df)
    data.frame(
        llply(df[,!names(df) %in% dont_aggregate], u),
        age=mean(df$lifespan),
        sd=sd(df$lifespan)/n^0.5,
        n=nrow(df)
    )   
})

# Prepare for Meta
aggregate$yi <- with(aggregate, 1/age)
aggregate$vi <- with(aggregate, (1/age - 1/(age + sd))^2)   

# Restore Sanity
numerics <- names(data)[laply(data, is.numeric)]
for(n in numerics) {
    if(n %in% names(aggregate)){
        aggregate[,n] <- as.numeric(as.character(aggregate[,n]))
    }
}

# Model
m <- model_meta(aggregate)
```

```
## Error: Model matrix not of full rank. Cannot fit model.
```

```r
c <- coefficients(m)
aggregate <- aggregate %.%
  mutate(i=c[paste0('cluster', cluster)],
         a=c[paste0('a:cluster', cluster)],
         B=c[paste0('cluster', cluster, ':B')],
         tau2=m$tau2)
aggregate$p_my_analysis <- predict(m)$pred
```

```
## Error: replacement has 9 rows, data has 95
```

```r
aggregate$p_10B3 <- predict(model_10B3(aggregate))

# Project
# Add projections across the entire range (0-1.5 Gy) for
# acute and protracted exposures.  This will create nicer
# graphs.
doses = seq(from=0, to=1.5, by=.1)
coefficients = unique(aggregate[,c('i', 'a', 'B', 'cluster')])
projections = ddply(coefficients, .(cluster), function(df){
  acute <- with(df, cbind(data.frame(
    dose = doses,
    type = 'A',
    fractions = 1,
    p_my_analysis = i + a * doses + B * doses^2
  ), df))
  
  chronic <- with(df, cbind(data.frame(
    dose = doses,
    type = 'C',
    fractions = 1000,
    p_my_analysis = i + a * doses
  ), df))
  
  rbind(acute, chronic)
})
projections$p_10B3 <- predict(model_10B3(aggregate),
                              newdata=projections)

# Show
g <- aggregate
f <- function(x) round(x, 2)
g$cluster <- with(g, paste0(
    cluster, '\n',
    #'a=', f(a), 
    #' B=', f(B), 
    ' ddref =', f((a + B) / a)
))
projections$cluster <- projections$cluster
projections$cluster <- with(projections, paste0(
    cluster, '\n',
    #'a=', f(a), 
    #' B=', f(B), 
    ' ddref =', f((a + B) / a)
))

ggplot(g, aes(
    dose, 
    1/age, 
    label=type,
    group=type)) + 
    geom_errorbar(aes(
        ymin=1/age + (vi + tau2)^0.5, 
        ymax=1/age - (vi + tau2)^0.5
        ), alpha=0.5, width=.05, color='red') +
    geom_errorbar(aes(
        ymin=1/age + vi^0.5, 
        ymax=1/age - vi^0.5
    ), alpha=0.5, width=.1) +
    geom_text(size=4) + 
    geom_smooth(
        data = projections,
        aes(dose, p_10B3), 
        method='lm', 
        formula='y ~ x + I(x^2)',
        se=FALSE,
        color='black'
    ) + 
    geom_smooth(
        data = projections,
        aes(dose, p_my_analysis), 
        method='lm', 
        formula='y ~ x + I(x^2)',
        se=FALSE,
        color='red'
    ) + 
    facet_wrap(~ cluster, scales="free_y")  
```

![plot of chunk unnamed-chunk-30](Figs/unnamed-chunk-30.png) 

```r

#TODO(ben) clean, document, commit these changes
#TODO(ben) reorder cluster factor by number of animals
#TODO(ben) put ggsave in a util function (or just use something better)


ggsave_for_ppt('meta_regression.png')
```


#### Results

Curves don't change that radically, though standard errors often change rather dramatically!

_____________________________________________________________________________  
^ back to [table of contents](#contents)








<a name="10B4-meta-all"></a>

Meta Regression profiles on all data
========================================================
*Last update: June 2013*

Show graphs like Storer 1979 10B4 but for all data using the random effects meta regression.


```r

# Libraries
library(ggplot2)
library(plyr)
library(metafor)

# Data
setwd("~/janus/scripts")
data <- readRDS("data/ddref.rds")

# Helpers
ggsave_for_ppt <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
    height = 7.68, units = "in"))

model_10B4 <- function(data, o) {
    glm(I(1/age) ~ I(dose + o * dose^2/(fractions)), data = data, weights = n)
}
model_meta <- function(data, o) {
    data$curved_dose <- with(data, dose + o * dose^2/(fractions))
    rma(yi, vi, mods = cbind(curved_dose), data = data, method = "ML")
}
normalize_likelihood <- function(l, delta) {
    l <- l - max(l)
    l <- exp(l)
    l <- l/sum(l)
    l <- l/delta
    
    l
}

# Define Acute
chronic <- data$fractions > 1
data$type <- "A"
data$type[chronic] <- "C"

# Mean Lifespans
aggregate <- ddply(data, .(cluster, group_id, sex), function(df) {
    u <- function(x) paste(unique(x), collapse = " ")
    dont_aggregate <- c("lifespan", "id", "X", "n")
    n <- nrow(df)
    data.frame(llply(df[, !names(df) %in% dont_aggregate], u), age = mean(df$lifespan), 
        sd = sd(df$lifespan)/n^0.5, n = nrow(df))
})

# Prepare for Meta
aggregate$yi <- with(aggregate, 1/age)
aggregate$vi <- with(aggregate, (1/age - 1/(age + sd))^2)

# Restore Sanity
numerics <- names(data)[laply(data, is.numeric)]
for (n in numerics) {
    if (n %in% names(aggregate)) {
        aggregate[, n] <- as.numeric(as.character(aggregate[, n]))
    }
}

# Model
low = -2
high = 6
delta = 0.01  # Reduce to increase resolution
o_range = (low/delta):(high/delta) * delta

# Model
aggregate <- ldply(o_range, function(o) {
    ddply(aggregate, .(cluster, sex), function(df) {
        
        # BEIR
        m <- model_10B4(df, o)
        df$l_10B4 <- logLik(m)
        
        # Random Effects
        m <- model_meta(df, o)
        df$l_meta <- logLik(m)
        
        df$o <- o
        df
    })
})

# Summarize Effect
get_summary_effect <- function(o, l) {
    aggregate <- data.frame(o, l)
    summary <- ddply(aggregate, .(o), function(df) {
        sum(df$l, na.rm = TRUE)
    })
    names(summary) <- c("o", "l")
    
    with(summary, data.frame(o, l))
}

summary_10B4 <- with(aggregate, get_summary_effect(o, l_10B4))
summary_meta <- with(aggregate, get_summary_effect(o, l_meta))
summary <- data.frame(o <- summary_10B4$o, l_10B4 <- normalize_likelihood(summary_10B4$l, 
    delta), l_meta <- normalize_likelihood(summary_meta$l, delta))

# Normalize
aggregate <- ddply(aggregate, .(cluster, sex), function(df) {
    df$l_10B4 <- normalize_likelihood(df$l_10B4, delta)
    df$l_meta <- normalize_likelihood(df$l_meta, delta)
    df
})


# Show As in 10B4 http://www.nap.edu/openbook.php?record_id=11340&page=257
a <- aggregate

show <- function(g) {
    g$cluster <- as.factor(as.character(g$cluster))
    g$l_10B4 <- pmin(g$l_10B4, 1)
    g$l_meta <- pmin(g$l_meta, 1)
    suppressWarnings(print(ggplot(g, aes(o, l)) + geom_path(aes(o, l_10B4), 
        color = "black") + geom_path(aes(o, l_meta), color = "red") + ylim(0, 
        1) + facet_wrap(~cluster)))
}

g <- a
show(g)
```

![plot of chunk unnamed-chunk-31](Figs/unnamed-chunk-311.png) 

```r
ggsave_for_ppt("meta_regression_profile.png")

summary$l_10B4 <- pmin(4, summary$l_10B4)
summary$l_meta <- pmin(4, summary$l_meta)
ggplot(summary, aes(o, l)) + geom_path(aes(o, l_10B4), color = "black") + geom_path(aes(o, 
    l_meta), color = "red") + ylim(0, 4)
```

![plot of chunk unnamed-chunk-31](Figs/unnamed-chunk-312.png) 

```r
ggsave_for_ppt("meta_regression_summary_effect.png")
```


#### Results

We still seem to have biased likelihood estimates, but things are improving a bit...

_____________________________________________________________________________  
^ back to [table of contents](#contents)



