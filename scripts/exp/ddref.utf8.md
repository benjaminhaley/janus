[DDREF][public link]
========================================================
Measure ddref, for my thesis ([code][code link]).
benjamin.haley@gmail.com
*last update: June 2014*


[public link]: http://rpubs.com/benjaminhaley/ddref
[code link]: https://github.com/benjaminhaley/janus/blob/master/scripts/exp/ddref.Rmd

Abstract
========================================================
TODO write abstract


<a name="contents"></a>

Table of contents
========================================================

**Background**

  - [Introduction](#introduction) - What are we doing here?
  - [Defining DDREF](#defining_ddref) - What is the equation?
  - [Log Likelihood](#loglike) - How is log likelihood calculated?
  - [To log or not?](#log-or-not) - Should data be fit to a linear scale or a log linear scale and should a threshold be used when estimating DDREF?

**Data**

  - [Data Funnel](#data_funnel) - Which data will we analyze?
  - [Data Cleaning](#cleaning) - Damn, data, you look good!
  - [Visualize](#visual_concordance) - Show what the data looks like.
  - [Atomic Bomb Survivors](#lss) - Load data from atomic bomb survivors as well.

**Analysis**

  - [Reproduce BEIR 10-2](#10-2) - Reproduce dose response for atomic bomb survivors. (fail)
  - [Reproduce BEIR 10-3](#10-3) - Reproduce profile likelihood curves that combine animal data with outcomes from atomic bomb survivors.
  - [Reproduce BEIR 10-3 lss curve](#10-3-lss) - Reproduce DDREF likelihood profiles for atomic bomb survivors.
  - [Reproduce BEIR 10B2](#10B2) - Show that we can fit the same model of cancer risk vs. dose as BEIR VII.
  - [Reproduce BEIR 10B3](#10B3) - Show that we can fit the same model of lifespan vs. dose as BEIR VII.
  - [Reproduce BEIR 10B4](#10B4) - Show that we can reproduce the likelihood profiles from animal data in BEIR VII.
  - [10B3 on all data](#10B3-all-data) - Fit 1/lifespan models on all of the data.
  - [10B4 on all data](#10B4-all-data) - 1/lifespan models profile likelihoods on all of the data.
  - [10B3 with metaregression](#10B3-meta) - Reproduce dose response figure applying the principals of meta-regression
  - [10B4 with metaregression](#10B4-meta) - Reproduce profile likelihood figure applying the principals of meta-regression.
  - [10B3 metareression on all data](#10B3-meta-all) Apply meta regression to all of the datasets.
  - [10B4 metareression on all data](#10B4-meta-all) Apply meta regression to all of the datasets to generate profiles.

**Supplemental**

  - [Concordance](#concordance) - Tables describing the data in detail.
  - [10B4 with only acute data](#10B4-with-only-acute-data) reproduce lifespan analysis using only the acute data which is available as individual level data.
  - [Metaregression](#metaregression) - Show the principals of
  metaregression.

___________________________________________________________________




<a name="introduction"></a>

Introduction
========================================================
*Last update: May 2013*


Radiation protection standards are based primarily upon observations of atomic bomb survivors supplemented by animal studies ??BEIR VII source??.  The animal studies are used, amoung other things to estimate the effects of protraction, i.e. would the dose delivered by the atomic bombs have induced as many health effects if it were delivered more slowly and spread across a broader population like modern radiation exposures?

It is generally ??linear quadratic citations??, though not unanimously ??disenting citations??, assumed that the dose response to low-LET radiation has a linear-quadratic form.  Concretely:

`risk ~ a * Dose + B * Dose^2`

This formula is based on observations of cellular systems which regularly show a linear quadratic response to radiation.  Specifically, log(cell viability) and gross chromosomal abberations show a linear quadratic response to radiaiton in most cellular systems ??citation??.

Mechanistically this is explained by DNA damage and repair.  A single track of low-LET radiation may lead to inviabilty or


TODO: why does cell viability need a log opperation to be linear quadratic where chromosomal abberations do not?  Is it possible that mortality also needs a log transformation?




Health effects, especially carcinogenesis rates, observed in the atomic bomb survivors are divided by a dose and dose rate effectiveness factor (DDREF) to estimate the health effects that would be observed if the same dose had been protracted.


TODO fill in ?? above


TODO(later) write an abstract
TODO(later) read and spruce up this doc





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

^ back to [table of contents](#contents)

___________________________________________________________________

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
data <- data.frame(
    y=1:100,
    x=rnorm(100, 1:100)
)
n <- nrow(data)
m <- glm(y ~ x, data=data)
data$p <- predict(m)
data$e <- data$y - data$p
o2 = with(data,
    sum(e^2) / (n)
)
o2
```

```
## [1] 1.143
```

```r
l <- with(data,
    -(n/2) * log(2*pi) +
    -(n/2) * log(o2) +
    -(1/(2*o2)) * sum(e^2)
)
l - as.numeric(logLik(m))
```

```
## [1] 0
```

^ back to [table of contents](#contents)

___________________________________________________________________

<a name="data_funnel"></a>

DATA: Data Funnel
========================================================
*Last update: April 2013*


### Introduction:
Create a data set that might be used for DDREF analysis and
make a description of this data.


```r
# Common functions
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# DATA
d <- readRDS('data/external5.rds')

# Report for funnel graph
count <- function(data){
  count_unique <- function(x) length(unique(x))
  with(data, c(
    studies      = count_unique(file),
    clusters     = count_unique(cluster),
    treatments   = count_unique(group_id),
    animals      = count_unique(id),
    'not vetted' = count_unique(id) - count_unique(id[is_vetted]),
    'to exclude' = count_unique(id[exclude])
  ))
}

filter_by_n_groups <- function(data, threshold=3){
  ddply(data, .(cluster), function(df){
    n_groups = length(unique(paste(df$dose,
                                   df$dose_rate,
                                   df$fractions)))
    if(n_groups >= threshold){
      return(df)
    } else {
      return(NULL)
    }
  })
}

# Define
bad_qualities <- c(
  'accel. alpha local',
  'accel. alpha whole body',
  'accel. neutrons 0.1-10 MeV',
  'neutrons 1-10 MeV',
  'neutrons C-252',
  'neutrons fission',
  'neutrons>10 MeV',
  'X-rays local',
  'gamma-rays local',
  'Bremsstrahlung > 3MeV.'
)

# Aliases
# Allow a more concise representation of a name.
# For instace ♂ is preferable to Male
aliases <- list(
  'quality'= c(
    'gamma-rays Co-60'='γ-ray',
    'gamma-rays Co-60, gamma-rays Co-60'='γ-ray',
    'gamma-rays Co-60, gamma-rays Co-60, gamma-rays Co-60'='γ-ray',
    'gamma-rays Cs-137'='γ-ray',
    'gamma-rays whole body'='γ-ray',
    'gamma-rays'='γ-ray',
    'X-rays whole body'='X-ray'
  ),
  'sex'= c(
    Both='♂/♀',
    Female='♀',
    Male='♂'
  ),
  'lab'= c(
    '2'='CEN-FAR',
    '3'='ENEA',
    '9'='SCK/CEN',
    '11'='TNO',
    '1002'='DAVIS',
    '1003'='ANL',
    '1005'='ITRI',
    '1007'='ORNL',
    '1008'='CSU'
  ),
  'strain'=c(
    'beagle'='Beagle'
  )
)


threshold_dose <- 1.5

# Fix
# TODO(later) move these fixes to radiation.R

# Stray fixes
## Some of the B6CF1 mice are missing their species
d[d$strain == 'B6CF1', 'species'] <- 'Mouse'
## One animal is listed as a control despite having a dose, remove her
contradictory_dose_and_quality <- (d$quality == 'none (controls)' &
                                     d$dose != 0 &
                                     !is.na(d$dose) &
                                     !is.na(d$quality))
d <- d[!contradictory_dose_and_quality,]

# NA doses
d$dose[is.na(d$dose)] <- 0
d$dose_rate[is.na(d$dose_rate)] <- 0
d$fractions[d$dose == 0] <- 0
d$dose_rate[d$dose == 0] <- 0

# NA quality
d$quality[is.na(d$quality)] <- 'none (controls)'

# Add missing fractions
d$fractions[is.na(d$fractions)] <- 1
d$fractions[d$fractions == 0] <- 1

# Add fractions seperated by days
d$day_fractions <- d$fractions
s <- d$fraction_interval < 1 & !is.na(d$fraction_interval)
d$day_fractions[s] <- d$fractions[s] * d$fraction_interval[s]

# Add lab
d$lab <- sub('(^[0-9]*).*$', '\\1', d$study_id)

# If assignment age is not listed, assume it is zero
d$assignment_age[is.na(d$assignment_age)] <- 0

# If an animal is a control, it should have no age at treatment
# Nor an age at last treatment
d$age_at_treatment[d$dose == 0] <- NA
d$age_at_last_treatment[d$dose == 0] <- NA

# Age at last treatment
d$age_at_last_treatment <- d$age_at_treatment
s <- !is.na(d$fraction_interval)
d$age_at_last_treatment[s] <- with(d[s,],
                                   age_at_treatment +
                                   fraction_interval * (fractions - 1),
)

# Assign aliases
# Replace all values in a given column with their aliases
# e.g. replace gamma-rays with γ
for(column in names(aliases)) {
  for(name in names(aliases[[column]])) {
    alias = aliases[[column]][name]
    d[d[column] == name & !is.na(d[column]),column] <- alias
  }
}
```

#### Define Clusters
In general we want to cluster on:

  `lab, species, strain, and sex`

```r
d$cluster = with(d, paste(sex,
                          strain,
                          species,
                          lab, sep='--'))
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
d$intended_assignment_age <- d$assignment_age
labs_that_recorded_true_age_at_assignment <- c(
  'ANL'
)
clusters_that_recorded_true_age_at_assignment = unique(
  d$cluster[
    d$lab %in% labs_that_recorded_true_age_at_assignment
])
for(c in clusters_that_recorded_true_age_at_assignment) {
  d$intended_assignment_age[d$cluster == c] <-
    median(d$assignment_age[d$cluster == c])
}
d$cluster <- paste(d$cluster, d$intended_assignment_age, sep='--')
```

##### Duplicate controls
Control animals may control for multiple clusters.  For example the same mouse could control for a group exposed to gamma rays and others exposed to x-rays.  Therefore control groups ought to be duplicated and included in each cluster that they might control for.

Concretely, control animals should match sex, species, strain, assignment age, and lab.  Controls should be duplicated for each unique quality and age of first exposure provided the aforementioned criteria are met.


```r
d <- ddply(d, .(cluster), function(df) {
  control   <- df[df$quality == 'none (controls)',]
  treatment <- df[df$quality != 'none (controls)',]

  # Create a cluster for each non control intended age of treatment
  ddply(treatment, .(quality), function(treatment_group){

    # Add control to each treatment group
    df2 <- rbind(treatment_group, control)

    # Define quality by the treatment group
    # i.e.
    #    quality = 'none (control)'  ->  quality = 'gamma'
    df2$quality <- treatment_group$quality[1]

    # Add quality to the cluster name
    df2$cluster <- with(df2, paste(cluster,
                                   quality,
                                   sep='--'))
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
d <- d[!d$quality %in% bad_qualities,]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         24         43        457      76096      23213      15465
```

```r
# Dose below threshold (as in BEIR VII)
d <- d[!(d$dose > threshold_dose),]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         23         35        230      45730       6220       9662
```

```r
# Lifespan not NA
d <- d[!is.na(d$lifespan),]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         23         35        230      45722       6220       9662
```

```r
# No other treatments
d <- d[d$other_treatments == 'none',]
count(d)
```

```
##    studies   clusters treatments    animals not vetted to exclude 
##         21         34        175      43043       4832       8516
```

```r
# Died before their 'assignment age'
# TODO(later) why should there be any mice that died before their assignemtn age?
d <- d[d$lifespan > d$assignment_age,]
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
for(ex in exclusions){
  d <- d[!d$reason == ex,]
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
#    studies   clusters treatments    animals not vetted to exclude
#          8         17         95      29072          0          0

# How many duplicates after filtering?
table0(d$duplicates)  # 19462 FALSE 9610 TRUE
```

```
## 
## FALSE  TRUE 
## 19462  9610
```

```r
# Warnings
# show, but do not remove
warnings <- sort(unique(d$warning_reason))
warnings <- warnings[warnings != ""]
d_wo_warnings <- d
for(w in warnings){
  d_wo_warnings <- d_wo_warnings[
    !d_wo_warnings$warning_reason == w,
    ]
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
#    studies   clusters treatments    animals not vetted to exclude
#          6         13         73      18903          0          0

# Save
setwd('~/janus')
saveRDS(d, 'data/funneled.rds')
```

^ back to [table of contents](#contents)

___________________________________________________________________

<a name="cleaning"></a>

Clean
========================================================
DDREF data has been filtered.  Now its time to prettify it.


```r
# Common functions
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
setwd('~/janus')
d <- readRDS('data/funneled.rds')

# Is acute?
d$acute <- d$fractions == 1 | d$dose == 0
d$protracted <-  d$dose > 0 &
                 (d$fractions > 1 |
                  d$dose_rate < 0.01)

# Observations per cluster
d = d %>%
  group_by(cluster) %>%
  mutate(n_in_cluster=length(cluster))

# Give the clusters pretty names
for(c in unique(d$cluster)) {
  elements = as.list(strsplit(c, '--')[[1]])
  names(elements) <- c('sex', 'strain', 'species', 'lab', 'age', 'quality')
  pretty_cluster = with(elements, paste(
    sex, strain, pluralize(species), lab,
    '\n', quality, 'at', age, 'days old'))
  d$cluster[d$cluster == c] <- pretty_cluster
}

# Define Acute
chronic <- d$fractions > 1
d$type <- 'A'
d$type[chronic] <- 'C'

# Order clusters
# By number of observations.  This will put the cluster with
# the most observations first in ggplots
sort_by_n <- function(x) {
  factor(x,
         levels = names(sort(table(x), decreasing=TRUE)))
}
d$cluster = sort_by_n(d$cluster)
d$cluster_id <- as.numeric(d$cluster)
d$cluster <- with(d, paste0(cluster_id, ' - ', cluster))
d$cluster = sort_by_n(d$cluster)

# Save Data for later use
saveRDS(d, 'data/ddref.rds')
write.csv0(d,file='data/ddref.csv')
```

### Results
Data is so fresh and so clean.

________________________________________________________

<a name="visual_concordance"></a>

Visualize
========================================================
*Last Update: April 2014*


Now that the data is reasonably clean, show what it looks
like.


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
setwd('~/janus')
d <- readRDS('data/ddref.rds')
```




```r
# TODO(later): figure out what is the cause of early effects in ORNL data based on the pathology codes released with that dataset.

ggplot(d,
       aes(lifespan,
           color=dose,
           group=factor(paste(dose,
                              dose_rate,
                              fractions)),
           y=..scaled..)) +
  geom_density(adjust=2) +
  facet_wrap(~ cluster, scales='free') +
  scale_colour_gradient(
    guide = guide_legend(title = "Dose (Gy)"),
    trans = "sqrt"
  ) +
  geom_vline(
    aes(xintercept=intended_assignment_age),
    alpha=0.5
  ) +
  expand_limits(x = -4)
```

<img src="Figs/unnamed-chunk-9.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width="983.04" />

#### Figure: Lifespan by dose and cluster
Density plots show the frequency, y-axis, of a given lifespan (in days), x-axis.  Sepperate density curves are shown for each distinct dose and dose rate.  The total dose delivered is labeled by color as shown in the figure legend.  The mean age at first exposure is denoted by a gray vertical line on each graph.

Clusters are sepperated by facets and labeled with sex, strain, species, lab, quality of radiation, and mean age at first exposure (in days).  The clusters are ordered by the number of animals in the cluster.  The cluster with the most animals, Male RFM/Un Mice from ORNL, are at the top left and the cluster with the least animals is furthest to the right on the bottom row.

##### Related observations:
- **Different clusters show very different responses to the same total dose.**  For example, compare Female RFM/UN Mice from ORNL (1) to Female C57Bl/6Bd mice from the same institution (10).  The only difference between these experiments is the strain of mice used.  The former show a strong response to gamma rays, the latter a weak response or no response.  These differences could reflect strain-specific differences in radiosensitivity or methodological errors in the way that the studies were conducted.
- **The cluster used to estimate DDREF shows a particularly strong and early response**.  RFM/Un mice from ORNL were used as the acute condition in the estimate of DDREF from BEIR VII (chronic exposure is not shown here because individual level data is not available).  Notably, this is the strongest radiation response seen in any cluster, including other similar ones from ORNL.  Moreover, the response is bimodal, with an early effect that is not seen in other clusters.  This suggests that the acute effects are being over-estmimated in BEIR VII and that DDREF should be closer to one.


```r
n_unique <- function(...) length(unique(paste(...)))

comparable_doses <- d %>%
  ungroup() %>%
  group_by(cluster_id, dose) %>%
  summarize(n_groups=n_unique(fractions, dose_rate)) %>%
  filter(n_groups > 1)

g <- d %>%
  ungroup() %>%
  filter(cluster_id %in% comparable_doses$cluster_id,
         dose %in% c(comparable_doses$dose, 0))

ggplot(g,
       aes(lifespan,
           color=dose,
           group=factor(paste(dose,
                              dose_rate,
                              fractions)),
           linetype = protracted,
           y=..scaled..)) +
  geom_density(adjust=2) +
  facet_grid(dose~cluster, scales='free') +
  scale_colour_gradient(
    guide = guide_legend(title = "Dose (Gy)"),
    breaks= c(0,0.5,1.0,1.5),
    limits= c(0,1.5)
  ) +
  geom_vline(
    aes(xintercept=intended_assignment_age),
    alpha=0.5
  ) +
  expand_limits(x = -4)
```

<img src="Figs/unnamed-chunk-10.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" width="983.04" />

#### Figure: Directly comparable protracted exposures
Similar to the figure above, except that the data has been limited to those cases were there are two groups in the same cluster with the same dose, but differences in dose rate or number of fractions.  Doses that were delivered more slowly are labeled as protracted (dotdash line).  Facets of the graph are defined by cluster and total dose delivered.  Controls are included for comparison.

##### Additional details
In cluster 4, doses were broken into 10 or 8 fractions seperrated by three hours or 1 day respectively.  In cluster 6 doses were broken into 10 fractions sepperated by one day.  Notably both of these experiments were conducted at the same laboratory (SCK/CEN) using the same irradiator (a CS-137 source).

*Note, ORNL also conducted experiments with directly comparable doses both acute and protracted, but individual level data is not avaiable for the acute exposures.

##### Observations
The effects of fractionation are decidely ambivilant.  Cluster 4 shows signs of high DDREF, cluster 6 shows signs of a DDREF close to 1.  The only substantial difference between these experiments is the strain used.  Three facts might account for this observation, perhaps DDREF is strain specific, perhaps there were methodological errors in the way these studies were conducted, or perhaps random events are obscuring the true pattern.



```r
d <- d %>%
  group_by(cluster, dose, dose_rate, fractions) %>%
  arrange(lifespan) %>%
  mutate(survival=rank(-lifespan) / length(lifespan))

ggplot(d,
       aes(lifespan,
           survival,
           color=dose,
           group=factor(paste(dose, dose_rate, fractions)))) +
  geom_path() +
  facet_wrap(~ cluster, scales='free') +
  scale_color_continuous(
    guide = guide_legend(title = "Dose (Gy)"),
    trans = "sqrt"
  ) +
  geom_vline(
    aes(xintercept=intended_assignment_age),
    alpha=0.5
  ) +
  expand_limits(x = -4)
```

<img src="Figs/unnamed-chunk-11.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" width="983.04" />

#### Figure: Survival plots
As before, but presented as traditional survival plots instead.  The y axis represents the fraction of animals alive a at a given time.  Other elements are organizeds as before.

##### Related observations
This does not tell us anything new, I've included it because survival plots are standard.  Personally I find them less informative than the lifespan density plots shown above.



```r
d <- d %>%
  ungroup() %>%
  group_by(cluster, dose, dose_rate, fractions) %>%
  arrange(lifespan) %>%
  mutate(group_survival=rank(-lifespan) / length(lifespan))

# How many animals were alive after X days in each cluster?
d <- d %>%
  ungroup() %>%
  group_by(cluster) %>%
  arrange(lifespan) %>%
  mutate(cluster_survival=rank(-lifespan) / length(lifespan))

ggplot(d,
       aes(lifespan,
           group_survival - cluster_survival,
           color=dose,
           linetype=protracted,
           group=factor(paste(dose, dose_rate, fractions)))) +
  geom_path() +
  facet_wrap(~ cluster, scales='free_x') +
  scale_color_continuous(
    guide = guide_legend(title = "Dose (Gy)"),
    trans = "sqrt"
  ) +
  geom_vline(
    aes(xintercept=intended_assignment_age),
    alpha=0.5
  ) +
  ylim(-0.4, 0.4)
```

<img src="Figs/unnamed-chunk-12.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" width="983.04" />

#### Figure: Relative survival plots
Like the survival plot above, expecept that survival group within the entire cluster is subtracted from survival within a particular group.  The result is a graph which emphasizes the differences between survival.  Groups with positive values (above y=0) are surviving longer than the cluster average, groups with negative values (below y=0) are surviving less time than the cluster average.  In this graph groups with protracted exposures (multiple fractions or dose rates < 0.01 Gy/min)

##### Related observations
- **Useful graph, but difficult to interpret** This is actually one of my favorite visualizations of this data because it really uses all of the space of the graph to convey information.  However, it takes some thought to wrap one's head around it, so it might not be suitable for presentation in a paper.
- **Cluster 1 does not look as extreme in this graph** It is clear that some clusters show effects roughly as strong as cluster 1 (the cluster used to estimate acute effects in the DDREF estimate).  Concretely clusters 7 (Female BC3F1 mice) and 9 (Female C3Hf/Bd Mice) have responses of similar strength.  Still, no responses happen as early as those in cluster 1, so it is still a suspicious case suggesting that DDREF has been over-estimated.
- **Protraction appears to alievate or eliminate the effects of exposure** In this graph it was possible to meaningfully show protracted groups.  The general trend is that protraction seems to be beneficial.  This is especially obvious in cluster 4, but generally corroborated with clusters 2, 3, and 6 in which the protracted treatment groups tend to cluster with the control and low dose groups.




^ back to [table of contents](#contents)

__________________________________________________________________

<a name="lss"></a>

Atomic bomb survivor data
========================================================
*Last update: April 2014*


For comparison, let's load data from the atomic bomb survivors, [lss14][lss], and see how lifespan changes as a function of dose in these populations using similar visualizations.

*Acknowledgement: This report makes use of data obtained from the Radiation Effects Research Foundation (RERF), Hiroshima and Nagasaki, Japan. RERF is a private, non-profit foundation funded by the Japanese Ministry of Health, Labour and Welfare and the U.S. Department of Energy, the latter through the National Academy of Sciences.The conclusions in this report are those of the authors and do not necessarily reflect the scientific judgment of RERF or its funding agencies.*

*Please send a copy of any reprints that make use of these data to:*
*Archives Unit, Library and Archives Section*
*Department of Information Technology*
*Radiation Effects Research Foundation*
*5-2 Hijiyama Park*
*Minami-ku Hiroshima, 732-0815 JAPAN*

[lss]: http://rerf.jp/library/dl_e/lss14_document.pdf


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Helper functions
get_max_map <- function(min_map, max=Inf) {
  max_map <- c(min_map[2:length(min_map)], max)
  names(max_map) <- names(min_map)
  max_map
}
get_mean_map <- function(min_map, max=Inf) {
  mean_map <- (min_map + get_max_map(min_map, max)) / 2
  mean_map
}

# Load data
setwd('~/janus/')
data <- read.csv('data/lss14/lss14.csv')

# Define Terms
sex_map <- c('1'='♂',
             '2'='♀')
agecat_min_map <- c('1'=0,
                    '2'=5,
                    '3'=10,
                    '4'=15,
                    '5'=20,
                    '6'=25,
                    '7'=30,
                    '8'=35,
                    '9'=40,
                    '10'=45,
                    '11'=50,
                    '12'=55,
                    '13'=60,
                    '14'=65,
                    '15'=70,
                    '16'=75,
                    '17'=80,
                    '18'=85,
                    '19'=90,
                    '20'=95,
                    '21'=100)

agecat_mean_map <- get_mean_map(agecat_min_map, 120)
dose_min_map <- c('1'=0.000,
                  '2'=0.005,
                  '3'=0.020,
                  '4'=0.040,
                  '5'=0.060,
                  '6'=0.080,
                  '7'=0.100,
                  '8'=0.125,
                  '9'=0.150,
                  '10'=0.175,
                  '11'=0.200,
                  '12'=0.250,
                  '13'=0.300,
                  '14'=0.500,
                  '15'=0.750,
                  '16'=1.000,
                  '17'=1.250,
                  '18'=1.500,
                  '19'=1.750,
                  '20'=2.000,
                  '21'=2.500,
                  '22'=3.000)
dose_mean_map = get_mean_map(dose_min_map, 4)

ctime_min_map <- c('1'=1951,
                  '2'=1956,
                  '3'=1961,
                  '4'=1966,
                  '5'=1971,
                  '6'=1976,
                  '7'=1981,
                  '8'=1986,
                  '9'=1991,
                  '10'=1996,
                  '11'=2001)

threshold = 1.5001

# Spruce up the data
data <- data %>%
  # Translate
  # Values into different units
  mutate(sex = sex_map[sex],
         agex = agecat_min_map[agexcat],
         age  = agecat_min_map[agecat],
         dose = dose_mean_map[dosecat],
         ctime= ctime_min_map[ctime])

# Save a copy for future use
write.csv0(data, 'data/lss14/lss14_spruced_up.csv')

# Shorten the data
data <- data %>%
  # Thin
  # Only select the colums of interest
  select(city,
         sex,
         agex,
         age,
         dose,
         subjects,
         death,
         ctime) %>%
  # Shorten
  # Remove those with a dose above threshold
  filter(dose < threshold)

# One row per death
dead <- ldply(unique(data$death), function(n) {
  d <- data[data$death == n,]
  d[rep(1:nrow(d), n),]
})

# Prove it was actually done
sum(data$death) == 49879
```

```
## [1] TRUE
```

```r
nrow(dead) == 49879
```

```
## [1] TRUE
```

```r
# Clean dead
# Make it truly represent one individual per row by removing
# aggregated values
dead <- dead %>%
  select(-subjects, -death) %>%
  mutate('alive' = 0)

# One row for each person still alive
# Note, defining the minimum age for people still alive is
# a little challenging because the relationship between age
# and calendar time is not one to one.  Concretely someone
# exposed at age 0-5 might be 5, 10, or 15 years old in 1950
table0(dead$age[dead$agex == 0 &
                dead$ctime == 1951])
```

```
## 
##  5 10 15 
## 27 15  1
```

```r
# this makes it difficult to know their projected age at some
# later date, like 2004.
#
# To overcome this challenge we will calculate the distribution
# of min calendar time - min age for a given age at exposure and
# use this distribution of difference to project a distribution
# of min ages for a new calendar time (2004 and later, for which
# we have no more data.
living <- ddply(data, .(agex, dose, city, sex), function(df){
  subjects = sum(df$subjects)
  dead = sum(df$death)
  living = subjects - dead
  .all <- function(x) unique(df[x])

  if(living == 0){
    return(NULL)
  }

  # There should be one row for each living person
  # and age should be a distribution.
  # see explanation above
  same_agex = data[data$agex == df$agex[1],]
  age = sample(with(same_agex, 2004 + age - ctime),
               size=living,
               replace=TRUE)

  data.frame(
    city = .all('city'),
    sex = .all('sex'),
    agex = .all('agex'),
    age = age,
    dose = .all('dose'),
    ctime = 2004,
    alive = 1)
})

# Merge living and dead
long <- rbind(dead, living)

# Prove that long is the correct size
nrow(long) == 85498
```

```
## [1] TRUE
```

```r
sum(data$subjects) == 85498
```

```
## [1] TRUE
```

```r
# Prove that ages look reasonable
ggplot(long, aes(ctime, age, color=alive)) +
  geom_point(alpha=0.002) +
  facet_wrap(~agex)
```

<img src="Figs/unnamed-chunk-14.png" title="plot of chunk unnamed-chunk-14" alt="plot of chunk unnamed-chunk-14" width="983.04" />

```r
# Reduce resolution
# (as it is there are too many categories for graphing)
low_res <- long %>%
  mutate(agex = floor(agex/20)*20,
         age_string = paste0(agex, '+ years'),
         lifespan = age,
         dose = round(dose * 2)/2)
```


```r
g <- low_res
# Show it off
ggplot(g %>% filter(!alive),
       aes(x=lifespan,
           color=dose,
           group=factor(paste0(dose)),
           y=..scaled..)) +
  geom_density(adjust=2) +
  scale_colour_gradient(
    guide = guide_legend(title = "Mean Dose (Sv)"),
    trans = "sqrt",
    breaks= c(0,0.5,1.0,1.5),
    limits= c(0,1.5)
  ) +
  geom_vline(
    aes(xintercept=agex),
    alpha=0.5
  ) +
  facet_wrap( ~ age_string + sex) +
  xlim(0, 100)
```

<img src="Figs/unnamed-chunk-15.png" title="plot of chunk unnamed-chunk-15" alt="plot of chunk unnamed-chunk-15" width="983.04" />

#### Figure: LSS lifespan by dose, sex, and age at exposure
Analogous the the lifespan density plots shown for animals except that this details survivors of the atomic bomb exposures in Hiroshima and Nagasaki.  Lifespan of survivors who died is shown on the x axis in years, the height of each line shows the relative frequency of this lifespan compared to others.  All density curves are scaled so that their maximum value is 1.  Density curves are groups by age at exposure and gender (shown in the facet labels) and by minimum total dose (Sv) designed by line color.  Age of exposure is also designated by a vertical line.

##### Related observations
- **Artifacts are introduced by those still alive** Most survivors expososed at 0-20 years old and many survivors exposed at 20-40 years old are still alive.  That means that only the 40+ year old survivors represent a complete dataset.
- **Effect are not as strong as those seen in ORNL data** qualitatively the effects of radiation exposure seem milder than those observed in the ORNL data set.  Thse are no strong signs of an early effect.



```r
# Note by multiplying age * alive plus a small amount we ensure
# that survivors who are still alive always have a higher rank
# than those that have already died.
g <- low_res %>%
  group_by(agex, dose, sex) %>%
  arrange(age) %>%
  mutate(survival=rank(-age * (alive + 0.0001)) / length(age))

ggplot(g %>% filter(!alive),
       aes(age,
           survival,
           color=dose,
           group=factor(dose))) +
  geom_path() +
  facet_wrap(~ age_string + sex) +
  scale_colour_gradient(
    guide = guide_legend(title = "Mean Dose (Sv)"),
    trans = "sqrt",
    breaks= c(0,0.5,1.0,1.5),
    limits= c(0,1.5)
  ) +
  geom_vline(
    aes(xintercept=agex),
    alpha=0.5
  )
```

<img src="Figs/unnamed-chunk-16.png" title="plot of chunk unnamed-chunk-16" alt="plot of chunk unnamed-chunk-16" width="983.04" />

#### Figure: LSS survival by dose, sex, and age at exposure
As before.

##### Observations
- **Not as extreme as ORNL** This view shows more strongly that the effects seen in the ORNL data used in DDREF analysis were stronger than those seen in the atomic bomb survivors.



```r
g <- low_res %>%
  ungroup() %>%
  group_by(agex, dose, sex) %>%
  arrange(age) %>%
  mutate(group_survival=rank(-age * (alive + 0.0001)) / length(age))

# How many people were alive after X years in each cluster?
g <- g %>%
  ungroup() %>%
  group_by(agex, sex) %>%
  arrange(age) %>%
  mutate(cluster_survival=rank(-age * (alive + 0.0001)) / length(age))

ggplot(g %>% filter(!alive),
       aes(age,
           group_survival - cluster_survival,
           color=dose,
           group=factor(dose))) +
  geom_path() +
  facet_wrap(~ age_string + sex) +
  scale_colour_gradient(
    guide = guide_legend(title = "Mean Dose (Sv)"),
    trans = "sqrt",
    breaks= c(0,0.5,1.0,1.5),
    limits= c(0,1.5)
  ) +
  geom_vline(
    aes(xintercept=agex),
    alpha=0.5
  ) +
  ylim(-0.4, 0.4)
```

<img src="Figs/unnamed-chunk-17.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" width="983.04" />

#### Figure: Relative LSS survival
As with the animal data we the difference between survival of an entire cluster (defined by age of exposure and sex) and a group exposed to a particular dose within that group.

##### Related observations
- **Effect of radiation on human survival is weak and noisy** At the most extreme points of the graph surival might differ by 0.1 (10%), and it is often positive, indicating that much of the difference observed is noise.  This is similar to the effects seen on animal survival outside of ORNL which show weak and noisy responses.



```r
# TODO(later): It would be reasonable to add animals exposed at different ages to the same cluster, just as I did for the atomic bomb survivors.  I only need to be sure that I calculate survival based on the total still alive

# Reduce dose resolution
# We want less dose categories total, but agex resolution
# should not be reduced
g <- long %>%
  mutate(dose = round(dose * 2)/2)

# Determine survival
#
# Survival should include the number of people alive at a given age
# and the number of people that are at risk at that same age.
# Because some people were irradiated at an older age they should
# be included in the total at risk only at ages
#
# Note by multiplying age * alive plus a small amount we ensure
# that survivors who are still alive always have a higher rank
# than those that have already died.

g <- g %>%
  group_by(dose, sex) %>%
  arrange(age) %>%
  mutate(n_alive = rank(-age * (alive + 0.0001)),
         n_at_risk = rep(sum(agex <= age), length(age)),
         survival = n_alive / n_at_risk)

ggplot(g %>% filter(!alive),
       aes(age,
           survival,
           color=dose,
           group=factor(dose))) +
  geom_path() +
  facet_wrap(~ sex) +
  scale_colour_gradient(
    guide = guide_legend(title = "Mean Dose (Sv)"),
    trans = "sqrt",
    breaks= c(0,0.5,1.0,1.5),
    limits= c(0,1.5)
  ) +
  ylim(0, 1)
```

<img src="Figs/unnamed-chunk-18.png" title="plot of chunk unnamed-chunk-18" alt="plot of chunk unnamed-chunk-18" width="983.04" />

#### LSS survival by sex
As before except that all age at first exposures are combined into a single graph.  The total survival is calcualated by the number of people alive at a given age divided by the number of people who were exposed at that same age or younger.

##### Related observations
**Less noisy** This looks like the stratified graphs from before, but is noticibly less noisy.  There is a reasonably clean relationship between dose and life shortening.
**Shows the effect of irradiating a population** Unlike the previous graphs, this shows the effects or irradiating an entire population all at once.
**May mask age specific reactions** The stratified graph left the mild impression that exposure to younger population might be more damaging.  If this is true, then this graph completely masks that effect.



```r
# How many people were alive after X years in each group?
g <- g %>%
  ungroup() %>%
  group_by(dose, sex, city) %>%
  arrange(age) %>%
  mutate(n_alive = rank(-age * (alive + 0.0001)),
         n_at_risk = rep(sum(agex <= age), length(age)),
         group_survival = n_alive / n_at_risk)

# How many people were alive after X years in each cluster?
g <- g %>%
  ungroup() %>%
  group_by(sex, city) %>%
  arrange(age) %>%
  mutate(n_alive = rank(-age * (alive + 0.0001)),
       n_at_risk = rep(sum(agex <= age), length(age)),
       cluster_survival = n_alive / n_at_risk)

ggplot(g %>% filter(!alive),
       aes(age,
           group_survival - cluster_survival,
           color=dose,
           linetype=factor(city),
           group=factor(paste0(dose, city)))) +
  geom_path() +
  facet_wrap(~ sex) +
  scale_colour_gradient(
    guide = guide_legend(title = "Mean Dose (Sv)"),
    trans = "sqrt",
    breaks= c(0,0.5,1.0,1.5),
    limits= c(0,1.5)
  ) +
  ylim(-0.4, 0.4)
```

<img src="Figs/unnamed-chunk-19.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="983.04" />

#### Relative LSS survival by sex
As before, but now we show the difference between survival at a given dose and the overall survival of the cohort.

##### Related observations
- **Same story, less noise** This graph tells the same story as before, with substantially less noise.  At its maximum survival, ~75 years old, the difference in survival between people irradiated at > 1 Gy might be 10% of the population. This is a sizable difference to be sure, but not even close to what was seen in the ORNL data.



^ back to [table of contents](#contents)

__________________________________________________________________

<a name="10B2"></a>

Reproduce BEIR 10B2
========================================================
*Last update: May 2014*


Reproduce Figure 10B2 estimated DDREF using cancer incidence data.

### Their approach
BEIR VII used total lifetime cancer indicence adjusted for competing risks for a variety of tumor endpoints.  They only included acute exposures and purposefully excluded various cancer endpoints.

"Tables 1 and 2 because those risk estimates were not adjusted for competing causes of death; (2) results for doses greater than 2 Gy; and (3) results on lymphomas, ovarian cancer, reticulum cell carcinoma, and nonmyeloid leukemias, because these are thought to arise via atypical biological mechanisms, as discussed in Chapter 3, or to reflect an ill-defined combination of cancer types. The risks presented here are based on acute exposures only."
- [BEIR VII pg 254][BEIR VII pg 254]

Models were fit as in 10B3 by fitting linear quadratic models to mean incidence rates per group with a fixed curvature, o.  Concretely:

  `mean incidence rate ~ a * dose + o * a * dose^2`

TODO: determine how this adjustment for competing risks was performed?

### The data source
This analysis was based on data presented in [Table 6 of Edwards 1992][Edwards 1992].  That source could not be found, however most of the data from that source was also present in [Tables 3.1-3.8 of Cox 1995][Cox 1995].  This latter paper was used as the data source and the remaining experiments ??? were estimated from Figure 10B2 directly.

Note, Edwards This source was not found, however most of the same information

[BEIR VII pg 254]: http://www.nap.edu/openbook.php?record_id=11340&page=254

<a name="Edwards 1992"></a>
Edwards, A.A. 1992. Low Dose and Low Dose Rate Effects in Laboratory Animals, Technical Memorandum 1(92). Chilton, UK: National Radiological Protection Board.


```r
# TODO: 10B2 needs to also use meta regression as in 10B4 to see how that influences the effect

# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- read.csv('data/edwards_1992.csv', sep='\t')


# apply selection criteria
data <- data %>%
  filter(protraction == 'Acute',
         !is.na(risk),
         dose <= 2.0)

# Fit model to each facet
data <- ddply(data, .(facet), function(df){
  model <- lm(risk ~ dose + I(dose^2), df, weights=(1/error^2))
  df$seperate_prediction <- predict(model)
  df
})


# Fit models to all data
# vary curvature and measure total likelihood of each model
low = -2
high = 6
delta = .01
o_range = (low/delta):(high/delta) * delta
fits <- ldply(o_range, function(o){
  model <- lm(risk ~ factor(facet) * I(dose + o*dose^2),
              data,
              weights=(1/error^2))
  c(o=o,
    likelihood=logLik(model))
})

fits <- fits %>%
  mutate(likelihood=normalize_likelihood(likelihood, delta))

ggplot(fits, aes(o, likelihood)) + geom_path()
```

<img src="Figs/unnamed-chunk-211.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" width="983.04" />

```r
# Make predictions for most likely model
o = with(fits, o[which.max(likelihood)])
model <- lm(risk ~ factor(facet) * I(dose + o*dose^2),
              data,
              weights=(1/error^2))
data$single_prediction <- predict(model)


# Plot a reproduction of 10B2
ggplot(data,
       aes(x=dose,
           y=risk,
           ymin=risk - error*2,
           ymax=risk + error*2, 32)) +
  geom_point(aes(shape=source)) +
  geom_pointrange(aes(ymin=risk - error*2, ymax=risk)) +
  geom_pointrange(aes(ymin=risk, ymax=risk + error*2)) +
  geom_smooth(aes(x=dose, y=seperate_prediction),
              method="lm",
              formula = "y ~ x + I(x^2)",
              fullrange=TRUE,
              color='black') +
  geom_smooth(aes(x=dose, y=single_prediction),
              method="lm",
              formula = "y ~ x + I(x^2)",
              fullrange=TRUE,
              color='black',
              linetype='dotted') +
  facet_wrap(~ facet + cancer + sex + strain + species,
             scales='free_y',
             ncol=3) +
  xlim(0, 2) +
  xlab("Dose (Gy)") +
  ylab("Risk %")
```

<img src="Figs/unnamed-chunk-212.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" width="983.04" />

```r
# save fits for use in 10B4
write.csv0(fits, 'data/10B2_fits.csv')
```

#### Figure: 10B2 reproduction
A direct reproduction of figure 10B2 from the BEIR VII report.

##### 10B2
This is the original [10B2 figure][10B2-citation] from the beir VII report [beir-10b2].

![10B2-image]

[10B2-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=258
[10B2-image]: http://dl.dropbox.com/u/1131693/bloodrop/10B2.jpg


##### Observations
- **We can reproduce BEIR VII** This graph provides visual evidence that we can accurately reproduce best fit lines used for DDREF estimation from cancer data.  These likelihoods will be combined with DDREF estimates from atomic bomb survivors and animal lifespan studies to provide a final estimate of DDREF.  Only the lifespan estimates are to be updated by this work, but these other estimates must be faithfully reproduced so that they can compbine with the modified lifespan estimates to provide a final modified estimate of DDREF.

__________________________________________________________________

<a name="10B3"></a>

Reproduce BEIR 10B3
========================================================
*Last update: July 2013*

Reproduce the BEIR estimates on the oak ridge lifespan data
from storer 1979 (3575012.pdf).

*note*: Apparently BEIR VII grouped together acute exposures from tables 1 and 2 if they had the same total dose ([without grouping][ungrouped], [with grouping][grouped]).

[ungrouped]: http://dl.dropbox.com/u/1131693/bloodrop/10b3_temp_before.png
[grouped]: http://dl.dropbox.com/u/1131693/bloodrop/10b3_temp_png.png


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- read.csv('data/storer_1979.csv', sep='\t')

# Helpers
model_10B3 <- function(data){
  glm(
    I(1/age - 1/control_age) ~ dose + I(dose^2 / (fractions)) - 1,
    data=data,
    weights=1/sd^2
  )
}

# Constants
threshold_dose_to_model = 1.5001
strain_to_model = 'RFM'
sex_to_model = 'F'
type_in_my_analysis = 'A'
chronic_threshold_for_dose_rate = 0.1
rate_to_fraction = c('6.9e-05'=Inf, '0.4'=1, '0.45'=1)
chronic_to_letter = c('TRUE'='C', 'FALSE'='A')

# Clean
# Here we will:
#   1. Define fractions, chronic, and type
#   2. Combine acute exposures with the same treatment
#      (see note above for a justification)
#   3. Define which data is modeled
data = data %>%
  mutate(fractions = convert(rate, rate_to_fraction),
         chronic = rate < chronic_threshold_for_dose_rate,
         type = convert(chronic, chronic_to_letter)) %>%
  group_by(dose, type, sex, fractions, strain) %>%
  summarise(table=list_unique(table),
            total_n=sum(n),
            rate=mean(rate),
            age= sum(age * n) / total_n,
            sd= (sum(sd^2 * n) / total_n)^0.5) %>%
  mutate(modeled_in_10B3 = dose < threshold_dose_to_model &
                           strain == strain_to_model &
                           sex == sex_to_model,
         in_my_analysis = type == type_in_my_analysis,
         n = total_n) %>%
  select(-total_n) %>%
  ungroup() %>%
  group_by(sex, strain) %>%
  mutate(control_age = age[dose == 0])

# Save for later use
# TODO: re-enable
write.csv0(data, 'data/storer_1979_processed.csv')

# Fake data to predict across
# This allows us to fit two lines across a limited
# data range for graphing
to_predict <- expand.grid(
    fractions = c(1, Inf),
    dose = seq(0, 1.5, 0.1)
)
to_predict$type <- 'A'
to_predict$type[to_predict$fractions > 1] <- 'C'
m <- model_10B3(data[data$modeled_in_10B3,])
to_predict$p <- predict(m, newdata=to_predict) + 1/636.175
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
g <- data[with(data,
    strain == 'RFM' &
    sex == 'F' &
    rate != 0.4
),]
ggplot(g, aes(dose, 1/age)) +
  geom_text(size=5, aes(label=type, group=type)) +
  geom_path(data=to_predict, aes(dose, p, group=type)) +
  scale_y_continuous(breaks = 0.0002*0:4+0.0016)
```

<img src="Figs/unnamed-chunk-23.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" width="983.04" />

### Results
I am capable of reproducing their results [very well][overlay].  There are a few tricks that were not explicitly stated in BEIR VII.

1. They combined treatement groups from tables 1 and 2 if they were
   acute and had the same total dose
2. They only used female RFM mice.
3. The regression intercept was [set to the control condition][wo-intercept]
4. The regression is weighted by 1/sd^2

*Data not represented*
This figure does not include male mice or BALB/c females from [Storer 1979][storer 1979].  Its not clear if these are excluded from the profile likelihood generation as well, or not.

In any case they fit the inclusion criteria, and should have been addressed.

[overlay]: http://dl.dropbox.com/u/1131693/bloodrop/10b3_temp_png.png
[wo-intercept]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot_2014-05-21_16_45_53_png_-_Dropbox.png

^ back to [table of contents](#contents)

________________________________________________________________

<a name="10B4"></a>

Reproduce BEIR 10B4
========================================================
*Last update: June 2014*

Reproduce the BEIR estimates on the oak ridge lifespan data
from storer 1979 (3575012.pdf) and combine these with the
oak ridge tumor estimates developed in the
[10B2 reproduction](#10B2) to reproduce 10B4.

Verify that confidence intervals are the same as those seen
in table 10-2.


```r
# TODO(ben) make sure the reproduction is perfect, instead of almost perfect
# TODO(ben) check if they included BALB/c females or RFM mice

# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- read.csv('data/storer_1979_processed.csv')
data_10B2 <- read.csv('data/10B2_fits.csv')

# Model
model_10B4 <- function(data, o){
    glm(
        I(1/age) ~ I(dose + o*dose^2 / (fractions)),
        data=data,
        weights=1/sd^2
    )
}

beir_r <- get_likelihoods(data %>% filter(modeled_in_10B3), model_10B4)

# Merge likelihoods with data from 10B2
data <- merge(
  beir_r %>% mutate(o=round(o, 2)),
  data_10B2 %>% mutate(o=round(o, 2)))

# Clean data and calcualte the mean likelihood
data = data %>%
  mutate(tumor = likelihood,
         lifespan = l,
         mean = (tumor + lifespan)/2) %>%
  select(-l, -likelihood)

# save for later
write.csv0(data, file='data/10B4.csv')
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
g <- melt(data,
          id.vars='o',
          value.name='likelihood',
          variable.name='source')
ggplot(g, aes(o, likelihood, linetype=source)) +
  geom_path() +
  scale_linetype_manual(values=c('dotted', 'dotdash', 'solid')) +
  scale_y_continuous(breaks = c(0:5)/5, limits=c(0,1.2))
```

<img src="Figs/unnamed-chunk-25.png" title="plot of chunk unnamed-chunk-25" alt="plot of chunk unnamed-chunk-25" width="983.04" />

##### Table 10-2
This is the original [10-2 table][table-10-2-citation] from the beir VII report [beir-10b4].

![table-10-2-image]

[table-10-2-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=250
[table-10-2-image]: http://dl.dropbox.com/u/1131693/bloodrop/table%2010-2.png

##### Reproduce Table 10-2 row 1
Reproduce confidence intervals from my re-production, to prove that
I am faithfully determining confidence intervals and to butress visual proof with numeric evidence.

Note, my confidence interfal should match the first row and last column, the 95% confidence interval from radiobiology data.


```r
# is        "0.4 (0.1, 3.5)"
# should be "0.5 (0.1, 3.2)"
confidence_interval(data$o, data$mean, 0.05)
```

```
## [1] "0.42 (0.06, 3.54)"
```

```
## [1] 0.06 0.42 3.54
```


#### Results

I was able to reproduce figure 10B4 and the confidence intervals very well.  The slight disparity that is observed could be due to outstanding methodological issues or small differences in the data.  For example, perhaps the original researchers applied rounding that I did not, or perhaps my estiamtes of the points on their graphs were different.

In any case, the estimate has changed a trivial amount from the original.  Making it perfect is a job for later.

^ back to [table of contents](#contents)

___________________________________________________________________

<a name="10-2"></a>

Reproduce BEIR 10-2 (fail)
========================================================
*Last update: May 2014*

Reproduce the BEIR estimates of dose reponse for atomic bomb survivors.


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- read.csv('data/lss14/lss14_spruced_up.csv')

# Constants
graphable_threshold = 2.0
modelable_threshold = 1.5

# Filter the data
data <- data %>%
  select(city, sex, gd3, ahs, ctime, solid,
         subjects, agex, age, dose, pyr, death) %>%
  filter(dose <= graphable_threshold)

# Model
model <- function(new_terms) {
  formula <- solid ~ city +
                     sex*I(log(age + 1)) +
                     sex*I(log(agex + 1)) +
                     offset(log(pyr)) -
                     1

  formula <- update(formula, new_terms)

  glm(formula,
      family='poisson',
      data = data)
}

factor_fit <- model(~ . + factor(dose))

# Data points to predict
to_predict <- expand.grid(
  city = unique(data$city),
  sex = unique(data$sex),
  agex = 30,
  age = 60,
  pyr = 1,
  dose = unique(data$dose))

# Make predictions
err <- function(model, data) {
  baseline <- data
  baseline$dose <- min(data$dose)

  risk <- exp(predict(model, newdata=data))
  control_risk <- exp(predict(model, newdata=baseline))
  err <- risk / control_risk - 1

  err
}
to_predict$err <- err(factor_fit, to_predict)

# Average by Dose and city
aggregate <- to_predict %>%
  group_by(dose) %>%
  summarize(err=mean(err))
```

#### Show
##### 10-2
This is the original [10-2 figure][10-2-citation] from the beir VII report [beir-10-4].

![10-2-image]:

[10-2-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=248
[10-2-image]: http://dl.dropbox.com/u/1131693/bloodrop/10-2.jpg


##### Reproduce 10-2
This is my attempt to reproduce 10-2 to prove that I am faithfully applying their methodology.

```r
# Show data
lm_smooth <- function(...) geom_smooth(method='lm',
                                       se=FALSE,
                                       color='black',
                                       fullrange=TRUE,
                                       ...)

ggplot(data=aggregate %.% filter(dose <= modelable_threshold),
       aes(dose, err)) +
  lm_smooth(formula='y ~ x - 1') +
  lm_smooth(formula='y ~ I(x + 0.3*x^2) - 1') +
  lm_smooth(formula='y ~ I(x + 0.7*x^2) - 1') +
  geom_point(data=aggregate, aes(dose, err))
```

<img src="Figs/unnamed-chunk-28.png" title="plot of chunk unnamed-chunk-28" alt="plot of chunk unnamed-chunk-28" width="983.04" />


#### Results
I was unable to reproduce figure 10-2 using original lss data.  As you can see, the data points found in the original are not the same that I find.

There are several causes for these discrepiancies:

1. **BEIR VII does not specificy exactly what data was used** for this figure.  From the following quote it might be inferred that they used some work-in-progress dataset that is not publically available.
> "Because the most recent cancer incidence data were not yet available outside of RERF, analyses of these data were conducted under the direction of the committee by RERF investigators who served as agents of the Academy."
> ANNEX 12B - pg 296
> http://www.nap.edu/openbook.php?record_id=11340&page=296

2. **BEIR VII does not articulate how data was clustered** they used some grouping of doses that is different than the one avaiable from the lss downloads.  This would probably be a problem that we could solve by hand if we knew what data was being used.

3. **BEIR VII does not specify exactly what models were used** for this fit.  They reference a variety of models in the annexes of chapter 12 and we could probably determine which was used here if we could solve the first two problems.

#### What to do?
This reproduction is necessary to form a final estimate of DDREF using the same methodology as BEIR VII and therefore to know how much updates to the animal data affected the final result.  It would be re-assuring to reproduce this data from source values, but fine if we can simply get the results as a profile likelihood.  Therefore I will:

*Try to reproduce figure 10-3 (the profile likelihood curves that will actually be used) using data reproduced from these graphs rather than original profile likelihood data.*

^ back to [table of contents](#contents)

___________________________________________________________________

<a name="10-3-lss"></a>

Reproduce BEIR 10-3 lss curve (from a reproduction of 10-2)
========================================================
*Last update: May 2014*

Reproduce the BEIR VII estimates of DDREF likelihood using atomic bomb survivor data.

*Note this data is based on a reproduction of figure 10-2 instead of on original atomic bomb survivor data for the reasons listed in my [attempt to reproduce 10-2](#10-2)


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- read.csv('data/10-2.csv')  # a reproduction of BEIR 10-2

# Show 10-2
# Prove that we can do the linear and quadratic fits for 10-2
fit <- function(...) geom_smooth(
  ...,
  method='lm',
  se=FALSE,
  color='black',
  fullrange=TRUE)

ggplot(data %>% filter(dose < 1.5), aes(dose, err)) +
  fit(formula='y ~ x - 1') +
  fit(formula='y ~ I(x + 0.3 * x^2) - 1',
               linetype='dotted') +
  fit(formula='y ~ I(x + 0.7 * x^2) - 1',
               linetype='dotdash') +
  scale_y_continuous(breaks = c(0:3)/2) +
  scale_x_continuous(breaks = c(0:4)/2) +
  geom_pointrange(data=data,
                  aes(ymax=err + 2*sd, ymin=err - 2*sd)) +
  coord_cartesian(xlim=c(-0.1,2.1), ylim=c(-0.1,1.6))
```

<img src="Figs/unnamed-chunk-29.png" title="plot of chunk unnamed-chunk-29" alt="plot of chunk unnamed-chunk-29" width="983.04" />

#### Reproduction of Figure 10-2
We managed a nearly perfect reproduction of 10-2 from the reconstituted data which shows that our formulas are correct.

*Methodological error*
It is strange that the authors chose to determine DDREF on linear quadratic fits to excess relative risk, rather than fitting seperate models on the poisson regressions that were used to excess relative risk in the first place.  I will try to correct that oddity in my subsequent work.

*Trivial statistical error*
Erroneously, these fits are from unweighted regression where each point counts equally.  They should have been performed such that data points with more people in them (lower doses) recieve a higher weight.  Fortunately this error is not repeated for the profile likelihood analysis and so it does not affect the overall analysis, only this graph.


```r
# Data
data_to_model <- data %>%
  # < 1.5 Sv of exposure
  filter(dose < 1.5) %>%
  # Prevent sd from ever equalling 0 (to allow 1/sd to work)
  mutate(sd = sd + 0.00001)

# Model
modeler <- function(data, o) glm(err ~ I(dose + o*dose^2) - 1,
                                 data=data,
                                 weights=1/sd^2)
likelihoods <- get_likelihoods(data_to_model, modeler)

# save for later
setwd('~/janus')
write.csv0(likelihoods, file='data/10-3-lss.csv')
```


```r
# Show
ggplot(likelihoods, aes(o, l)) +
  geom_path(linetype='dotdash') +
  scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5), limits=c(0,1.5))
```

<img src="Figs/unnamed-chunk-31.png" title="plot of chunk unnamed-chunk-31" alt="plot of chunk unnamed-chunk-31" width="983.04" />

#### BEIR 10-3 reproduction of lss analysis
We are able to form a 'reasonable' reproduction of the atomic bomb survivor curve from 10-3.  It's peak does not rise quite as high as the original firgure.  This may be because of errors when re-constructing the data used in this figure, or because of unknown differences in methodology.  In any case, the difference is small and difficult to resolve because the original data and full methodology is un-available.


![10-3-image]:

##### BEIR 10-3
This is the original [10-3 figure][10-3-citation] from the beir VII report [beir-10-3].

[10-3-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=249
[10-3-image]: http://dl.dropbox.com/u/1131693/bloodrop/10-3.jpg

##### Table 10-2
This is the original [10-2 table][table-10-2-citation] from the beir VII report [beir-10b4].

![table-10-2-image]

[table-10-2-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=250
[table-10-2-image]: http://dl.dropbox.com/u/1131693/bloodrop/table%2010-2.png

##### Reproduce Table 10-2 row 2
Reproduce confidence intervals from my lss profile likelihood curve reproductions, to provide quantitiave evidence that I am faithfully applying the same methodology.


```r
# is        "0.3 (-0.2, 1.7)"
# should be "0.3 (-0.1, 1.5)"
confidence_interval(likelihoods$o, likelihoods$l, 0.05)
```

```
## [1] "0.31 (-0.19, 1.66)"
```

```
## [1] -0.19  0.31  1.66
```

These values are very close to the original.  Good reason to move forward.

^ back to [table of contents](#contents)

___________________________________________________________________

<a name="10-3"></a>

Reproduce BEIR 10-3 from reproductions of BEIR 10B4 and 10-2
========================================================
*Last update: June 2014*

Reproduce the BEIR VII estimates of DDREF likelihood that combines data from atomic bomb survivors and animal data.


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data_lss <- read.csv('data/10-3-lss.csv')
data_animal <- read.csv('data/10B4.csv')

# Rename likelihood columns
# so we can distiguish between sources
data_lss <- data_lss %>%
  mutate(LSS_likelihood = l) %>%
  select(o, LSS_likelihood)

data_animal <- data_animal %>%
  mutate(Radiobiological_prior = mean) %>%
  select(o, Radiobiological_prior)

# Merge likelihoods from 10-3 and 10B4
data <- merge(
  data_lss %>% mutate(o=round(o, 2)),
  data_animal %>% mutate(o=round(o, 2)))

# Determine profile resolutions
# (this is needed to normalize the profile likelihood curves for
#  graphing)
delta = data_lss$o[2] - data_lss$o[1]

# Find the posterior distribution
# (by baysian update)
data <- data %>%
  mutate(Combined_posterior = normalize_likelihood(
           log(Radiobiological_prior) + log(LSS_likelihood),
           delta)
         )
```


```r
g <- melt(data,
          id.vars='o',
          value.name='likelihood',
          variable.name='source')
ggplot(g, aes(o, likelihood, linetype=source)) +
  geom_path() +
  scale_linetype_manual(values=c('dotted', 'dotdash', 'solid')) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5), limits=c(0,1.5))
```

<img src="Figs/unnamed-chunk-34.png" title="plot of chunk unnamed-chunk-34" alt="plot of chunk unnamed-chunk-34" width="983.04" />

#### BEIR 10-3 reproduction
Pretty good reproduction.  The lss curve is slightly lower than would be expected, this issue is [discussed in the section where that reproduction was made](#10-3-lss).

![10-3-image]:

##### BEIR 10-3
This is the original [10-3 figure][10-3-citation] from the beir VII report [beir-10-3].

[10-3-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=249
[10-3-image]: http://dl.dropbox.com/u/1131693/bloodrop/10-3.jpg


##### Table 10-2
This is the original [10-2 table][table-10-2-citation] from the beir VII report [beir-10b4].

![table-10-2-image]

[table-10-2-citation]: http://www.nap.edu/openbook.php?record_id=11340&page=250
[table-10-2-image]: http://dl.dropbox.com/u/1131693/bloodrop/table%2010-2.png

##### Reproduce Table 10-2
Reproduce confidence intervals from my profile likelihood curve reproductions, to provide quantitiave evidence that I am faithfully applying (nearly) the same methodology.


```r
o = data$o

# should be "0.5 ( 0.1, 3.2)"
# is        "0.4 ( 0.1, 3.5)"
confidence_interval(o, data[['Radiobiological_prior']], 0.05)
```

```
## [1] "0.42 (0.06, 3.54)"
```

```
## [1] 0.06 0.42 3.54
```

```r
# should be "0.3 (-0.1, 1.5)"
# is        "0.3 (-0.2, 1.7)"
confidence_interval(o, data[['LSS_likelihood']], 0.05)
```

```
## [1] "0.31 (-0.19, 1.66)"
```

```
## [1] -0.19  0.31  1.66
```

```r
# should be "0.5 ( 0.1, 1.2)"
# is        "0.4 ( 0.1, 1.2)"
confidence_interval(o, data[['Combined_posterior']], 0.05)
```

```
## [1] "0.38 (0.08, 1.22)"
```

```
## [1] 0.08 0.38 1.22
```

Nearly identical, only some very small (trivial?) differences.  Time to move forward.

^ back to [table of contents](#contents)

___________________________________________________________________

<a name="10B3-all-data"></a>

1/lifespan on all data
========================================================
*Last update: July 2014*


Show graphs like Storer 1979 10-B3 but for all data.


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- readRDS('data/ddref.rds')

# Helpers
model_10B3 <- function(data){
    glm(
        I(1/age - 1/control_age) ~ dose + I(dose^2 / (fractions)) - 1,
        data=data,
        weights=1/sd^2
    )
}

# Prediction Intervals
to_predict <- expand.grid(
    fractions = c(1, 1000),
    dose = seq(0, 1.5, 0.1)
)
to_predict$type <- 'A'
to_predict$type[to_predict$fractions > 1] <- 'C'

# Mean Lifespans
aggregate = data %>%
  group_by(cluster, group_id) %>%
  summarize(age=mean(lifespan),
            n=length(lifespan),
            sd=sd(lifespan)/n^0.5,
            dose=only(dose),
            fractions=only(fractions),
            type=only(type))

# Determine control ages
# This is so we can fix the curve to always fit the control
# as BEIR VII did
control_ages <- aggregate %>%
  group_by(cluster) %>%
  summarize(control_age=mean(age[dose == min(dose)]))
aggregate <- merge(aggregate, control_ages)

# Save aggregated data for later use
write.csv0(aggregate, file='data/10B4_all_data_aggregated.csv')

# Model
predictions <- ddply(aggregate, .(cluster), function(df){
  m <- model_10B3(df)
  to_predict$p <- predict(m, newdata=to_predict)
  to_predict$cluster <- only(df$cluster)
  to_predict
})
predictions <- merge(predictions, control_ages)

# Show
# As in 10B3
# http://www.nap.edu/openbook.php?record_id=11340&page=257
ggplot(aggregate, aes(dose,
                      1/age,
                      label=type,
                      group=type)) +
  geom_smooth(
      data=predictions,
      aes(dose, p + 1/control_age, linetype=type),
      method='lm',
      formula='y ~ x + I(x^2)',
      se=FALSE,
      color='black'
  ) +
  geom_text(size=4) +
  facet_wrap(~ cluster, scales='free_y')
```

<img src="Figs/unnamed-chunk-36.png" title="plot of chunk unnamed-chunk-36" alt="plot of chunk unnamed-chunk-36" width="983.04" />

#### Results

The fits are from well behaved!

Chronic effects may appear better or worse than projected acute effects.  Sometimes hormesis like respsonses appear.  Its not obvious from this graph because the y-axis is stretched to fit the data, but the magnitude of the effect is changing wildly too.

It is no wonder that radiobiology is full of debate!

At this point we should be a bit skeptical of organizing the data in this, the BEIR VII manner.  While that approach seemed reasonable given the ORNL data that they worked with, it clearly does not generalize well.  This may be because the underlying statitical approach is flawed, or simply that these graphs a very robust way of displaying the effect.  In any case we question the 'intuitive appeal' of graph 10B3.  While it seemed quite difinitive in isolation, the effect is lost when we try to repeat it on new datasets.

However, we see some glimmers of hope.  While we can't trust these fits outright, in those cases where both chronic and acute exposures are available, the chronic exposure is regularly less damaging, albeit with some wild variation in magnitude.  This suggests that we would do well to be more inclusive in our data aquisition, to include higher doses in our analysis.

^ back to [table of contents](#contents)

________________________________________________________________________

<a name="10B4-all-data"></a>

1/lifespan profiles on all data
========================================================
*Last update: June 2013*


Show graphs like Storer 1979 10B4 but for all data.


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
aggregate <- read.csv('data/10B4_all_data_aggregated.csv')

# Helpers
model_10B4 <- function(data, o){
  glm(
    I(1/age - 1/control_age) ~ I(dose + o * dose^2 / (fractions)) - 1,
    data=data,
    weights=1/sd^2
  )
}

# Model
likelihoods <- ddply(aggregate, .(cluster), function(df){
  l <- get_likelihoods(df, model_10B4)
  l$cluster <- only(df$cluster)
  l
})

# Summarize Effect
delta = likelihoods$o[2] - likelihoods$o[1]
summary_effect <- likelihoods %>%
  group_by(o) %>%
  summarize(log_l = sum(log(l))) %>%
  mutate(l = normalize_likelihood(log_l, delta),
         cluster = "summary effect") %>%
  select(-log_l)
likelihoods <- rbind(likelihoods, summary_effect)

# Show
# As in 10B4
# http://www.nap.edu/openbook.php?record_id=11340&page=257
ggplot(likelihoods, aes(o, l)) +
    geom_path() +
    coord_cartesian(ylim=c(0,1)) +
    facet_wrap(~ cluster)
```

<img src="Figs/unnamed-chunk-37.png" title="plot of chunk unnamed-chunk-37" alt="plot of chunk unnamed-chunk-37" width="983.04" />

#### Results
Looks bad, clearly, we are over-confident!

^ back to [table of contents](#contents)

________________________________________________________________________

<a name="10B3-meta"></a>

Meta Regression of BEIR 10B3
========================================================
*Last update: June 2013*

BEIR fits oak ridge data as if they are points, but actually each point represents many samples and we know the standard deviation of this estimate.  Therefore [metaregression](#metaregression) is a more appropriate form of analysis.  Here we show the fit by meta-regression.

We will also run a heterogeneity test.


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Save for later use
data <- read.csv('data/storer_1979_processed.csv')

# Helpers
model_10B3 <- function(data){
  glm(
    I(1/age - 1/control_age) ~ dose + I(dose^2 / (fractions)) - 1,
    data=data,
    weights=1/sd^2
  )
}
model_meta <- function(data){
  rma(
    yi,
    vi,
    mods = ~ dose + I(dose^2 / (fractions)) - 1,
    data = data,
    method='ML'
  )
}
predict_meta <- function(m, newdata){
    newdata$a <- newdata$dose
    newdata$B <- with(newdata, dose^2 / (fractions))
    predict(m, newmods=with(newdata, cbind(a, B)))$pred
}

# Prepare for Meta
data$yi <- with(data, 1/age - 1/control_age)
data$vi <- with(data, (1/age - 1/(age + sd))^2)

# Fake data to predict across
# This allows us to fit two lines across a limited
# data range for graphing
to_predict <- expand.grid(
    fractions = c(1, Inf),
    dose = seq(0, 1.5, 0.1)
)
to_predict$type <- 'A'
to_predict$type[to_predict$fractions > 1] <- 'C'


m <- model_10B3(data %>% filter(modeled_in_10B3))
to_predict$p <- predict(m, newdata=to_predict) + 1/636.175

m <- model_meta(data %>% filter(modeled_in_10B3))
to_predict$p_meta <- predict_meta(m, newdata=to_predict) + 1/636.175
data$tau2 <- m$tau2
p_heterogeneity <- m$QEp

g <- data[with(data,
    strain == 'RFM' &
    sex == 'F' &
    rate != 0.4
),]
ggplot(g, aes(dose, 1/age)) +
  geom_text(size=5, aes(label=type, group=type)) +
  geom_path(data=to_predict, aes(dose, p, group=type)) +
  geom_path(data=to_predict, aes(dose, p_meta, group=type), color='red') +
  scale_y_continuous(breaks = 0.0002*0:4+0.0016) + 
  geom_errorbar(aes(ymin=1/age + (vi + tau2)^0.5,
                    ymax=1/age - (vi + tau2)^0.5), 
                alpha=0.5, width=.05, color='red') +
  geom_errorbar(aes(ymin=1/age + (vi)^0.5,
                    ymax=1/age - (vi)^0.5), 
                alpha=0.5, width=.15) + 
  annotate("text", x=1, y=0.0024,
            label=paste("p heterogeneity < ",
                        format(p_heterogeneity, scientific=TRUE, digits=1)))
```

<img src="Figs/unnamed-chunk-38.png" title="plot of chunk unnamed-chunk-38" alt="plot of chunk unnamed-chunk-38" width="983.04" />

#### Results

The a/B ratio goes down a bit.  Standard error bars are much larger than they were originally.

Heterogeneity is highly significant as measured by restricted maximum likelihood.  More on that measurement from the metafor paper (http://www.jstatsoft.org/v36/i03/paper) and they cite

Q-test
Hedges LV, Olkin I (1985). Statistical Methods for Meta-Analysis. Academic Press, San Diego, CA.

REML
Viechtbauer W (2005). Bias and Eciency of Meta-Analytic Variance

Estimators in the Random-Eects Model." Journal of Educational  and Behavioral Statistics, 30(3), 261-293.


^ back to [table of contents](#contents)

________________________________________________________________________

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
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- read.csv('data/storer_1979.csv', sep='\t')

# Helpers
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

<img src="Figs/unnamed-chunk-39.png" title="plot of chunk unnamed-chunk-39" alt="plot of chunk unnamed-chunk-39" width="983.04" />

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


^ back to [table of contents](#contents)

________________________________________________________________________

<a name="10B3-meta-all"></a>

Meta-regression on all data
========================================================
*Last update: June 2013*


Show graphs like Storer 1979 10B3 but for all data using random effects meta-regression.


```r
# TODO(ben) Why is this section failing?

# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- readRDS('data/ddref.rds')
```

#### Modeling functions
Specify modeling functions.

`model_10B3` will fit a linear quadratic model exactly as in the BEIR VII report, without accounting for within or between group error.


```r
model_10B3 <- function(data){
    glm(
        I(1/age) ~ dose*cluster + I(dose^2/fractions)*cluster,
        data=data,
        weights=n
    )
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
    data = head(data, 6),
    method="ML"
  )
}


# # Mean Lifespans
# aggregate <- ddply(data, .(cluster, group_id, sex), function(df){
#     u <- function(x) paste(unique(x), collapse=' ')
#     dont_aggregate <- c('lifespan', 'id', 'X', 'n')
#     n <- nrow(df)
#     data.frame(
#         llply(df[,!names(df) %in% dont_aggregate], u),
#         age=mean(df$lifespan),
#         sd=sd(df$lifespan)/n^0.5,
#         n=nrow(df)
#     )
# })
#
# # Prepare for Meta
# aggregate$yi <- with(aggregate, 1/age)
# aggregate$vi <- with(aggregate, (1/age - 1/(age + sd))^2)
#
# # Restore Sanity
# numerics <- names(data)[laply(data, is.numeric)]
# for(n in numerics) {
#     if(n %in% names(aggregate)){
#         aggregate[,n] <- as.numeric(as.character(aggregate[,n]))
#     }
# }
#
# # Model
# m <- model_meta(aggregate)
# c <- coefficients(m)
# aggregate <- aggregate %>%
#   mutate(i=c[paste0('cluster', cluster)],
#          a=c[paste0('a:cluster', cluster)],
#          B=c[paste0('cluster', cluster, ':B')],
#          tau2=m$tau2)
# aggregate$p_my_analysis <- predict(m)$pred
# aggregate$p_10B3 <- predict(model_10B3(aggregate))
#
# # Project
# # Add projections across the entire range (0-1.5 Gy) for
# # acute and protracted exposures.  This will create nicer
# # graphs.
# doses = seq(from=0, to=1.5, by=.1)
# coefficients = unique(aggregate[,c('i', 'a', 'B', 'cluster')])
# projections = ddply(coefficients, .(cluster), function(df){
#   acute <- with(df, cbind(data.frame(
#     dose = doses,
#     type = 'A',
#     fractions = 1,
#     p_my_analysis = i + a * doses + B * doses^2
#   ), df))
#
#   chronic <- with(df, cbind(data.frame(
#     dose = doses,
#     type = 'C',
#     fractions = 1000,
#     p_my_analysis = i + a * doses
#   ), df))
#
#   rbind(acute, chronic)
# })
# projections$p_10B3 <- predict(model_10B3(aggregate),
#                               newdata=projections)
#
# # Show
# g <- aggregate
# f <- function(x) round(x, 2)
# g$cluster <- with(g, paste0(
#     cluster, '\n',
#     #'a=', f(a),
#     #' B=', f(B),
#     ' ddref =', f((a + B) / a)
# ))
# projections$cluster <- projections$cluster
# projections$cluster <- with(projections, paste0(
#     cluster, '\n',
#     #'a=', f(a),
#     #' B=', f(B),
#     ' ddref =', f((a + B) / a)
# ))
#
# ggplot(g, aes(
#     dose,
#     1/age,
#     label=type,
#     group=type)) +
#     geom_errorbar(aes(
#         ymin=1/age + (vi + tau2)^0.5,
#         ymax=1/age - (vi + tau2)^0.5
#         ), alpha=0.5, width=.05, color='red') +
#     geom_errorbar(aes(
#         ymin=1/age + vi^0.5,
#         ymax=1/age - vi^0.5
#     ), alpha=0.5, width=.1) +
#     geom_text(size=4) +
#     geom_smooth(
#         data = projections,
#         aes(dose, p_10B3),
#         method='lm',
#         formula='y ~ x + I(x^2)',
#         se=FALSE,
#         color='black'
#     ) +
#     geom_smooth(
#         data = projections,
#         aes(dose, p_my_analysis),
#         method='lm',
#         formula='y ~ x + I(x^2)',
#         se=FALSE,
#         color='red'
#     ) +
#     facet_wrap(~ cluster, scales="free_y")
#
#
# ggsave_for_ppt('meta_regression.png')
```

#### Results

Curves don't change that radically, though standard errors often change rather dramatically!

^ back to [table of contents](#contents)

________________________________________________________________________

<a name="10B4-meta-all"></a>

Meta Regression profiles on all data
========================================================
*Last update: June 2013*


Show graphs like Storer 1979 10B4 but for all data using the random effects meta regression.


```r
# TODO(ben) why isn't this working?

# # Common
# setwd('~/janus')
# source('scripts/util/util.R') # http://goo.gl/VYzkAs
#
# # Data
# data <- readRDS('data/ddref.rds')
#
# # Helpers
# model_10B4 <- function(data, o){
#     glm(
#         I(1/age) ~ cluster * I(dose + o*dose^2 / (fractions)),
#         data=data,
#         weights=n
#     )
# }
# model_meta <- function(data, o){
#     data$curved_dose <- with(data, dose + o*dose^2 / (fractions))
#     rma(
#         yi,
#         vi,
#         mods = cbind(cluster, curved_dose),
#         data = data,
#         method="ML"
#     )
# }
#
#
# # Define Acute
# chronic <- data$fractions > 1
# data$type <- 'A'
# data$type[chronic] <- 'C'
#
# # Mean Lifespans
# aggregate <- ddply(data, .(cluster, group_id), function(df){
#     u <- function(x) paste(unique(x), collapse=' ')
#     dont_aggregate <- c('lifespan', 'id', 'X', 'n')
#     n <- nrow(df)
#     data.frame(
#         llply(df[,!names(df) %in% dont_aggregate], u),
#         age=mean(df$lifespan),
#         sd=sd(df$lifespan)/n^0.5,
#         n=nrow(df)
#     )
# })
#
# # Prepare for Meta
# aggregate$yi <- with(aggregate, 1/age)
# aggregate$vi <- with(aggregate, (1/age - 1/(age + sd))^2)
#
# # Restore Sanity
# numerics <- names(data)[laply(data, is.numeric)]
# for(n in numerics) {
#     if(n %in% names(aggregate)){
#         aggregate[,n] <- as.numeric(as.character(aggregate[,n]))
#     }
# }
#
# # Model
# low = -2
# high = 6
# delta = .1   # Reduce to increase resolution
# o_range = (low/delta):(high/delta) * delta
#
# # Model
# summary <- ldply(o_range, function(o){
#       # BEIR
#       m <- model_10B4(aggregate, o)
#       l_10B4 <- logLik(m)
#
#       # Random Effects
#       m <- model_meta(aggregate, o)
#       l_meta <- logLik(m)
#
#       c(l_10B4=l_10B4, l_meta=l_meta, o=o)
# })
# summary <- summary %>%
#   mutate(l_10B4 = normalize_likelihood(l_10B4, delta),
#          l_meta = normalize_likelihood(l_meta, delta))
#
# # Normalize
# aggregate <- ddply(aggregate, .(cluster, sex), function(df){
#     df$l_10B4 <- normalize_likelihood(df$l_10B4, delta)
#     df$l_meta <- normalize_likelihood(df$l_meta, delta)
#     df
# })
#
#
# # Show
# # As in 10B4
# # http://www.nap.edu/openbook.php?record_id=11340&page=257
# a <- aggregate
#
# show <- function(g){
#     g$cluster <- as.factor(as.character(g$cluster))
#     g$l_10B4 <- pmin(g$l_10B4, 1)
#     g$l_meta <- pmin(g$l_meta, 1)
#     suppressWarnings(print(
#     ggplot(g, aes(o, l)) +
#         geom_path(aes(o, l_10B4), color='black') +
#         geom_path(aes(o, l_meta), color='red') +
#         facet_wrap(~ cluster) + scale_y_log10()
#     ))
# }
#
# g <- a
# show(g)
#
#
# ggsave_for_ppt('meta_regression_profile.png')
#
# summary$l_10B4 <- pmin(4, summary$l_10B4)
# summary$l_meta <- pmin(4, summary$l_meta)
# ggplot(summary, aes(o, l)) +
#     geom_path(aes(o, l_meta))
#     ylim(0,4)
# ggsave_for_ppt('meta_regression_summary_effect.png')
#
#
# # TODO: Maybe I would not get absurd profile-likelihood values if I just perfomed a single regression, instead of multiple regressions with bayesian updates.  Concretely I would do 1/age ~ cluster * ...
```

#### Results

We still seem to have biased likelihood estimates, but things are improving a bit...

^ back to [table of contents](#contents)

__________________________________________________________________

<a name="log-or-not"></a>

To log or not to log?
==========================================================
It is strange that dose response in cell systems is linear quadratic for chromosomal abberations and log linear quadratic for cell survival.  Concretely:

`chromosomal abberations ~ a * Dose + B * Dose^2`

TODO: add figure pic from thesis

whereas

`log(survival) ~ a * Dose + B * Dose^2`

TODO: add a pic of cell survival

I propose that the dose response is actually always log linear quadratic, but that for rare outcomes the distinction does not matter and a regular linear quadratic response will fit the data quite well.

To prove that this could be true I will develop a little fake survival data set and show that when viability is high, either formula fits the data, whereas when viability is low only the log linear quadratic formula fits the data.



TODO: ask Gayle or Little about this
TODO: look in Hall and BEIR, why do they say that ERR has inward curvature?


```r
# Fake data
n <- 200
a <- 0.001
B <- 0.0001
data <- data.frame(dose=0:n) %>%
  mutate(viability = exp(-a*dose - B*dose^2))

# Wrapper function for plotting
show <- function(data) {
  ggplot(data, aes(dose, viability)) +
    geom_point(size=0.5) +
    geom_smooth(method='lm',
                formula='y ~ x + I(x^2)',
                se=FALSE)
}


# Log scale
show(data) + scale_y_log10()
```

<img src="Figs/unnamed-chunk-441.png" title="plot of chunk unnamed-chunk-44" alt="plot of chunk unnamed-chunk-44" width="983.04" />

```r
# Normal scale
show(data)
```

<img src="Figs/unnamed-chunk-442.png" title="plot of chunk unnamed-chunk-44" alt="plot of chunk unnamed-chunk-44" width="983.04" />

```r
# Normal scale where viability is high
show(data %>% filter(viability > 0.70))
```

<img src="Figs/unnamed-chunk-443.png" title="plot of chunk unnamed-chunk-44" alt="plot of chunk unnamed-chunk-44" width="983.04" />

**Figures: Linear quadratic model fits**
In each figure  dose (x axis) is plotted against viability (y axis).
Black dots represent hypothetical data points fit to a log linear quadratic model without error.  The blue line represents a linear quadratic fit to the data.  In the first figure, the data set is plotted with viability on a log scale.  The second figure is identical, but viability is plotted on a linear scale.  The third figure is identical to the second, on a linear scale, but the data is truncated to only include outcomes with high viability.

**Results**
This hypothetical data proves the point, log linear quadratic models and linear quadratic models are nearly indistiguishiable when viablity is high, but only the log linear quadratic model can fit the data well when viablity extends into the low range.

The implication of this finding is that log linear quadratic models is always valid and is necessary if the outcome of interest affects a large percentage of the population (the cutoff is arbitrary, but the author might suggest > 30%).  This further suggests that we need not create a threshold of 1.5 Gy or 2 Gy when assessing DDREF, instead we must fit results to log linear quadratic scales and avoid a cutoff entirely.

**How could the literature be wrong**
If we read up in Hall, we find that the proposed cutoff is controversial, Gray proposed the bell shaped dose response in the 1960s to explain leukemia induction in mice which seemed to diminish at high doses.  He proposed that this is because the cells that were damaged and would have lead to cancer had become inviable instead.  Therefore some optimal dose results in maximum cancer induction by damaging many cells without inducing so much damage that they become inviable.

However, subsequent data has not born out this theory in other systems like breast cancer (Brehner and Sachs) in children as St. Jude's treated for leukemia (Neglia 2006).

![][http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-05-07%2012.21.33.png]

**Figures from Hall**
These figures, copied from Hall's textbook illustrate Gray's proposal and counter evidence from Brehner and Sachs.

**Conclusion**
We would do well to use log linear quadratic models and avoid the tacit assumption that there is some threshold dose beyond which cancer rates (or mortality rates) fall.

TODO: Try estimating DDREF without a cutoff
TODO: See if the hypotherical threshold proposed by BEIR VII actually plays out ([figure 10-1][BEIR 10-1])


[BEIR 10-1]: http://www.nap.edu/openbook.php?record_id=11340&page=247#p2000e6f39960247001


^ back to [table of contents](#contents)

__________________________________________________________________



<a name="concordance"></a>

Concordance
========================================================
*Last update: April 2013*


Give a detailed description of the dataset for those that want to make a close inspection.



```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
setwd('~/janus')
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

find_in_file <- function(pattern, file='~/janus/scripts/exp/radiation.R'){
  	lines <- readLines(file)
		lines[grepl(pattern, lines)]
}
```

#### Group details
Show the details of each treatment group in the dataset organized by cluster


```r
for(id in sort(unique(d$cluster_id))) {
  df <- d[d$cluster_id == id,]
  cluster <- as.character(df$cluster[1])
  cat('\n',
      cluster,
      '\n------------------------------------------------\n')
  s <- ddply(df, .(group_id), function(df) {
    group_summary(df)
  })
  s <- s %>%
    mutate(study=sub('^[0-9]*-', '', group_id),
           group=as.numeric(sub('^[0-9]*-', '', study)),
           study=as.numeric(sub('-[0-9]*$', '', study))) %>%
    arrange(study, group) %>%
    select(-study, -group)
  print(s)
}
```

```
## 
##  1 - ♀ RFM/Un Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##    group_id  ♂    ♀  avg. age dose rate # fractions warnings
## 1  1007-3-9   - 2696      632  0.1  0.4           1        1
## 2 1007-3-10   -  930      614 0.25  0.4           1        1
## 3 1007-3-11   - 1064      553  0.5  0.4           1        1
## 4 1007-3-12   -  237      541 0.75  0.4           1        1
## 5 1007-3-13   - 1045      538    1  0.4           1        1
## 6 1007-3-14   - 1005      487  1.5  0.4           1        1
## 
##  2 - ♀ B6CF1 Mice ANL 
##  γ-ray at 114 days old 
## ------------------------------------------------
##     group_id  ♂    ♀  avg. age dose  rate # fractions warnings
## 1  1003-20-2   -  857      945    -     -           1         
## 2  1003-20-4   -  397      903 0.86  0.04           1         
## 3  1003-21-2   -  185      932    -     -           1         
## 4  1003-21-4   -  200      936 0.86  0.04           1         
## 5  1003-22-2   -  464      970    -     -           1         
## 6  1003-24-2   -  175      991    -     -           1         
## 7  1003-25-2   -   50      960    -     -           1         
## 8  1003-26-2   - 1138      978    -     -           1         
## 9  1003-26-3   -  497      963 0.22  0.01           1         
## 10 1003-26-4   -  346      968 0.43  0.02           1         
## 11 1003-26-5   -  194      935 0.86  0.04           1         
## 12 1003-29-2   -  584      986    -     -           1         
## 13 1003-29-4   -  598      957    1 8e-04          60         
## 14 1003-30-2   -  399      977    -     -           1         
## 
##  3 - ♂ B6CF1 Mice ANL 
##  γ-ray at 113 days old 
## ------------------------------------------------
##     group_id  ♂   ♀  avg. age dose  rate # fractions warnings
## 1  1003-20-1 843   -      952    -     -           1         
## 2  1003-20-3 386   -      922 0.86  0.04           1         
## 3  1003-21-1 200   -      985    -     -           1         
## 4  1003-21-3 199   -      970 0.86  0.04           1         
## 5  1003-21-5 160   -      939  1.4  0.07           1         
## 6  1003-22-1 557   -      985    -     -           1         
## 7  1003-24-1 310   -      987    -     -           1        6
## 8  1003-25-1  60   -     1011    -     -           1         
## 9  1003-26-1 200   -     1043    -     -           1         
## 10 1003-28-1 120   -     1020    -     -           1         
## 11 1003-29-1 592   -      993    -     -           1         
## 12 1003-29-3 594   -      971    1 8e-04          60         
## 13 1003-30-1 393   -     1007    -     -           1         
## 
##  4 - ♂ C57BL/Cnb Mice SCK/CEN 
##  γ-ray at 84 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1    9-6-1 467   -      613    -    -           1         
## 2    9-6-2 241   -      581 0.25  0.3           1         
## 3    9-6-3 236   -      564  0.5  0.3           1         
## 4    9-6-4 241   -      550    1  0.3           1         
## 5    9-6-8 107   -      605 0.25  0.3          10         
## 6    9-6-9 109   -      604  0.5  0.3          10         
## 7   9-6-10 115   -      615    1  0.3          10         
## 8   9-6-14 104   -      622    1  0.3           8         
## 
##  5 - ♂ RFM/Un Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1 1007-3-1 430   -      711    -    -           1        1
## 2 1007-3-2 256   -      720  0.1  0.4           1        1
## 3 1007-3-3  94   -      711 0.25  0.4           1        1
## 4 1007-3-4 247   -      680  0.5  0.4           1        1
## 5 1007-3-5 230   -      673    1  0.4           1        1
## 6 1007-3-6 199   -      651  1.5  0.4           1        1
## 
##  6 - ♂ BALB/c/Cnb Mice SCK/CEN 
##  γ-ray at 84 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1    9-5-1 322   -      766    -    -           1         
## 2    9-5-2 191   -      745 0.25    4           1        3
## 3    9-5-3 194   -      736  0.5    4           1         
## 4    9-5-4 191   -      732    1    4           1         
## 5    9-5-8 111   -      778 0.25    4          10         
## 6    9-5-9 110   -      740  0.5    4          10         
## 7   9-5-10 113   -      751    1    4          10        3
## 
##  7 - ♀ BC3F1 Mice ENEA 
##  X-ray at 91 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1    3-1-1   - 353      889    -    -           1        5
## 2    3-1-2   - 100      912 0.04 0.06           1         
## 3    3-1-3   -  84      893 0.08 0.06           1         
## 4    3-1-4   -  53      854 0.16 0.06           1         
## 5    3-1-5   -  58      874 0.32 0.06           1         
## 6    3-1-6   -  57      833 0.64  0.6           1         
## 7    3-1-7   -  60      707  1.3  0.6           1         
## 8    3-1-9   - 279      865    -    -           1         
## 
##  8 - ♂ C57BL/6Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1 1007-2-2 502   -      906    -    -           1         
## 2 1007-2-4 254   -      909  0.5  0.4           1         
## 3 1007-2-6 260   -      922    1  0.4           1         
## 
##  9 - ♀ C3Hf/Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##    group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1  1007-2-9   - 501      778    -    -           1         
## 2 1007-2-11   - 249      727  0.5  0.4           1         
## 3 1007-2-13   - 250      693    1  0.4           1         
## 
##  10 - ♀ C57BL/6Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1 1007-2-1   - 491      858    -    -           1         
## 2 1007-2-3   - 253      855  0.5  0.4           1         
## 3 1007-2-5   - 251      865    1  0.4           1         
## 
##  11 - ♂ C3Hf/Bd Mice ORNL 
##  γ-ray at 70 days old 
## ------------------------------------------------
##    group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1 1007-2-10 502   -      732    -    -           1         
## 2 1007-2-12 244   -      713  0.5  0.4           1         
## 3 1007-2-14 248   -      721    1  0.4           1         
## 
##  12 - ♂ leucopus Peromyscus ANL 
##  γ-ray at 137 days old 
## ------------------------------------------------
##    group_id  ♂   ♀  avg. age   dose  rate # fractions warnings
## 1 1003-27-1 210   -     1388      -     -           1         
## 2 1003-27-2 203   -     1461      -     -           1         
## 3 1003-27-3 189   -     1358 0.0086 4e-04           1         
## 4 1003-27-4 181   -     1350  0.014 7e-04           1         
## 
##  13 - ♂ BC3F1 Mice ENEA 
##  X-ray at 92 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1   3-5-19 430   -      824    -    -           1        2
## 2   3-5-20  44   -      828  0.5  0.1           1         
## 3   3-5-21  48   -      797    1  0.1           1         
## 
##  14 - ♂ C57BL/Cnb Mice SCK/CEN 
##  X-ray at 7 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1    9-7-1 105   -      757    -    -           1        4
## 2   9-7-10  72   -      777  0.5    1           1        4
## 3   9-7-11  70   -      810    1    1           1        4
## 
##  15 - ♂ BC3F1 Mice ENEA 
##  X-ray at -4 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1    3-5-1  34   -      853    -    -           1         
## 2    3-5-3  48   -      799  0.3  0.1           1         
## 3    3-5-5  61   -      822  0.9  0.1           1         
## 4    3-5-7  46   -      897  1.5  0.1           1         
## 
##  16 - ♀ BC3F1 Mice ENEA 
##  X-ray at -4 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1    3-5-2   -  39      866    -    -           1         
## 2    3-5-4   -  40      883  0.3  0.1           1         
## 3    3-5-6   -  44      850  0.9  0.1           1         
## 4    3-5-8   -  50      872  1.5  0.1           1         
## 
##  17 - ♂ BC3F1 Mice ENEA 
##  X-ray at 580 days old 
## ------------------------------------------------
##   group_id  ♂   ♀  avg. age dose rate # fractions warnings
## 1   3-5-35  41   -      886    -    -           1         
## 2   3-5-36  42   -      901  0.5  0.1           1         
## 3   3-5-37  43   -      874    1  0.1           1
```

#### Warnings
Some issues were found when digging through the input data that were not judged to be severe enough to exclude the data, but do exemplify deviations from our expectations.  These are listed as follows.


```r
# Warnings are listed in radiation.R on lines that start with
# the following prefix
prefix <- "# warning-"

# Get a list of all the warnings relevant to this dataset
warnings <- gsub('[^0-9]', '', unique(d$warning_reason))
warnings <- sort(as.numeric(warnings[warnings != ""]))

# Find the corresponding warning definition in radiation.R
warning_prefixes <- paste0(prefix, warnings)
for(p in warning_prefixes) cat(find_in_file(p), '\n')
```

```
## # warning-1: Mean lifespan in the groups in this study varied from those reported in the formal literature (table 1 of Ullrich 1979 - jstor.org/stable/pdfplus/3575012.pdf).  Two groups varied substantially as noted in exclusion-7.  The others varied by 0-8 days, always less than one standard deviation of the mean.  I searched dillegently for cause of death annotations which should be excluded to correct the figures, but I could not find any.  The problem seems to originate with the data source. 
## # warning-2: Group 3-5-19 has 430 animals with a mean lifespan of 824 days in the data but should only have 203 animals with a mean lifespan of 827 days according to 3576356.pdf table II.  The difference in mean lifespan is small and this is the only group with such a disparity, which makes me think its probably worth including regardless. 
## # warning-3: 9-5-2 and 9-5-10 the mean lifespans in the data (738 and 739) are not the same at those in 3575970.pdf table 1 (743 and 747).  These are the only two discrepiances and are both less than 10 days.  Probably ok. 
## # warning-4: mean lifespans in 9-7 are consistently off by 2 (both up and down) from those in 3579307.pdf table 1.  The disparity is small, much less than the standard deviation, but still a bit worrisome. 
## # warning-5: 3-1-1 has a lifespan two days higher in the data (889) than in 3577210.pdf table 1 (887).  This is the only problem in this dataset which makes me think that the data is correct and the original table is wrong. 
## # warning-6: 1003-24-1 Has a mean lifespan of 878 days in the data and 887 days in table 11 of anl-95-3.  The counts and standard errors are the same and the total size of the disprepiancy is small ~10 days.  I am guessing that the table contains a typo
```

#### Study Details
Here are a description of each of the original studies used in this analysis as provided by the 'Gray Book' ([Gerber et al. 1996](#gerber_1996)).  Studies can easily be found by lab and study id which are the first two parts of the group id.  Concretely, a group id of 1007-3-6 is the sixth groups from the third study conducted at lab 1007, Oak Ridge National Laboratory.

TODO(later) summarize this information in a table
TODO(later) this information should be stored as data, not text

##### 3-1
cluster 7 - ♀ BC3F1 Mice ENEA X-ray at 91 days old

![][3-1]
More detail in ([Covelli 1988][Covelli 1988])

[3-1]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-24%2020.59.22.png
[Covelli 1988]: http://dl.dropbox.com/u/1131693/bloodrop/3577210.pdf


##### 3-5
cluster 13 - ♂ BC3F1 Mice ENEA X-ray at 92 days old
cluster 15 - ♂ BC3F1 Mice ENEA X-ray at -4 days old
cluster 16 - ♀ BC3F1 Mice ENEA X-ray at -4 days old
cluster 17 - ♂ BC3F1 Mice ENEA X-ray at 580 days old

![][3-5]
More detail in ([Covelli 1984][Covelli 1984])

[3-5]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.25.17.png
[Covelli 1984]: http://dl.dropbox.com/u/1131693/bloodrop/3576356.pdf


##### 9-5
cluster 6 - ♂ BALB/c/Cnb Mice SCK/CEN γ-ray at 84 days old

![][9-5]
More detail in ([Maisin 1983][Maisin 1983])

[9-5]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.28.32.png
[Maisin 1983]: http://dl.dropbox.com/u/1131693/bloodrop/3575970.pdf


##### 9-6
cluster 4 - ♂ C57BL/Cnb Mice SCK/CEN γ-ray at 84 days old

![][9-6]
![][9-6-2]
More detail in ([Maisin 1988][Maisin 1988])

[9-6]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.29.07.png
[9-6-2]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.29.42.png
[Maisin 1988]: http://dl.dropbox.com/u/1131693/bloodrop/3577205.pdf


##### 9-7
cluster 14 - ♂ C57BL/Cnb Mice SCK/CEN X-ray at 7 days old

![][9-7]
More detail in ([Maisin 1988][Maisin 1988])

[9-7]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.30.19.png
[Maisin 1988]: Reference


##### 1003-20
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-20]
More detail in ([Grahn 1995][Grahn 1995])

[1003-20]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-22%2016.36.24.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-21
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-21]
More detail in ([Grahn 1995][Grahn 1995])

[1003-21]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.20.41.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-22 cluster  (only controls)
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-22]
More detail in ([Grahn 1995][Grahn 1995])

[1003-22]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.21.26.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-24 cluster  (only controls)
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-24]
More detail in ([Grahn 1995][Grahn 1995])

[1003-24]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.22.00.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-25 cluster  (only controls)
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-25]
More detail in ([Grahn 1995][Grahn 1995])

[1003-25]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.22.34.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-26 cluster
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-26]
More detail in ([Grahn 1995][Grahn 1995])

[1003-26]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.23.02.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-27 cluster
cluster 12 - ♂ leucopus Peromyscus ANL γ-ray at 137 days old

![][1003-27]
More detail in ([Grahn 1995][Grahn 1995])

[1003-27]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.26.08.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-28 cluster  (only controls)
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-28]
More detail in ([Grahn 1995][Grahn 1995])

[1003-28]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.23.34.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-29 cluster
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-29]
More detail in ([Grahn 1995][Grahn 1995])

[1003-29]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.23.59.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf


##### 1003-30 cluster  (only controls)
cluster 2 - ♀ B6CF1 Mice ANL γ-ray at 114 days old
cluster 3 - ♂ B6CF1 Mice ANL γ-ray at 113 days old

![][1003-30]
More detail in ([Grahn 1995][Grahn 1995])

[1003-30]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.24.25.png
[Grahn 1995]: https://dl.dropboxusercontent.com/u/1131693/bloodrop/anl-95-3.pdf



##### 1007-2
cluster 8 - ♂ C57BL/6Bd Mice ORNL γ-ray at 70 days old
cluster 9 - ♀ C3Hf/Bd Mice ORNL γ-ray at 70 days old
cluster 10 - ♀ C57BL/6Bd Mice ORNL γ-ray at 70 days old
cluster 11 - ♂ C3Hf/Bd Mice ORNL γ-ray at 70 days old

![][1007-2]
More detail in ([Storer 1988][Storer 1988])

[1007-2]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.31.08.png
[Storer 1988]: http://dl.dropbox.com/u/1131693/bloodrop/3577229.pdf


##### 1007-3
cluster 1 - ♀ RFM/Un Mice ORNL γ-ray at 70 days old
cluster 5 - ♂ RFM/Un Mice ORNL γ-ray at 70 days old

![][1007-3]
More detail in ([Ullrich 1979][Ullrich 1979])

[1007-3]: http://dl.dropbox.com/u/1131693/bloodrop/Screenshot%202014-04-23%2014.31.26.png
[Ullrich 1979]: http://dl.dropbox.com/u/1131693/bloodrop/3575012.pdf


##### References
<a name="gerber_1996"></a>
Gerber, Watson, Sugahara, Okada. International Radiobiology Archives of Long-Term Animal Studies I. Descriptions of Participating Institutions and Studies. 1996. [link](http://www.ustur.wsu.edu/nra/pdf/ira.pdf)

^ back to [table of contents](#contents)

____________________________________________________________________



<a name="10B4-with-only-acute-data"></a>

Supplemental: Reproduce lifespan analysis using only acute data
========================================================
*Last update: June 2014*

Individual level data from oak ridge is only available from acute exposures.  How does the profile likelihood change if only this data is used?

This is a simple adaptation of the techniques used to [reproduce 10B4](#10B4).


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Data
data <- read.csv('data/storer_1979_processed.csv')
data_10B2 <- read.csv('data/10B2_fits.csv')

# Model
model_10B4 <- function(data, o){
    glm(
        I(1/age) ~ I(dose + o*dose^2 / (fractions)),
        data=data,
        weights=n
    )
}

data <- get_likelihoods(
  data %>% filter(in_my_analysis & modeled_in_10B3),
  model_10B4)


# Merge likelihoods with data from 10B2
data <- merge(
  data %>% mutate(o=round(o, 2)),
  data_10B2 %>% mutate(o=round(o, 2)))

# Clean data and calcualte the mean likelihood
data = data %>%
  mutate(tumor = likelihood,
         lifespan = l,
         mean = (tumor + lifespan)/2) %>%
  select(-l, -likelihood)
```

#### Show

##### 10B4 with acute lifespan data
Here is 10B4 if only acute data is used.


```r
g <- melt(data,
          id.vars='o',
          value.name='likelihood',
          variable.name='source')
ggplot(g, aes(o, likelihood, linetype=source, color=source)) +
  geom_path() +
  scale_linetype_manual(values=c('dotted', 'dotdash', 'solid')) +
  scale_color_manual(values=c('black', 'red', 'black')) +
  scale_y_continuous(breaks = c(0:5)/5, limits=c(0,1.2))
```

<img src="Figs/unnamed-chunk-49.png" title="plot of chunk unnamed-chunk-49" alt="plot of chunk unnamed-chunk-49" width="983.04" />


##### Confidence intervals with acute only
Here's how the confidence intervals of the mean come out when only acute data is used.


```r
# was       " 0.4 ( 0.1, 3.5)"
# is        "-0.1 (-0.3, 1.1)"
confidence_interval(data$o, data$mean, 0.05)
```

```
## [1] "-0.12 (-0.28, 1.05)"
```

```
## [1] -0.28 -0.12  1.05
```


#### Results
If we only include acute results we see that the profile likelihood changes dramatically, giving a substantially lower estimate.  The mean confidence interval goes down as well.  The net effect is that we center around a confidence interval very close to 1.

Scary that this result is so sensitive to these two treatment groups!

^ back to [table of contents](#contents)

___________________________________________________________________



<a name="metaregression"></a>

Meta Regression Figure
========================================================
*Last update: July 2014*


A figure that shows off the principal of meta-regression.


```r
# Common
setwd('~/janus')
source('scripts/util/util.R') # http://goo.gl/VYzkAs

# Fake data
set.seed(1)
data <- data.frame(
    yi = rnorm(5, mean=1:5, sd=1),
    vi = rnorm(5)^2,
    x = 1:5
)

# Add an outlier
data$yi[3] <- 0
data$vi[3] <- .01

# Model
m <- rma(
    yi,
    vi,
    mods = cbind(x),
    data = data,
    method="ML"
)
data <- data %>%
  mutate(p = predict(m)$pred,
         tau2 = m[['tau2']],
         o2 = vi + tau2,
         e = 0.3*(p - yi))

# Predict likelihood
n <- nrow(data)
l <- with(data, sum(
    -(1/2) * sum(e^2 / o2) +
    -(1/2) * log(o2) +
    -(1/2) * log(2*pi)
))
logLik(m)
```

```
## 'log Lik.' -9.124 (df=3)
```

```r
l
```

```
## [1] -7.834
```

```r
# Show
ggplot(data, aes(x, yi)) +
    geom_point() +
    geom_errorbar(aes(
        ymin=yi - vi^0.5,
        ymax=yi + vi^0.5,
    ), width=0.1, alpha=0.5) +
    geom_errorbar(aes(
        ymin=yi - (vi + tau2)^0.5,
        ymax=yi + (vi + tau2)^0.5,
    ), width=0.1, alpha=0.5, color='red') +
    geom_path(aes(x, p), color='black')
```

<img src="Figs/unnamed-chunk-51.png" title="plot of chunk unnamed-chunk-51" alt="plot of chunk unnamed-chunk-51" width="983.04" />

## Figure: The principal of metaregression
Here we show an artificially generated dataset of 5 points with one outlier in the middle.  The standard error of each point is diplayed in grey error bars, the increased estimate, including random effects is displayed with red error bars.  The best fit after updating the error estimates via metaregression is shown with the line in black.

The essence here is that the initial error estamates are too low if a straight line explains the data.  Specifically the outlier is highly confident, but falls far off the best fit line.  This implies that the confidence is too high or that the relationship is not linear afterall.  With meta-regression we assume the relationship is in-fact linear but that the error of the data points has been systematicaly under-estimated.  The random effects error estimate is added to each point equally so that their distance from the best fit line is a probable number of stadard deviations.

^ back to [table of contents](#contents)

________________________________________________________________________