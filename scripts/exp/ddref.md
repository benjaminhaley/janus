DDREF
========================================================
Measure ddref, for my thesis.  
*last update: April 2014*

## Table of contents

- Background 
  - [Defining DDREF](#defining_ddref) - What is the equation?
  - [Cumulative ERR](#cumulative_err) - A new way of graphing risk.
  - [Log Likelihood](#loglike) - How is log likelihood calculated?
  - [Metaregression](#metaregression) - Show the principals of metaregression.
- Data
  - [Data Funnel](#data_funnel) - Which data will we analyze?
  - [Data Cleaning](#data_cleaning) - Damn, data, you look good!
  - [Concordance](#concordance) - Show what the data looks like.
- Analysis
  - [Reproduce BEIR 10B3](#10B3) - Show that we can fit the same model of
    lifespan vs. dose as oak ridge.
  - [Cherry Picking](#cherry) - I thought Oak Ridge was cheating, I was wrong.
    Sorry!
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


<a name="defining_ddref"></a>

BACKGROUND: Defining DDREF
========================================================
*April 2013*

DDREF is defined ambiguosly.  It can be derived from acute
exposures as 1 + Dβ/α  where response ~  D*α + D*β^2.  But for 
the purposes of radiation protection, we need a functional 
definition, that distinguishes a cutoff of dose and doserate
beyond which the DDREF correction ought to be used.  What are 
these cutoffs?  I will do a literature search to find out.

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

*References*

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

*Results*

1. Survival hazard was not used in BEIR VII, only mean lifespan
   this means I am doing something unique.
2. BEIR VII used 1.5 Sv as an upper threshold, because response
   falls off above this level, I should too.
3. When doses are fractionated apply the equation:
   a*D + B*(D^2/K), where K is the number of fractions
4. Dealing with doserate changes is hard, pg 246 suggests that
   repair processes take up to 24 hours, so this might be a
   natural break point.
5. Estimate DDREF at 1 Sv to make it compatible with lss estimates


<a name="cumulative_err"></a>

BACKGROUND: Cumulative ERR
========================================================
*May 2013*

Survival curves are a great way to look at time course data
but it is difficult to see the nature of the relationship between
a variable and multiple survival curves.

For example if mortality is increasing as the square of dose, this
is not visually obvious from a survival curve.

We could show relative mortality vs dose, but mortality estimates 
are quite variable for any given time point, and this variation 
might obscure the relationship.  Instead we might display
cumulative relative mortality vs dose.  Cumulative mortality
is better behaved than point estimates of mortality and so the
relative cumulative mortality should also be better behaved.  Excess
relative risk should also be similar for cumulative estimates vs
point estimates.

Here I want to try and draw these graphs I propose.


```r

# Libraries
library(plyr)
library(ggplot2)
library(reshape2)

# Constants
baseline_mortality <- function(time) 1e-06 * time^2
times <- 1:300
observations_per_group = 100
err <- function(dose) 1 + dose + dose^2
doses <- 0:10

# Data
groups <- rep(observations_per_group, length(doses))
names(groups) <- doses
survival <- data.frame(llply(groups))
names(survival) <- names(groups)
for (time in times) {
    for (d in (1:length(doses))) {
        dose <- doses[d]
        mortality <- baseline_mortality(time) * err(dose)
        groups[d] <- sum(runif(groups[d]) > mortality)
    }
    survival <- rbind(survival, groups)
}
survival$time <- c(0, times)

# Survival
survival <- melt(survival, id.vars = "time")
names(survival) <- c("time", "dose", "n")
survival$dose <- as.numeric(as.character(survival$dose))
survival$survival <- survival$n/observations_per_group
survival$cumulative_mortality <- -log(survival$survival)
survival$cumulative_err <- survival$cumulative_mortality/survival$cumulative_mortality[survival$dose == 
    0]
```


### Cumulative mortality


```r
# Graph
ggplot(survival, aes(time, survival, color = as.factor(dose))) + geom_path()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


### Cumulative Relative mortality vs dose


```r
g <- survival[abs(survival$cumulative_err) < Inf & !is.na(survival$cumulative_err), 
    ]
ggplot(g, aes(dose, cumulative_err, color = time)) + geom_point() + geom_smooth(method = "lm", 
    formula = y ~ poly(x, 2))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



### Results

Surival curves actually mask a quadratic relation.  Cumulative
excess relative risk makes it much more obvious (see graphs).
Notably, the actual curve in the excess relative risk model is
not a good approximation of the dose response curve, probably
because points with high error bars are weighted equally and
thereby bias the result.

### Conclusions

I recommend including generating the cumulative excess relative
risk graphs in my report.


<a name="data_funnel"></a>

DATA: Data Funnel
========================================================
*April 2013*

### Introduction:
Create a data set that might be used for DDREF analysis and
make a description of this data.


```r
focus_study = "3-6"

# LIBRARIES
library(plyr)
library(dplyr)
```

```
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
## arrange, desc, failwith, id, mutate, summarise, summarize
## 
## The following objects are masked from 'package:stats':
## 
## filter, lag
## 
## The following objects are masked from 'package:base':
## 
## intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(survival)
```

```
## Loading required package: splines
```

```r

# DATA
setwd("~/janus/scripts")
d <- readRDS("../data/external5.rds")

# HELPERS Make survival data amenable to ggplot
table0 <- function(...) table(..., useNA = "ifany")

# Report for funnel graph
count <- function(data) {
    count_unique <- function(x) length(unique(x))
    with(data, c(studies = count_unique(file), treatments = count_unique(group_id), 
        animals = count_unique(id), one_study = count_unique(id[study_id == 
            focus_study]), exclude = count_unique(id[exclude])))
}

# TODO(ben5) remove
duplicate_multiclusters <- function(data) {
    # Duplicate groups that belong to multiple clusters multiclusters are
    # space seperated and should be copied so that they belong to each cluster
    # in the list.
    multi <- function(df) {
        clusters <- as.character(df$cluster[1])
        clusters <- strsplit(clusters, " ")[[1]]
        if (length(clusters) < 2) {
            return(df)
        } else {
            result <- data.frame()
            for (c in clusters) {
                d <- df
                d$cluster <- c
                result <- rbind(result, d)
            }
            return(result)
        }
    }
    ddply(data, .(cluster), multi)
}
filter_by_n_groups <- function(data, threshold = 3) {
    ddply(data, .(cluster, sex), function(df) {
        n_groups = length(unique(df$group_id))
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
    "X-rays local", "gamma-rays local")

threshold_dose <- 1.5

# Fix

# NA doses d$dose[is.na(d$dose)] <- 0

# Add missing fractions
d$fractions[is.na(d$fractions)] <- 1
d$fractions[d$fractions == 0] <- 1

# Add fractions seperated by days
d$day_fractions <- d$fractions
s <- d$fraction_interval < 1 & !is.na(d$fraction_interval)
d$day_fractions[s] <- d$fractions[s] * d$fraction_interval[s]

# Duplicate those in multiple clusters
d <- duplicate_multiclusters(d)

d$rand = 1:nrow(d)

# Correct Assignment age
d <- d %.% group_by(cluster) %.% mutate(assignment_age = min(assignment_age, 
    lifespan, na.rm = T)) %.% ungroup()


# Age at last treatment
d$age_at_last_treatment <- d$age_at_treatment
s <- !is.na(d$fraction_interval)
d$age_at_last_treatment[s] <- with(d[s, ], age_at_treatment + fraction_interval * 
    (fractions - 1), )


# FILTER

# Initial counts
count(d)
```

```
##    studies treatments    animals  one_study    exclude 
##         34        909     127919       1562       8266
```

```r
# 34 studies, 909 treatments, 128K animals

# Dose not NA
d <- d[!is.na(d$dose), ]
count(d)
```

```
##    studies treatments    animals  one_study    exclude 
##         29        810     125304       1488       8266
```

```r
# 29 studies, 810 treatments, 125K animals

# Only low-LET, whole body
d <- d[!d$quality %in% bad_qualities, ]
count(d)
```

```
##    studies treatments    animals  one_study    exclude 
##         28        445      81883        699       8266
```

```r
# 28 studies, 445 treatments, 81K animals

# Dose too high (matching BEIR specs)
d <- d[!(d$dose > threshold_dose), ]
count(d)
```

```
##    studies treatments    animals  one_study    exclude 
##         23        215      45564        357       4659
```

```r
# 23 studies, 215 treatments, 45K animals

# Lifespan not NA
d <- d[!is.na(d$lifespan), ]
count(d)
```

```
##    studies treatments    animals  one_study    exclude 
##         23        215      45556        357       4659
```

```r
# 23 studies, 215 treatments, 45K animals

# No other treatments
d <- d[d$other_treatments == "none", ]
count(d)
```

```
##    studies treatments    animals  one_study    exclude 
##         23        172      43930        357       4609
```

```r
# 23 studies, 172 treatments, 44K animals

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
##    studies treatments    animals  one_study    exclude 
##         23        171      43850        357       4529 
## [1] "see exclusion-2 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##         23        171      43839        357       4518 
## [1] "see exclusion-3 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##         23        171      43769        357       4448 
## [1] "see exclusion-5 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##         23        170      43748        357       4427 
## [1] "see exclusion-6 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##         23        170      43747        357       4426 
## [1] "see exclusion-7 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##         23        169      40034        357        713 
## [1] "see exclusion-8 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##         22        166      39321        357          0
```

```r
# 22 studies, 166 treatments, 39K animals

# Remove cases with few treatment groups
d <- filter_by_n_groups(d)
count(d)
```

```
##    studies treatments    animals  one_study    exclude 
##         11        108      21902          0          0
```

```r
# 11 studies, 108 treatments, 22K animals

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
## [1] "see warning-2 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##         10         96      13469          0          0 
## [1] "see warning-3 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##          9         86      12911          0          0 
## [1] "see warning-4 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##          8         60      11289          0          0 
## [1] "see warning-5 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##          8         59      10859          0          0 
## [1] "see warning-6 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##          8         57      10551          0          0 
## [1] "see warning-7 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##          7         52      10162          0          0 
## [1] "see warning-8 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##          7         51       9809          0          0 
## [1] "see warning-9 in radiation.R"
##    studies treatments    animals  one_study    exclude 
##          6         47       8706          0          0
```

```r
d_wo_warnings <- filter_by_n_groups(d_wo_warnings)
count(d_wo_warnings)
```

```
##    studies treatments    animals  one_study    exclude 
##          5         43       8430          0          0
```

```r
# 5 studies, 43 treatments, 8.5K animals

# Save
setwd("~/janus/scripts")
saveRDS(d, "../data/funneled.rds")
```



<a name="cleaning"></a>

Clean and Summarize
========================================================
DDREF data has been filtered.  Now its time to prettify it.


```r
# Libraries
library(plyr)
library(ggplot2)
library(survival)

# Data
setwd("~/janus/scripts")
d <- readRDS("../data/funneled.rds")

# Helpers Print treatment data
get_treatment <- function(data) {
    with(data, paste(round(dose, 3), "Gy", "at", round(dose_rate, 3), "Gy/min", 
        "in", fractions, "fractions"))
}

# Print group summaries
summarize_numeric <- function(x) {
    n <- length(x)
    na_message <- ""
    if (any(is.na(x))) {
        na_message <- paste0(" missing ", sum(is.na(x)))
    }
    paste0(round(mean(x, na.rm = T), 1), " +/- ", round(sd(x, na.rm = T)/n^0.5, 
        1), na_message)
}
group_summary <- function(data) {
    .all <- function(x, c = " ") paste(unique(x), collapse = c)
    treatments <- get_treatment(data)
    with(data, c(strain = .all(strain), males = sum(sex == "Male"), females = sum(sex == 
        "Female"), mean_lifespan = summarize_numeric(lifespan), age_at_treatment = summarize_numeric(age_at_treatment), 
        treatments = .all(treatments, "\n"), other_treatments = .all(other_treatments), 
        warnings = .all(warning_reason)))
}

# Clean

# Simplify qualities qualities need nice easy names
d$quality[grepl("gamma", d$quality)] <- "gamma"
d$quality[grepl("controls", d$quality)] <- "control"
d$quality[grepl("X-rays", d$quality)] <- "x-ray"
d$source[is.na(d$source)] <- "control"

# Symbols for genders
d$sex_symbol <- "♂"
d$sex_symbol[d$sex == "Female"] <- "♀"

# Is acute?
d$acute <- d$fractions == 1 | d$dose == 0
d$protracted <- d$fractions > 1 & d$dose > 0

# Other Treatments
d$other_treatments[d$other_treatments == "none"] <- ""

# Institution
names <- data.frame(id = c("3", "9", "11", "1003", "1007"), name = c("ENEA", 
    "SCK/CEN", "RBI-TNO", "ANL", "ORNL"))
d$institution <- gsub("-[0-9]*$", "", d$study)
d$institution_name <- names$name[match(d$institution, names$id)]

# Cluster names
ignore <- c("control", 0)
d$rounded_age_at_t <- d$age_at_treatment
d$rounded_age_at_t[d$study == "1003-21"] <- 110
d$rounded_age_at_t[d$study == "1003-27"] <- 135
u <- function(x) paste(sort(unique(x[!x %in% ignore & !is.na(x)])), collapse = " or ")
d <- ddply(d, .(cluster, sex), function(df) {
    df$cluster_name <- with(df, paste(u(institution_name), u(cluster), "\n", 
        nrow(df), u(sex_symbol), u(strain), u(species), "\n", u(source), u(dose_rate), 
        "Gy/min", "\n", u(fractions), "fraction", "\n", "starting at", u(rounded_age_at_t), 
        "days old"))
    
    df
})

# Summaries
group_summaries <- ddply(d, .(group_id), function(df) {
    group_summary(df)
})
cluster_summaries <- ddply(d, .(cluster), function(df) {
    group_summary(df)
})
write.csv(group_summaries, file = "results/ddref_group_summaries.csv")
write.csv(cluster_summaries, file = "results/ddref_cluster_summaries.csv")

# Save Data for later use
write.csv(d, file = "data/ddref.csv")
```

        
### Results
Data is filtered and saved as ddref.csv for later use.  
See ddref_group_summaries.csv and ddref_cluster_summaries.csv
for some pretty results.  See also the funnel graph.


<a name="concordance"></a>

========================================================
Show Data
*June 2013*

Now that the data is reasonably clean, show what it looks
like.


```r
# Libraries
library(plyr)
library(ggplot2)
library(survival)

# Data
setwd("~/janus/scripts")
d <- read.csv("data/ddref.csv")

# Helpers
collapse_paste <- function(...) paste(..., collapse = " ")
paste_columns <- function(df) unlist(alply(df, 1, collapse_paste))
ggsave0 <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
    height = 7.68, units = "in"))
show <- function(g) {
    suppressWarnings(print(ggplot(g) + geom_hline(aes(yintercept = age_at_treatment)) + 
        geom_segment(aes(x = dose, xend = dose, y = age_at_treatment, yend = age_at_last_treatment)) + 
        geom_violin(scale = "count", aes(dose, lifespan, color = protracted, 
            group = factor(paste(dose, acute)))) + facet_wrap(~cluster_name) + 
        expand_limits(y = 0)))
}

# lifespan
g <- d[d$species == "Mouse" & d$sex == "Male", ]
show(g)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) 

```r
ggsave0("results/lifespan_by_cluster_male_mice.png")

g <- d[d$species == "Mouse" & d$sex == "Female", ]
show(g)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) 

```r
ggsave0("results/lifespan_by_cluster_female_mice.png")

g <- d[d$species == "Rat" & d$sex == "Female", ]
show(g)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-63.png) 

```r
ggsave0("results/lifespan_by_cluster_female_rat.png")

g <- d[d$species == "Peromyscus", ]
show(g)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-64.png) 

```r
ggsave0("results/lifespan_by_cluster_peromyscus.png")
```




<a name="10B3"></a>

Reproduce BEIR 10B3
========================================================
*June 2013*

Reproduce the BEIR estimates on the oak ridge lifespan data
from storer 1979 (3575012.pdf).


```r
    # Libraries
    library(ggplot2)
    library(plyr)
    
    # Data
    setwd('~/janus/scripts')
    data <- read.csv('data/storer_1979.csv', sep='\t')  
    
    # Helpers
    ggsave0 <- function(...) suppressWarnings(ggsave(..., 
                                    dpi=100, 
                                    width=10.24, 
                                    height=7.68, 
                                    units='in'))

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

    # Show
    # Reproduce 10B3
    # http://www.nap.edu/openbook.php?record_id=11340&page=257
    #ggsave0('beir_10B3_reproduction.png')
    g <- data[with(data, 
        strain == 'RFM' & 
        sex == 'F' &
        rate != 0.4
    ),]
    show(g)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using
## loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


### Results
I am capable of reproducing their results.  There are two tricks

  1. They stratified by strain and gender
  2. They weighted by n

Suspiciously they do not include data from table 2, but when I add this in, it does not make a huge difference, so I assume they are just not being careful.

I do however, think that they have cherry picked a well behaved
example from their dataset and it might be worth showing each
of the curves to show that the true result is not so easy to
discern. [update: this seems to be a false acusation, their results are wrong for several reasons, but they do not seem to be malicious]




<a name="cherry"></a>

Oak Ridge Cherry Picking
========================================================
The storer data suggests that oak ridge cherry picked the 
nicest example when showing off their data.  I can show that
the full story with all of the storer data is not so clean.


```r

# Libraries
library(ggplot2)
library(plyr)

# Data
setwd("~/janus/scripts")
data <- read.csv("data/storer_1979.csv", sep = "\t")

# Helpers
ggsave0 <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
    height = 7.68, units = "in"))

model_10B3 <- function(data) {
    glm(I(1/age) ~ dose + I(dose^2/(fractions)), data = data, weights = n)
}

# Constants
threshold = 1.5001

# Clean
data$fractions <- 1
data$fractions[data$rate < 0.1] <- Inf

# Define Acute
chronic <- data$rate < 0.1
data$type <- "A"
data$type[chronic] <- "C"

# Define strata
data$strata <- paste(data$sex, data$strain)

# Model
data <- ddply(data, .(strata), function(df) {
    s <- df$dose < threshold
    m <- model_10B3(df[s, ])
    df$p <- NA
    df$p[s] <- predict(m)
    
    df
})

# Show Show all data behind 10B3
# http://www.nap.edu/openbook.php?record_id=11340&page=257
ggplot(data, aes(dose, 1/age, group = type, label = type)) + geom_text(size = 4) + 
    geom_line(aes(dose, p)) + facet_wrap(~strata)
```

```
## Warning: Removed 3 rows containing missing values (geom_path). Warning:
## Removed 6 rows containing missing values (geom_path). Warning: Removed 1
## rows containing missing values (geom_path).
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
ggsave0("beir_10B3_cherry_picked.png")
```

    
#### Results
It does not look strongly cherry picked, I suppose I should retract that acusation.



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
## [1] 1.045
```

```r
l <- with(data, -(n/2) * log(2 * pi) + -(n/2) * log(o2) + -(1/(2 * o2)) * sum(e^2))
l - as.numeric(logLik(m))
```

```
## [1] 0
```






<a name="10B4"></a>

Reproduce BEIR 10B4
========================================================
*June 2013*
Reproduce the BEIR estimates on the oak ridge lifespan data
from storer 1979 (3575012.pdf).


```r

    # Libraries
    library(ggplot2)
    library(plyr)
    
    # Data
    setwd('~/janus/scripts')
    data <- read.csv('data/storer_1979.csv', sep='\t')  
    
    # Helpers
    ggsave0 <- function(...) suppressWarnings(ggsave(..., 
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

    # Reproduce 10B4
    # http://www.nap.edu/openbook.php?record_id=11340&page=258
    ggplot(beir_r, aes(o, l)) + 
        geom_path() + 
        #geom_path(data=my_r, color='red') + 
        scale_y_continuous(breaks = c(0:5)/5, limits=c(0,1))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
    ggsave0('beir_10B4_reproduction.png')  
```


#### Results

I was able to reproduce figure 10B4 near perfectly.  Importantly it becomes nearly straight if we only include the acute exposures.

It could be that the shoulder is very narrow, though this would be surprising given what we know about tissue level effects.

It could also be that there is some systematic bias between chronic and high dose experiments.  For instance the high dose rate was even higher than the experimenters thought that it was.

More reason for meta-analysis.



<a name="10B3-all-data"></a>

1/lifespan on all data
========================================================
*June 2013*
  
Show graphs like Storer 1979 10-3B but for all data.


```r

    # Libraries
    library(ggplot2)
    library(plyr)
    
    # Data
    setwd('~/janus/scripts')
    data <- read.csv('data/ddref.csv')  
    
    # Helpers
    ggsave0 <- function(...) suppressWarnings(ggsave(..., 
                                    dpi=100, 
                                    width=10.24, 
                                    height=7.68, 
                                    units='in'))

    model_10B3 <- function(data){
        glm(
            I(1/age) ~ dose + I(dose^2 / (fractions)),
            data=data,
            weights=n
        )
    }
    show <- function(g){
        f <- function(x) round(x, 2)
        g$cluster_name <- with(g, paste0(
            cluster_name, '\n',
            #'a=', f(a), 
            #' B=', f(B), 
            ' ddref =', f((a + B) / a)
        ))
        suppressWarnings(print(
        ggplot(g[is.na(g$p),], aes(
                dose, 
                1/age, 
                label=type,
                group=type
            )) + 
            geom_text(size=4) + 
            geom_smooth(
                data=g[!is.na(g$p),],
                aes(dose, p, color=type), 
                method='lm', 
                formula='y ~ x + I(x^2)',
                se=FALSE
            ) +
            facet_wrap(~ cluster_name)
        ))
    }
    
    # Define Acute
    chronic <- data$fractions > 1
    data$type <- 'A'
    data$type[chronic] <- 'C'
    
    # Prediction Intervals
    to_predict <- expand.grid(
        fractions = c(1, 1000),
        dose = seq(0, 1.5, 0.1)
    )
    to_predict$type <- 'A'
    to_predict$type[to_predict$fractions > 1] <- 'C'

    
    # Mean Lifespans
    aggregate <- ddply(data, .(cluster, group_id, sex), function(df){
        u <- function(x) paste(unique(x), collapse=' ')
        dont_aggregate <- c('lifespan', 'id', 'X', 'n')
        data.frame(
            llply(df[,!names(df) %in% dont_aggregate], u),
            age=mean(df$lifespan),
            age_sd=sd(df$lifespan),
            n=nrow(df)
        )   
    })
    
    # Restore Sanity
    numerics <- names(data)[laply(data, is.numeric)]
    for(n in numerics) {
        if(n %in% names(aggregate)){
            aggregate[,n] <- as.numeric(as.character(aggregate[,n]))
        }
    }
```

```
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
```

```r
    
    # Model
    m <- c('cluster', 'sex', 'cluster_name')
    df <- aggregate[
        aggregate$cluster == aggregate$cluster[1] &
        aggregate$sex == aggregate$sex[1] &
        aggregate$cluster_name == aggregate$cluster_name[1]
    ,]
    aggregate <- ddply(
        aggregate, 
        .(cluster, sex, cluster_name), 
        function(df){
            # Extract Coefficients
            m <- model_10B3(df)
            c <- m$coefficients
            df$a <- c['dose']
            df$B <- c['I(dose^2/(fractions))']
            
            # Extract Predictions
            predictions <- to_predict
            addin <- names(df)[!names(df) %in% names(predictions)]
            predictions <- merge(df[1, addin], predictions, all=TRUE)
            predictions$p <- predict(m, newdata=predictions)
            
            # merge
            df$p <- NA
            out <- rbind(df, predictions)
            
            out
        }
    )

    # Show
    # As in 10B3
    # http://www.nap.edu/openbook.php?record_id=11340&page=257
    a <- aggregate

    g <- a[a$sex == 'Male' & a$species == "Mouse",]
    show(g)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-111.png) 

```r
    ggsave0('inverse_lifespan_male_mice.png')

    g <- a[a$sex == 'Female' & a$species == "Mouse",]
    show(g)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-112.png) 

```r
    ggsave0('inverse_lifespan_female_mice.png')

    g <- a[a$sex == 'Female' & a$species == "Rat",]
    show(g)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-113.png) 

```r
    ggsave0('inverse_lifespan_female_rat.png')
    
    g <- a[a$species == 'Peromyscus',]
    show(g)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-114.png) 

```r
    ggsave0("inverse_lifespan_peromyscus.png")
```


#### Results

Turns out that DDREF is consistently present.  When a study
had both chronic and acute exposures the differences seemed
genuine.

The tricky part is the shape of the acute response, it can
be convoluted in many ways.  Also it can vary widely in terms
of dose response.  This suggests to me that the differences 
seen between workers and atomic bomb survivors are not the
product of a high estimate of DDREF, instead perhaps it is 
something else!?


<a name="10B4-all-data"></a>

1/lifespan profiles on all data
========================================================
*June 2013*

Show graphs like Storer 1979 10B4 but for all data.


```r

# Libraries
library(ggplot2)
library(plyr)

# Data
setwd("~/janus/scripts")
data <- read.csv("data/ddref.csv")

# Helpers
ggsave0 <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
    height = 7.68, units = "in"))

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


# Define Acute
chronic <- data$fractions > 1
data$type <- "A"
data$type[chronic] <- "C"

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
```

```
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
```

```r


# Model
low = -2
high = 6
delta = 0.5  # decrease for higher resolution
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
    g$cluster_name <- as.factor(as.character(g$cluster_name))
    g$l <- pmin(g$l, 1)
    suppressWarnings(print(ggplot(g, aes(o, l)) + geom_path() + ylim(0, 1) + 
        facet_wrap(~cluster_name)))
}
g <- a[a$sex == "Male" & a$species == "Mouse", ]
show(g)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-121.png) 

```r
ggsave0("inverse_lifespan_profile_male_mice.png")


g <- a[a$sex == "Female" & a$species == "Mouse", ]
show(g)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-122.png) 

```r
ggsave0("inverse_lifespan_profile_female_mice.png")


g <- a[a$sex == "Female" & a$species == "Rat", ]
show(g)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-123.png) 

```r
ggsave0("inverse_lifespan_profile_female_rat.png")


g <- a[a$species == "Peromyscus", ]
show(g)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-124.png) 

```r
ggsave0("inverse_lifespan_profile_peromyscus.png")


summary$l <- pmin(summary$l, 1)
ggplot(summary, aes(o, l)) + geom_path() + ylim(0, 1)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-125.png) 

```r
ggsave0("inverse_lifespan_summary_effect.png")
```

    
#### Results
Looks bad, we are way too confident!




<a name="metaregression"></a>

Meta Regression Figure
========================================================
*June 2013*

A figure that shows off the principal of meta-regression.


```r
# Libraries
library(ggplot2)
library(plyr)
library(metafor)
```

```
## Loading required package: Formula
## 
## Loading 'metafor' package (version 1.9-2). For an overview and
## introduction to the package please type: help(metafor).
```

```r

# Helpers
ggsave0 <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
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

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r
ggsave0("meta_regression_example.png")
```






<a name="10B3-meta"></a>

Meta Regression of BEIR 10B3
========================================================
*June 2013*

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
ggsave0 <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
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

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

```r
ggsave0("beir_10B3_meta_regression.png")
```


#### Results

The a/B ratio goes down a bit.  Standard error bars are much larger than they were originally.

Heterogeneity is highly significant as measured by restricted maximum likelihood.  More on that measurement from the metafor paper (http://www.jstatsoft.org/v36/i03/paper) and they cite

Q-test  
Hedges LV, Olkin I (1985). Statistical Methods for Meta-Analysis. Academic Press, San Diego, CA.

REML  
Viechtbauer W (2005). Bias and Eciency of Meta-Analytic Variance 

Estimators in the Random-Eects Model." Journal of Educational  and Behavioral Statistics, 30(3), 261-293.


<a name="10B4-meta"></a>

Meta Regression of BEIR 10B4
========================================================
*June 2013*

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
    ggsave0 <- function(...) suppressWarnings(ggsave(..., 
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
    delta = .1
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

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

```r
    ggsave0('beir_10B4_meta_reression.png')    
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





<a name="10B3-meta-all"></a>

Meta Regression on all data
========================================================
*June 2013*

Show graphs like Storer 1979 10B3 but for all data using 
the random effects meta-regression technique.


```r

    # Libraries
    library(ggplot2)
    library(plyr)
    library(metafor)
    
    # Data
    setwd('~/janus/scripts')
    data <- read.csv('data/ddref.csv')  
    
    # Helpers
    ggsave0 <- function(...) suppressWarnings(ggsave(..., 
                                    dpi=100, 
                                    width=10.24, 
                                    height=7.68, 
                                    units='in'))

    model_10B3 <- function(data){
        glm(
            I(1/age) ~ dose + I(dose^2 / (fractions)),
            data=data,
            weights=n
        )
    }   
    model_meta <- function(data){   
        data$a <- data$dose
        data$B <- with(data, dose^2 / (fractions))
        
        # We can only estimate random effects with 
        # four points or more.
        method="ML"
        if(nrow(data) == 3){
            method="FE"
        }
        rma(
            yi, 
            vi, 
            mods = cbind(a, B), 
            data = data,
            method=method
        )
    }
    
    predict_meta <- function(m, newdata){
        newdata$a <- newdata$dose
        newdata$B <- with(newdata, dose^2 / (fractions))
        predict(m, newmods=with(newdata, cbind(a, B)))$pred
    }       
    show <- function(g){
        f <- function(x) round(x, 2)
        g$cluster_name <- with(g, paste0(
            cluster_name, '\n',
            #'a=', f(a), 
            #' B=', f(B), 
            ' ddref =', f((a + B) / a)
        ))      
        g$cluster_name <- as.factor(as.character(g$cluster_name))

        original <- g[is.na(g$p_10B3),] 
        predictions <- g[!is.na(g$p_10B3),] 
        
        suppressWarnings(print(

        ggplot(original, aes(
            dose, 
            1/age, 
            label=type,
            group=type
        )) + 
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
                data = predictions,
                aes(dose, p_10B3), 
                method='lm', 
                formula='y ~ x + I(x^2)',
                se=FALSE,
                color='black'
            ) + 
            geom_smooth(
                data = predictions,
                aes(dose, p_my_analysis), 
                method='lm', 
                formula='y ~ x + I(x^2)',
                se=FALSE,
                color='red'
            ) + 
            facet_wrap(~ cluster_name)  
          ))
    }
            
    # Define Acute
    chronic <- data$fractions > 1
    data$type <- 'A'
    data$type[chronic] <- 'C'

    # Prediction Intervals
    to_predict <- expand.grid(
        fractions = c(1, 1000),
        dose = seq(0, 1.5, 0.1)
    )
    to_predict$type <- 'A'
    to_predict$type[to_predict$fractions > 1] <- 'C'

            
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
```

```
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
```

```r
    
    df <- aggregate
    m <- model_meta(df)
    df$yi
```

```
##   [1] 0.0010149 0.0010306 0.0010648 0.0007236 0.0006847 0.0007361 0.0007443
##   [8] 0.0011656 0.0011039 0.0011699 0.0011006 0.0011559 0.0010846 0.0013664
##  [15] 0.0013761 0.0014032 0.0014439 0.0013924 0.0012848 0.0014056 0.0016290
##  [22] 0.0018094 0.0018482 0.0018572 0.0020534 0.0013886 0.0014070 0.0014709
##  [29] 0.0014852 0.0015368 0.0015811 0.0013289 0.0012870 0.0014872 0.0015145
##  [36] 0.0013191 0.0016351 0.0015386 0.0016828 0.0012606 0.0018407 0.0010955
##  [43] 0.0011125 0.0011109 0.0011116 0.0010775 0.0011217 0.0010797 0.0011135
##  [50] 0.0011168 0.0010992 0.0011383 0.0010833 0.0011117 0.0011627 0.0011772
##  [57] 0.0010429 0.0011103 0.0016621 0.0010534 0.0011117 0.0010913 0.0010987
##  [64] 0.0012715 0.0011117 0.0010539 0.0011530 0.0011291 0.0011248 0.0010970
##  [71] 0.0011204 0.0012170 0.0012252 0.0012082 0.0012482 0.0011726 0.0011553
##  [78] 0.0012523 0.0011327 0.0012160 0.0011772 0.0011149 0.0011469 0.0012131
##  [85] 0.0012082 0.0012545 0.0011775 0.0011556 0.0011526 0.0013136 0.0013527
##  [92] 0.0013541 0.0013722 0.0014107 0.0012860 0.0013514 0.0016510 0.0016386
##  [99] 0.0016365 0.0017287 0.0017922 0.0018523 0.0016669 0.0016965 0.0013206
## [106] 0.0012873 0.0012349 0.0013206 0.0012103 0.0012997
```

```r
    predict(m)$pred
```

```
##   [1] 0.001212 0.001334 0.001319 0.001212 0.001212 0.001334 0.001319
##   [8] 0.001212 0.001212 0.001306 0.001306 0.001337 0.001337 0.001212
##  [15] 0.001306 0.001306 0.001337 0.001337 0.001212 0.001212 0.001267
##  [22] 0.001306 0.001329 0.001337 0.001305 0.001236 0.001267 0.001306
##  [29] 0.001337 0.001305 0.001236 0.001212 0.001267 0.001337 0.001212
##  [36] 0.001236 0.001276 0.001337 0.001212 0.001267 0.001337 0.001257
##  [43] 0.001231 0.001292 0.001311 0.001212 0.001218 0.001236 0.001267
##  [50] 0.001292 0.001308 0.001337 0.001212 0.001231 0.001292 0.001311
##  [57] 0.001212 0.001262 0.001449 0.001212 0.001212 0.001276 0.001287
##  [64] 0.001332 0.001212 0.001276 0.001332 0.001510 0.001212 0.001292
##  [71] 0.001332 0.001212 0.001212 0.001306 0.001337 0.001212 0.001212
##  [78] 0.001276 0.001276 0.001335 0.001335 0.001305 0.001305 0.001212
##  [85] 0.001306 0.001337 0.001212 0.001306 0.001337 0.001212 0.001449
##  [92] 0.001267 0.001306 0.001337 0.001274 0.001334 0.001212 0.001449
##  [99] 0.001446 0.001267 0.001306 0.001337 0.001274 0.001334 0.001212
## [106] 0.001306 0.001337 0.001212 0.001306 0.001337
```

```r

    
    # Model
    aggregate <- ddply(aggregate, .(cluster, sex), function(df){
        # Meta Model
        m <- model_meta(df)
        c <- coefficients(m)
        df$a <- c['a']
        df$B <- c['B']
        df$tau2 <- m$tau2

        # Setup Predictions
        predictions <- to_predict
        addin <- names(df)[!names(df) %in% names(predictions)]
        predictions <- merge(df[1, addin], predictions, all=TRUE)
        predictions$p_my_analysis <- 
            predict_meta(m, newdata=predictions)
                
        # BEIR Model    
        m <- model_10B3(df)
        predictions$p_10B3 <- predict(m, newdata=predictions)
        
        # merge
        df$p_10B3 <- NA
        df$p_my_analysis <- NA
        
        out <- rbind(df, predictions)
        
        out
    })

    # Show
    # As in 10B3
    # http://www.nap.edu/openbook.php?record_id=11340&page=257
    a <- aggregate

    g <- a[a$sex == 'Male' & a$species == "Mouse",]
    show(g)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-161.png) 

```r
    ggsave0('meta_regression_male_mice.png')

    g <- a[a$sex == 'Female' & a$species == "Mouse",]
    show(g)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-162.png) 

```r
    ggsave0('meta_regression_female_mice.png')

    g <- a[a$sex == 'Female' & a$species == "Rat",]
    show(g)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-163.png) 

```r
    ggsave0('meta_regression_female_rat.png')

    g <- a[a$species == "Peromyscus",]
    show(g)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-164.png) 

```r
    ggsave0('meta_regression_peromyscus.png')
 
```


#### Results

Curves don't change that radically, though standard errors often change rather dramatically!



<a name="10B4-meta-all"></a>

Meta Regression profiles on all data
========================================================
*June 2013*

Show graphs like Storer 1979 10B4 but for all data using the random effects meta regression.


```r


# Libraries
library(ggplot2)
library(plyr)
library(metafor)

# Data
setwd("~/janus/scripts")
data <- read.csv("data/ddref.csv")

# Helpers
ggsave0 <- function(...) suppressWarnings(ggsave(..., dpi = 100, width = 10.24, 
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
```

```
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion Warning: NAs introduced by coercion
```

```r

# Model
low = -2
high = 6
delta = 0.5  # Reduce to increase resolution
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
    g$cluster_name <- as.factor(as.character(g$cluster_name))
    g$l_10B4 <- pmin(g$l_10B4, 1)
    g$l_meta <- pmin(g$l_meta, 1)
    suppressWarnings(print(ggplot(g, aes(o, l)) + geom_path(aes(o, l_10B4), 
        color = "black") + geom_path(aes(o, l_meta), color = "red") + ylim(0, 
        1) + facet_wrap(~cluster_name)))
}

g <- a[a$sex == "Male" & a$species == "Mouse", ]
show(g)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-171.png) 

```r
ggsave0("meta_regression_profile_male_mice.png")

g <- a[a$sex == "Female" & a$species == "Mouse", ]
show(g)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-172.png) 

```r
ggsave0("meta_regression_profile_female_mice.png")

g <- a[a$sex == "Female" & a$species == "Rat", ]
show(g)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-173.png) 

```r
ggsave0("meta_regression_profile_female_rat.png")

g <- a[a$species == "Peromyscus", ]
show(g)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-174.png) 

```r
ggsave0("meta_regression_profile_peromyscus.png")

summary$l_10B4 <- pmin(4, summary$l_10B4)
summary$l_meta <- pmin(4, summary$l_meta)
ggplot(summary, aes(o, l)) + geom_path(aes(o, l_10B4), color = "black") + geom_path(aes(o, 
    l_meta), color = "red") + ylim(0, 4)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-175.png) 

```r
ggsave0("meta_regression_summary_effect.png")
```


#### Results

We still seem to have biased likelihood estimates, but things are improving a bit...


