#############################################################################
#
# Utility functions
#
# General functions and libraries used in thesis.Rmd.

# Libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(stats4)
library(bbmle)

# Retrieve the one unique value of x,
# raise an error if x has more than one value
only <- function(x) { 
  u <- unique(x)
  n <- length(u)
  if(n > 1) stop(paste("More than one value", paste(head(u), collapse=', ')))
  u
}
renumber <- function(data) {
  # Renumber clusters now that some are gone
  data %>%
    group_by(study) %>%
    mutate(cluster_number = reorder(cluster_number)) %>%
    ungroup()
}


# Re-number the clusters to account for any filtered out
# reorder(c(2, 2, 4, 4)) == c(1, 1, 2, 2)
reorder <- function(x) {
  unique <- sort(unique(x), decreasing=FALSE)
  original <- unique
  new_value <- rank(unique)
  for(i in 1:length(original)) {
    x[x == original[i]] <- new_value[i]
  }
  x
}


# Survival curves
show_survival <- function(data) {
  # Calculate survival
  data <- data %>%
    group_by(study, dose, group) %>%
    arrange(lifespan) %>%
    mutate(survival=rank(-lifespan) / length(lifespan))
  
  # Add an observation at 100% survival for each group
  g <- ddply(data, .(stratum, group), function(df) {
    top <- df[1,]
    top$survival = 1
    top$lifespan = only(df$censor)
    rbind(top, df)
  })
  
  # Reformat stratum names
  g <- g %>%
    mutate(stratum = paste0(stratum_id, ". ", stratum),
           stratum = sub("♂", "\n♂", stratum),
           stratum = sub("♀", "\n♀", stratum),
           stratum = sub(" @.*", "", stratum)
    )

  # Organize strata by number of animals
  order <- g %>% 
    group_by(stratum) %>%
    summarize(n=length(stratum)) %>%
    arrange(-n) %>%
    select(stratum)
  g$stratum <- factor(g$stratum, levels=order$stratum)
  
  ggplot(g) +
    geom_path(
      aes(lifespan,
          survival,
          color=dose,
          group=group,
          linetype=fractions > 1)) +
    facet_wrap(~ stratum) +
    scale_linetype(
      labels = c("acute", "protracted"),
      guide = guide_legend(title = "")
    ) +
    scale_color_continuous(
      labels = c("control", "1 Gy", "2 Gy", "3 Gy", "4 Gy"),
      breaks = c(0, 1, 2, 3, 4),
      guide = guide_legend(title = ""),
      trans = "sqrt"
    ) +
    geom_vline(
      aes(xintercept=censor),
      color="grey",
      size=1
    ) +
    scale_y_continuous(labels=percent) +
    expand_limits(x = -4) +
    theme(
      text = element_text(size = 8, color="black"),
      axis.text = element_text(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.background = element_blank(),
      legend.key = element_blank(),
      strip.background = element_blank()
    ) +
    ylab("survival\n") +
    xlab("\nage (days)") 
}

# The number of distinct values not equal to na
# n_unique_values(c(0, 0, NA)) == 1
n_unique_values <- function(x) length(unique(x)[!is.na(unique(x))])

# A nicer print function
print0 <- function(...) cat(paste(..., sep='', collapse = ''), "\n")


# Count the number of animals, groups, and studies in an analysis
count <- function(message, data) {
  data <- data %>% 
    filter(!duplicate)
  
  cat(message, "\n", 
      nrow(data), "animals", 
      length(unique(paste(data$study, data$stratum, data$group))), "groups",
      length(unique(paste(data$study))), "studies.",
      "\n\n"
  )
}

# The ways we want to retrieve the data
get_data <- function(dose_limit = 4, 
                     data=original_data, 
                     exclude=c(),
                     censor=0.0) {

  # Hack
  # Manually add stratum id's so that they stay uniform
  map <- data.frame(
    stratum = c(
      "9-6 SCK/CEN ♂ C57BL/Cnb mice\n  γ-ray @84 days",
      "1003-22 ANL ♂ B6CF1 mice\n  γ-ray @120 days",
      "9-5 SCK/CEN ♂ BALB/c/Cnb mice\n  γ-ray @84 days",
      "3-5 ENEA ♂ BC3F1 mice\n  X-ray",
      "1003-24 ANL ♂ B6CF1 mice\n  γ-ray",
      "9-4 SCK/CEN ♂ C57BL/Cnb mice\n  X-ray @28 days",
      "9-7 SCK/CEN ♂ C57BL/Cnb mice\n  X-ray",
      "11-2-E TNO ♀ BN/BRIJ rats\n  X-ray @56 days",
      "11-2-F TNO ♀ BN/BRIJ rats\n  X-ray @56 days",
      "11-2-F TNO ♀ WAG/RIJ rats\n  X-ray @56 days",
      "11-2-B TNO ♀ WAG/RIJ rats\n  γ-ray @56 days",
      "11-2-B TNO ♀ WAG/RIJ rats\n  X-ray @56 days",
      "1003-24 ANL ♀ B6CF1 mice\n  γ-ray",
      "11-2-E TNO ♀ WAG/RIJ rats\n  X-ray @56 days",
      "11-2-E TNO ♀ SD/RIJ rats\n  X-ray @56 days",
      "11-2-D TNO ♀ WAG/RIJ rats\n  γ-ray @117 days",
      "11-2-A TNO ♀ WAG/RIJ rats\n  X-ray @56 days",
      "11-2-D TNO ♀ WAG/RIJ rats\n  γ-ray @56 days"
    ),
    stratum_id = 1:18
  )
  data <- merge(data, map)
  
  # HACK
  # Duplicate controls
  # 12 should share 11's control
  # 18 should share 16's control
  data$duplicate <- FALSE
  twelve <- data %>% filter(stratum_id == 12)
  twelve_control <- data %>% 
    filter(dose == 0, stratum_id == 11) %>%
    mutate(stratum_id = only(twelve$stratum_id), 
           stratum = only(twelve$stratum),
           duplicate = TRUE
    )
  eightteen <- data %>% filter(stratum_id == 18)
  eightteen_control <- data %>% 
    filter(dose == 0, stratum_id == 16) %>%
    mutate(stratum_id = only(eightteen$stratum_id), 
           stratum = only(eightteen$stratum),
           duplicate = TRUE
    )
  data <- rbind(data, twelve_control, eightteen_control)
  
    
  count("Originally: ", data)
  
  data <- data %>%
    filter(dose <= dose_limit)
  
  count("After filtering for dose: ", data)

  # See how many animals have direct comparisons  
  data <- data %>% 
    filter(stratum %in% is_comparison(data, dose_limit))
  count("Directly compare acute and fractionated exposures or age at exposure.: ", data)
  
  data <- data %>%
    filter(!stratum %in% exclude)

  count("After filtering excluded strata: ", data)

  # Censor animals that died before the final treatment in thier stratum
  treatment_span <- data %>%
    group_by(stratum) %>%
    summarize(first = min(treatment_age, na.rm=TRUE),
              last = max(last_treatment_age, na.rm=TRUE))
  data <- merge(data, treatment_span, by="stratum")
  data <- data %>%
    filter(lifespan > last)
  
  count("After censoring animals that died before the final treatment in thier strata, ", data)

  # Censor animals that died within X percent of lifespan of the final exposure
  median_lifespan <- data %>%
    group_by(stratum) %>%
    summarize(median_lifespan = median(lifespan, na.rm=TRUE))
  data <- merge(data, median_lifespan, by="stratum")
  data <- data %>%
    mutate(censor = last + median_lifespan * censor) %>%
    filter(lifespan > censor)
  
  count(
    paste("After censoring animals that died within ", round(censor * 100), 
          "% of median lifespan after the final treatment ")
    , data
  )

  # Be sure the cluster numbers remain correct
  data <- renumber(data)
  
  # Organize strata by number of animals
  order <- data %>% 
    group_by(stratum) %>%
    summarize(n=length(dose)) %>%
    arrange(-n) %>%
    select(stratum)
  data$stratum <- factor(
    data$stratum, 
    levels=order$stratum
  )
  
  data
}

# How to aggregate
aggregate <- function(data) {
  # Mean Lifespans
  aggregate = data %>%
    ungroup() %>%
    group_by(study,
             stratum,
             stratum_id,
             cluster_number,
             cluster,
             group,
             sex, 
             dose, 
             rate, 
             fractions,
             interval) %>%
    summarize(
      days_at_risk = sum(lifespan),
      avg_treatment_age = mean((last_treatment_age + treatment_age)/2),
      treatment_age = mean(treatment_age),
      last_treatment_age = mean(last_treatment_age),
      n=length(lifespan),
      mortality=n / sum(lifespan),
      sem=sd(1/lifespan)/n^0.5,
      lifespan_sem=sd(lifespan)/n^0.5,
      lifespan=mean(lifespan)
    ) %>%
    mutate(
      protracted = fractions > 1
    )
  
  
  # Add clusters to facet labels
  aggregate <- ddply(aggregate, .(stratum), function(df) {
    number <- df$cluster_number[!is.na(df$cluster_number)]
    cluster <- df$cluster[!is.na(df$cluster)]
    clusters <- paste0(
      "     ",
      unique(number), ". ",
      unique(cluster),
      collapse="\n")
    df$stratum_full_name <- paste0(
      "  ", 
      df$stratum, 
      "\n",
      clusters
    )
    
    df
  })
  
  # Scale to max lifespans/mortality
  aggregate <- ddply(aggregate, .(stratum), function(df){
    controls = df$dose == 0
    longest_lifespan = max(df$lifespan)
    lowest_mortality = min(df$mortality)
    scale_lifespan <- function(x) x / longest_lifespan
    scale_mortality <- function(x) x / lowest_mortality
    df <- df %>%
      mutate(raw_sem = sem,
             raw_mortality = mortality,
             raw_lifespan = lifespan,
             raw_lifespan_sem = lifespan_sem,
             lifespan = scale_lifespan(lifespan),
             avg_treatment_age = scale_lifespan(avg_treatment_age),
             treatment_age = scale_lifespan(treatment_age),
             last_treatment_age = scale_lifespan(last_treatment_age),
             sem = scale_mortality(sem),
             mortality = scale_mortality(mortality)
      )
    
    df
  })

  # Unique id for each cluster
  aggregate <- aggregate %>%
    mutate(full_cluster_name = paste(stratum, cluster),
           cluster_id = as.factor(as.numeric(as.factor(full_cluster_name))))
  
  # Organize strata by number of animals
  order <- aggregate %>% 
    group_by(stratum_full_name) %>%
    summarize(n=sum(n)) %>%
    arrange(-n) %>%
    select(stratum_full_name)
  aggregate$stratum_full_name <- factor(
    aggregate$stratum_full_name, 
    levels=order$stratum_full_name
  )
  
  aggregate
}


# A list of clusters that recieved fractionated exposures
# Or aged between first and last exposures
fractionated_or_aged <- c(
  ""
  #  ,"1003-20 ANL ♀ B6CF1 mice\n  γ-ray @110 days"         
  #  ,"1003-20 ANL ♂ B6CF1 mice\n  γ-ray @110 days"         
  #  ,"1003-21 ANL ♂ B6CF1 mice\n  γ-ray @110 days"
  ,"1003-22 ANL ♂ B6CF1 mice\n  γ-ray @120 days"         
  ,"1003-24 ANL ♀ B6CF1 mice\n  γ-ray"                   
  ,"1003-24 ANL ♂ B6CF1 mice\n  γ-ray"                   
  #  ,"1003-26 ANL ♀ B6CF1 mice\n  γ-ray @120 days"         
  #  ,"1003-27 ANL ♂ Peromyscus leucopus\n  γ-ray @140 days"
  #  ,"1003-29 ANL ♀ mice\n  γ-ray @110 days"               
  #  ,"1003-29 ANL ♂ mice\n  γ-ray @110 days"               
  ,"11-2-A TNO ♀ WAG/RIJ rats\n  X-ray @56 days"         
  ,"11-2-B TNO ♀ WAG/RIJ rats\n  X-ray @56 days"         
  ,"11-2-B TNO ♀ WAG/RIJ rats\n  γ-ray @56 days"         
  ,"11-2-D TNO ♀ WAG/RIJ rats\n  γ-ray @117 days"        
  ,"11-2-D TNO ♀ WAG/RIJ rats\n  γ-ray @56 days"         
  ,"11-2-E TNO ♀ BN/BRIJ rats\n  X-ray @56 days"         
  ,"11-2-E TNO ♀ SD/RIJ rats\n  X-ray @56 days"          
  ,"11-2-E TNO ♀ WAG/RIJ rats\n  X-ray @56 days"
  ,"11-2-F TNO ♀ BN/BRIJ rats\n  X-ray @56 days"         
  ,"11-2-F TNO ♀ WAG/RIJ rats\n  X-ray @56 days"         
  #  ,"3-1 ENEA ♀ BC3F1 mice\n  X-ray @35 days"             
  ,"3-5 ENEA ♂ BC3F1 mice\n  X-ray"                      
  ,"9-4 SCK/CEN ♂ C57BL/Cnb mice\n  X-ray @28 days"      
  ,"9-5 SCK/CEN ♂ BALB/c/Cnb mice\n  γ-ray @84 days"     
  ,"9-6 SCK/CEN ♂ C57BL/Cnb mice\n  γ-ray @84 days"      
  ,"9-7 SCK/CEN ♂ TODO C57BL/Cnb mice\n  X-ray"
)

stratum_with_acute_poisoning <- c(
  "9-4 SCK/CEN ♂ C57BL/Cnb mice\n  X-ray @28 days",
  "9-7 SCK/CEN ♂ C57BL/Cnb mice\n  X-ray"
)

fractionation_comparisons <- function(data) {
  acute <- unique(data$stratum[data$fractions == 1])
  fractionated <- unique(data$stratum[data$fractions > 1])
  intersect(acute, fractionated)
}

age_comparisons <- function(data) {
  comparisons <- data %>%
    group_by(stratum, cluster, group) %>%
    summarize(treatment_age = mean(treatment_age, na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(stratum) %>%
    summarize(min = min(treatment_age, na.rm=TRUE),
              max = max(treatment_age, na.rm=TRUE)) %>%
    filter(max - min > 10)
  comparisons$stratum
}

is_comparison <- function(data, dose = 4) {
  comparisons <- unique(c(fractionation_comparisons(data), age_comparisons(data)))
  # Hack
  if(dose <= 3) {
    exclude <- c("1003-24 ANL ♂ B6CF1 mice\n  γ-ray",
                 "1003-24 ANL ♀ B6CF1 mice\n  γ-ray")
    comparisons <- comparisons[!comparisons %in% exclude]
  }
  comparisons
}

get_dose_response_by_cluster <- function(aggregated) {
  # Find dose response for each cluster
  model <- lm(mortality ~ cluster_id * dose -
                cluster_id -
                dose +
                stratum - 
                1,
              weights=1/sem^2,
              aggregated)
  
  # Extract coefficients and standard error for each cluster
  dose_response = data.frame(coefficients(summary(model)))
  dose_response <- dose_response %>%
    mutate(feature = rownames(dose_response)) %>%
    filter(grepl('cluster_id', feature)) %>%
    mutate(cluster_id = sub('cluster_id', '', feature),
           cluster_id = sub(':dose', '', cluster_id),
           cluster_id = as.factor(as.numeric(cluster_id)),
           response = Estimate,
           sem = Std..Error) %>%
    select(cluster_id, response, sem)
  
  dose_response
}


# Aggregate at the cluster level
aggregate_by_cluster <- function(aggregated) {
  aggregated %>%
    group_by(cluster_id
             ,study
             ,stratum
             ,stratum_full_name
             ,cluster_number
             ,sex
             ,rate
             ,fractions
             ,treatment_age
             ,avg_treatment_age
             ,last_treatment_age
             ,full_cluster_name
    ) %>%
    summarize(n = sum(n))
}


show_overall_dose_response <- function(by_cluster) {
  ggplot(by_cluster %>% filter(avg_treatment_age > 0),
         aes(avg_treatment_age, 
             response, 
             color=fractions == 1)) +
    geom_point(size=3) +
    geom_segment(aes(x = avg_treatment_age,
                     xend = avg_treatment_age,
                     y = response - sem,
                     yend = response + sem,
                     size = 1/sem
    ), alpha=0.5) +
    geom_smooth(method="lm", 
                formula="y ~ exp(-x) - 1", 
                aes(weight=1),
                se=FALSE, 
                size=5) +
    scale_size(range=c(0.2, 5), guide=FALSE) +
    scale_color_manual(values=c("red", "black"), 
                       labels=c("protracted", "acute"),
                       name="") +
    scale_x_continuous(label=percent) +
    scale_y_continuous(label=percent) +
    xlab("Average relative treatment age") +
    ylab("Excess relative mortality per Gy") +
    coord_cartesian(ylim = c(-0.10, 0.30))
}

model_response <- function(by_cluster) {
  model <- lm(log(response) ~ 
                stratum + 
                avg_treatment_age + 
                I(fractions == 1) - 
                1,
              by_cluster %>%
                mutate(
                  response = ifelse(response < 0.00001, 0.00001, response)
                ),
              weights = 1 / sem^2)
}

add_predictions <- function(data, model) {
  data %>%
    mutate(prediction = exp(predict(model)))
}

add_cluster_predictions_to_aggregated <- function(by_cluster, aggregated) {
  if('prediction' %in% names(aggregated)) {
    aggregated <- aggregated %>% 
      select(-prediction)
  }
  
  aggregated <- merge(
    aggregated, 
    by_cluster %>%
      select(cluster_id, 
             prediction),
    by="cluster_id")
  
  aggregated %>%
    mutate(prediction = dose * prediction + 1)
}

ggsave_for_publication <- function(name, graph, width=7.2, height=5.4, ...) {
  suppressWarnings(ggsave(name,
                          graph,
                          dpi=600, 
                          width=width,   # < 7.5 in
                          height=height,  # < 8.75 in
                          units='in',
                          ...))
}

show_aggregated <- function(aggregated) {
  # Clean up
  g <- aggregated %>% 
#    filter(stratum %in% fractionation_comparisons(aggregated)) %>%
    mutate(cluster_number = ifelse(dose == 0, "C", as.character(cluster_number)),
           type = ifelse(dose == 0, 'control', ifelse(protracted, 'protracted', 'acute')),
           stratum_full_name = paste0(stratum_id, ". ", stratum_full_name))
  
  # Organize strata by number of animals
  order <- g %>% 
    group_by(stratum_full_name) %>%
    summarize(n=sum(n)) %>%
    arrange(-n) %>%
    select(stratum_full_name)
  g$stratum_full_name <- factor(g$stratum_full_name, levels=order$stratum_full_name)
  
  ggplot(g,
         aes(dose,
             mortality,
             label=cluster_number,
             color=type)) +
    geom_line(data = g %>% 
                filter(dose == 0 | protracted),
              aes(y=prediction), 
              color="red",
              show_guide=FALSE,
              stat="smooth",
              method="lm", 
              formula="y ~ x", 
              se=FALSE,
              size=0.5) +
    geom_line(data = g %>% 
                filter(dose == 0 | !protracted),
              aes(y=prediction), 
              color="black",
              show_guide=FALSE,
              stat="smooth",
              method="lm", 
              formula="y ~ x", 
              se=FALSE,
              size=0.5) +
    geom_segment(aes(x = dose,
                     xend = dose,
                     y = mortality - sem,
                     yend = mortality + sem,
                     size = 1/sem
    ), alpha=0.3, show_guide=FALSE) +
    geom_point(size=3,
               show_guide=FALSE,
               alpha=0.9,
               fill="white",
               pch=21) +
    geom_text(size=2,
              show_guide=FALSE) +
    facet_wrap(~ stratum_full_name) +
    scale_y_continuous(labels=percent) +
    scale_size_continuous(range=c(3, 7)) +
    coord_cartesian(ylim=c(min(g$mortality) * 0.9 , max(g$mortality) * 1.1)) +
    xlab("\ndose (Gy)") +
    ylab("relative mortality\n") +
    scale_color_manual(values=c("black", "grey30", "red"), 
                       name="") +
    theme(
      strip.text.x = element_text(hjust=0, size = 5),
      axis.text = element_text(color="black", size=5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill=NA),
      panel.background = element_blank(),
      legend.key = element_blank(),
      strip.background = element_blank()
    )
}


get_coefficients <- function(model) {
  coefficients <- data.frame(coefficients(summary(model)))
  coefficients %>%
    mutate(feature = rownames(coefficients),
           x = Estimate,
           sem = Std..Error) %>%
    select(feature, x, sem)
}

report_log_confidence <- function(x, sem) {
  p <- function(x) round(exp(x), 1)
  cat(p(x), " (", p(x - 1.96*sem), ", ", p(x + 1.96*sem), ")", "\n", sep="")
}

report_the_effects_of_age_and_fractionation <- function(coefficients) {
  # Fractionation
  fractionation <- coefficients %>% 
    filter(feature == "I(fractions == 1)TRUE")
  print0("\nfractionation:")
  report_log_confidence(fractionation$x, fractionation$sem)  
  # 3.0 (1.8, 5.1)
  
  # Age effects
  age <- coefficients %>% 
    filter(grepl("avg_treatment_age", feature))
  decade_conversion = 10 / 75  # Assuming a 75 year lifespan
  print0("\nage:")
  report_log_confidence(age$x * decade_conversion, 
                        age$sem * decade_conversion)  
  # 0.7 (0.7, 0.9)
}

# Ozasa model
get_mle_coefficients <- function(model) {
  coefficients <- data.frame(summary(model)@coef)[,1:2]
  rows <- rownames(coefficients)
  names(coefficients) <- c('x', 'o')
  coefficients <- coefficients %>%
    mutate(lower = x - 1.96 * o,
           upper = x + 1.96 * o)
  rownames(coefficients) <- rows
  round(coefficients, 5)
}

add_arguments <- function(fn, arguments){
  # adds arguments to a function, fn
  formals(fn) <- c(formals(fn), arguments)
  fn
}
# Ozasa style rate
get_rate_function <- function(start, err, baseline) {
  err <- add_arguments(err, start)
  baseline <- add_arguments(baseline, start)
  
  rate <- function() {
    # Forward relevant arguments
    a <- as.list(match.call())
    a <- a[2:length(a)]
    
    err <- do.call(err, a)
    baseline <- do.call(baseline, a)
    
    lambda <- (1 + err) * baseline
    
    lambda
  }
  
  rate <- add_arguments(rate, start)
  rate
}

get_likelihood_function <- function(outcome, sem, start, err, baseline) {
  rate <- get_rate_function(start, err, baseline)
  
  negative_log_likelihood <- function() {
    # Forward relevant arguments
    a <- as.list(match.call())
    a <- a[2:length(a)]
    
    # get predicted rate
    rate <- do.call(rate, a)
    
    # estimate likelihood
    error <- d[[outcome]] - rate
    tau2[tau2 < 0] <- 0
    sem <- (d[[sem]]^2 + tau2)^0.5
    nll <- -sum(dnorm(error, 0, sem, log=T))

    nll
  }
  negative_log_likelihood <- add_arguments(negative_log_likelihood, start)
  negative_log_likelihood
}
predict_mle <- function(model, start, err, baseline) {
  rate <- get_rate_function(start, err, baseline)
  do.call(rate, as.list(model@coef))
}
build_model <- function(likelihood_function, start) {
  model <- mle2(likelihood_function, 
               start, 
               method="BFGS"
               ,control = list(
#                   trace=TRUE
                 # maxit=1000
                 parscale=abs(unlist(start)) + 0.01
#                  ,ndeps=as.list(rep(1e-3, length(start)))
               ))
  model
}
get_baseline_function <- function(stratum_id) {
  stratum <- unique(stratum_id)
  n <- length(stratum)
  coefficients <- paste0(
    "r_",1:n," * as.integer(d$stratum_id == '", stratum, "') +"
  )
  baseline <- eval(parse(text=paste0(c(
    "function() {", 
      coefficients, 
      "0", 
    "}"), 
    collapse="\n"
  )))
  baseline
}

get_err_function <- function(stratum_id) {
  stratum <- unique(stratum_id)
  n <- length(stratum)
  coefficients <- 
#    paste0("err_",1:n," * d$dose * as.integer(d$stratum_id == '", stratum, "') +")
    "err_dose * d$dose + "
  err <- eval(parse(text=paste0(c(
    "function() {", 
    "ifelse(d$protracted, dref, 1) *",
    "ifelse(!is.na(d$avg_treatment_age), exp(err_treatment_age * d$avg_treatment_age), 1) *",
    "(",
    coefficients, 
    "0", 
    ")",
    "}"), 
    collapse="\n"
  )))
  
  err
}

get_start_list <- function(stratum_id) {
  stratum <- unique(stratum_id)
  n <- length(stratum)
  start <- eval(parse(text=paste0(c(
    "list(",
    paste0("r_", 1:n, "= 1,"),
    "err_dose = 0.1,",
#    paste0("err_", 1:n, "= 0.1,"),
    "dref = 1,",
    "err_treatment_age = -0.7,",
    "tau2 = 0.1",
    ")",
    collapse="\n"
  ))))

  start
}

report_poisson <- function(model) {
  c <- data.frame(coefficients(summary(model))[,1:2])
  rows <- rownames(c)
  names(c) <- c('x', 'o')
  c <- c %>%
    mutate(lower = exp(x - o * 1.96),
           middle = exp(x),
           upper = exp(x + o * 1.96)) %>%
    select(lower, middle, upper)
  rownames(c) <- rows
  c
}

