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
  
  # Organize strata by number of animals
  order <- data %>% 
    group_by(stratum) %>%
    summarize(n=length(stratum)) %>%
    arrange(-n) %>%
    select(stratum)
  data$stratum <- factor(data$stratum, levels=order$stratum)
  
  # Show survival
  ggplot(data,
         aes(lifespan,
             survival,
             color=dose,
             group=group)) +
    geom_rect(
      aes(xmin=first,
          xmax=last,
          ymin=0,
          ymax=1),
      color="black"
    ) +
    geom_path() +
    facet_wrap(~ stratum, scales="free_x") +
    scale_color_continuous(
      guide = guide_legend(title = "dose (Gy)"),
      trans = "sqrt"
    ) +
    expand_limits(x = -4) +
    scale_y_continuous(labels=percent) +
    theme(
      text = element_text(size = 5),
      axis.text = element_text(color="black", size=6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill=NA),
      panel.background = element_blank(),
      strip.background = element_blank()
    ) +
    xlab("age (days)")
}


# The number of distinct values not equal to na
# n_unique_values(c(0, 0, NA)) == 1
n_unique_values <- function(x) length(unique(x)[!is.na(unique(x))])

# A nicer print function
print0 <- function(...) cat(paste(..., sep='', collapse = ''), "\n")


# Count the number of animals, groups, and studies in an analysis
count <- function(message, data) {
  
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
  
  count("Originally: ", data)
  
  data <- data %>%
    filter(dose <= dose_limit)
  
  count("After filtering for dose: ", data)

  # See how many animals have direct comparisons  
  data <- data %>% 
    filter(stratum %in% is_comparison(data))
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
    filter(lifespan > last + median_lifespan * censor)
  
  count(
    paste("After censoring animals that died within ", round(censor * 100), 
          "% of median lifespan after the final treatment ")
    , data
  )

  # Be sure the cluster numbers remain correct
  data <- renumber(data)
  
  data
}

# How to aggregate
aggregate <- function(data) {
  # Mean Lifespans
  aggregate = data %>%
    ungroup() %>%
    group_by(study,
             stratum,
             cluster_number,
             cluster,
             group,
             sex, 
             dose, 
             rate, 
             fractions,
             interval) %>%
    summarize(
      avg_treatment_age = mean((last_treatment_age + treatment_age)/2),
      treatment_age = mean(treatment_age),
      last_treatment_age = mean(last_treatment_age),
      n=length(lifespan),
      mortality=mean(1/lifespan),
      sem=sd(1/lifespan)/n^0.5,
      lifespan=mean(lifespan)
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
  
  # Add controls to each cluster
  aggregate <- ddply(aggregate, .(stratum, cluster), function(df) {
    
    # Look for a control exactly matches to the cluster
    control <- controls %>% 
      filter(stratum %in% only(df$stratum),
             cluster %in% only(df$cluster))
    
    # If that can't be found, look for a match to the whole stratum
    if(nrow(control) == 0) {
      control <- controls %>% 
        filter(stratum %in% only(df$stratum))
    }
    
    # Find the control in the aggregate data
    control <- aggregate %>% 
      filter(as.character(study) == only(control$study),
             as.character(group) %in% control$group)
    
    # Add the control to the cluster
    control$cluster <- only(df$cluster)
    control$stratum <- only(df$stratum)
    control$fractions <- mean(df$fractions)
    control$rate <- mean(df$rate)
    control$treatment_age <- mean(df$treatment_age)
    control$last_treatment_age <- mean(df$last_treatment_age)
    control$avg_treatment_age <- mean(df$avg_treatment_age)
    control$stratum_full_name <- only(df$stratum_full_name)
    control$cluster_number <- only(df$cluster_number)
    df <- rbind(df, control)
    
    df
  })
  
  # Remove groups that aren't a part of a cluster
  # (this can happen if controls were not put in a specific cluster)
  aggregate <- aggregate %>%
    filter(!is.na(cluster))
  
  # Remove clusters that are only controls
  # (this can happen if controls were not put in a specific cluster)
  aggregate <- ddply(aggregate, .(stratum, cluster), function(df){
    if(all(df$dose == 0)) { return(NULL) }
    
    df
  })
  
  # Scale to control lifespans/mortality
  aggregate <- ddply(aggregate, .(stratum, cluster), function(df){
    controls = df$dose == 0
    control_lifespan = only(df$lifespan[controls])
    control_mortality = only(df$mortality[controls])
    scale_lifespan <- function(x) x / control_lifespan
    scale_mortality <- function(x) x / control_mortality
    df <- df %>%
      mutate(lifespan = scale_lifespan(lifespan),
             avg_treatment_age = scale_lifespan(avg_treatment_age),
             treatment_age = scale_lifespan(treatment_age),
             last_treatment_age = scale_lifespan(last_treatment_age),
             sem = scale_mortality(sem),
             mortality = scale_mortality(mortality)
      )
    
    df
  })
  
  # Unique id for each stratum
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

stratum_with_weird_survivals <- c(
  "9-4 SCK/CEN ♂ C57BL/Cnb mice\n  X-ray @28 days",
  "9-7 SCK/CEN ♂ C57BL/Cnb mice\n  X-ray",
  "11-2-B TNO ♀ WAG/RIJ rats\n  X-ray @56 days"  # maybe?
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

is_comparison <- function(data) {
  unique(c(fractionation_comparisons(data), age_comparisons(data)))
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
    xlab("Average treatment age (relative to control lifespan)") +
    ylab("Excess Mortality per Gy (relative to control mortality)") +
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
  g <- aggregated %>% 
    filter(stratum %in% fractionation_comparisons(aggregated))
  ggplot(g,
         aes(dose,
             mortality,
             label=cluster_number,
             group=cluster,
             color=fractions == 1)) +
    geom_line(aes(y=prediction), 
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
    ), alpha=0.5, show_guide=FALSE) +
    geom_point(size=2,
               show_guide=FALSE,
               alpha=0.5) +
    facet_wrap(~ stratum_full_name) +
    scale_y_continuous(labels=percent) +
    coord_cartesian(ylim=c(min(g$mortality) * 0.9 , max(g$mortality) * 1.1)) +
    xlab("dose (Gy)") +
    ylab("mortality (relative to controls)") +
    scale_color_manual(values=c("red", "black"), 
                       labels=c("protracted", "acute"),
                       name="") +
    theme(
      strip.text.x = element_text(hjust=0, size = 5),
      axis.text = element_text(color="black", size=6),
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