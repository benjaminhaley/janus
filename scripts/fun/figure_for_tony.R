# Figure for AnimalEPRIlowdoserate2016.docx
# Tony Brook's paper on low dose rate effects

library(dplyr)
library(ggplot2)

data <- read.csv("~/Downloads/demographics.csv")
summary <- data %>%
  filter(radn %in% c('C', 'G'),
         species == "Mus musculus",
         cause_of_death %in% c('Died', 'Sacrifice, moribund')) %>%
  group_by(expt, tmt, radn) %>%
  summarize(age = mean(age),
            dose = mean(total_dose) / 100,
            fractions = mean(fractions),
            dose_rate = mean(dose_rate) / 100,
            minutes = mean(time_min))


ggplot(summary, aes(dose, age, color=fractions > 1)) +
  scale_color_manual(values=c('red', 'black')) +
  geom_point() + 
  geom_smooth(method="lm", formula="y~x") + 
  #facet_wrap(~expt) +
  theme_classic() + 
  annotate("text", x=5, y=600, color='red', label="acute") + 
  annotate("text", x=25, y=800, color='black', label="fractionated") +
  ylab("lifespan") +
  xlab("dose (Gy)")
  