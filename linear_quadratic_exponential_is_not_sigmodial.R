a <- 1
B <- 20000000000000
e <- -0.7
dose <- seq(0, 4, 0.01)

response <- (a * dose + B * dose^2) * exp(dose * e)
library(ggplot2)
ggplot(NULL, aes(dose, response)) + geom_path()
