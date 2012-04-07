# Quick test of my spline intutition

library(ggplot2)
data(sunspot.month)   # sunspots observed by month over time
sunspot.month = head(data.frame(sunspot.month), 200)
sunspot.month$x <- as.numeric(rownames(sunspot.month))

# Simple line model
model <- glm(sunspot.month ~ x, data=sunspot.month)
summary(model)
sunspot.month$predict <- predict(model, sunspot.month)

# Add a splines
sunspot.month$x2         <- sunspot.month$x^2

sunspot.month$spline_0_1 <- sunspot.month$x - 50
sunspot.month$spline_0_1[sunspot.month$spline_0_1 < 0] <- 0
sunspot.month$spline_0_2 <- sunspot.month$spline_0_1^2

sunspot.month$spline_1_1 <- sunspot.month$x - 115
sunspot.month$spline_1_1[sunspot.month$spline_1_1 < 0] <- 0
sunspot.month$spline_1_2 <- sunspot.month$spline_1_1^2


sunspot.month$spline_2_1 <- sunspot.month$x - 175
sunspot.month$spline_2_1[sunspot.month$spline_2_1 < 0] <- 0
sunspot.month$spline_2_2 <- sunspot.month$spline_2_1^2


# Model with splines
model <- glm(sunspot.month ~ 1 + x + x^2 + spline_0_2 + spline_1_2 + spline_2_2, data=sunspot.month)
summary(model)
sunspot.month$predict_splines <- predict(model, sunspot.month)


head(sunspot.month)

ggplot(sunspot.month) + 
	geom_point(aes(x, sunspot.month), size=1) +
	geom_point(aes(x, predict), size=1, color="blue") +
	geom_point(aes(x, predict_splines), size=1, color="red")
