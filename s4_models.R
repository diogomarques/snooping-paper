# List experiment regression models for Study 4

library(list)
library(dplyr)
source("s4_data.R")

# How do age and depth predict estimated proportion?
fit.age = ictreg(list ~ age, data = list.by.treat.age.depth, treat = "treat", J = 4, method = "ml")
fit.depth = ictreg(list ~ depth, data = list.by.treat.age.depth, treat = "treat", J = 4, method = "ml")
fit.both = ictreg(list ~ age + depth, data = list.by.treat.age.depth, treat = "treat", J = 4, method = "ml")
#fit.interaction =  # ml model doesn't converge, either constrained or not
#  ictreg(list ~ age * depth, data = survey4, treat = "treat", J = 4, method = "lm") # lm yelds lower RSE
#  ictreg(list ~ age * depth, data = survey4, treat = "treat", J = 4, method = "nls")

summary(fit.age)
summary(fit.depth)
summary(fit.both)

# Image depicting the modela
attach(list.by.treat.age.depth)
label.depth = "Depth of adoption (scale 10-70)"
label.age = "Age (years)"
label.prediction = "Predicted likelihood (0 to 1)"
#pdf(file = "figures/s4_prevalence.pdf", width = 10, height = 7)
par(mfrow=c(1,2))
plot(age, predict.ictreg(fit.both)$fit,
     ylim = c(0,1),
     ylab = label.prediction,
     xlab = label.age,
     col = rgb(0.7,0.7,.7))
seq = seq(min(age), max(age), by = 0.01)
lines(seq, predict.ictreg(fit.age, newdata = data.frame(age=seq))$fit, lwd = 2)
plot(depth, predict.ictreg(fit.both)$fit,
     ylim = c(0,1),
     ylab = label.prediction,
     xlab = label.depth,
     col = rgb(0.7,0.7,.7))
seq = seq(min(depth), max(depth), by = 0.01)
lines(seq, predict.ictreg(fit.depth, newdata = data.frame(depth=seq))$fit, lwd = 2)
#dev.off()


# Correlations between predictions
predictions = data_frame(age = predict.ictreg(fit.age)$fit, 
                         depth = predict.ictreg(fit.depth)$fit, 
                         both = predict.ictreg(fit.both)$fit)

cor.test(predictions$age, predictions$depth)
cor.test(predictions$age, predictions$both)
cor.test(predictions$depth, predictions$both)

detach(list.by.treat.age.depth)