# List experiment regression models for Study 3
library(list)
library(dplyr)
source("s3_data.R")

# How do age and ownership status predict estimated proportion?
fit.ml = ictreg(UCT ~ AGE + OWNER, treat = "GROUP", J = 4, 
                data = uct.by.group.age.owner, method = "ml",
                constrained = F)
# Appendix E.1:
fit.ml

# Plot the prevalence as a funtion of age, with ownership as 3rd dimension 
# factor. TODO: use prediction with new data instead of interpolation for
# cleaner curves. 

# get prevalence predictions
predictions = predict(fit.ml)[[1]]

# filter for ownership factor
owners = (uct.by.group.age.owner$OWNER == "YES")

# create interpolation functions
f.owners = approxfun(uct.by.group.age.owner$AGE[owners], predictions[owners])
f.nonowners = approxfun(uct.by.group.age.owner$AGE[!owners], predictions[!owners])

#pdf(file = "figures/s3_prevalence.pdf")
curve(f.owners(x), 18, 80, ylab = "Predicted likelihood (0 to 1)", 
      xlab = "Age (years)", n= 1000, lwd = 2, bty = "n")
axis(side = 1, lwd = 1.5)
axis(side = 2, lwd = 1.5)
curve(f.nonowners(x), 18, 80, lty = 2, add = T, n = 1000, lwd = 2)
legend("topright", bty = "n", lwd = c(2,2), lty=c(1,2), 
       legend = c( "Smartphone owners","Others"))
#dev.off()
