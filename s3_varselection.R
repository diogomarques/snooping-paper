# Creates individual linear regression models w/ 1 variable plus group
# assingment, and compares them to reduced model.

# Load data
source("s3_loadData.R")

# Helper to print coefficient in way easy to copy to Latex
printCoef = function(model) {
  sum = summary(model)
  coef = sum$coefficients
  coeftxt = apply(coef, 1, function(x) paste(round(x[1], digits=5), 
                                   round(x[2], digits = 5), 
                                   round(x[3], digits = 3),
                                   round(x[4], digits = 4),
                                   sep = " & "))
  print(coeftxt)
}

# Model for group assignment only
model.base = lm(UCT ~ GROUP, data = uct.by.group)
sum = 
  summary(model.base)
r2.reduced = summary(model.base)$r.squared

# Model for group + GENDER
model = lm(UCT ~ GROUP + GENDER, data = uct.by.group.gender)
anova = anova(model.base, model, test = "F")
summary(model)
printCoef(model)
round(summary(model)$r.squared - r2.reduced, digits = 3) # delta r2
round(anova$F[2], digits = 2) # anova F statistc
anova$Df[2] # d.f.
round(anova$"Pr(>F)"[2], digits = 4) # p-value

# Model for group + AGE
model = lm(UCT ~ GROUP + AGE, data = uct.by.group.age)
anova = anova(model.base, model, test = "F")
summary(model)
printCoef(model)
round(summary(model)$r.squared - r2.reduced, digits = 3) # delta r2
round(anova$F[2], digits = 2) # anova F statistc
anova$Df[2] # d.f.
round(anova$"Pr(>F)"[2], digits = 4) # p-value

# Model for group + EDUCATION
# Small fix for education: set baseline to post-graduates
uct.by.group.education$ED = uct.by.group.education$EDUCATION
levels(uct.by.group.education$ED)
levels(uct.by.group.education$ED) = c("Col", "Grad", "HS", "LessHS", "Oth", "Ba")
uct.by.group.education$ED = relevel(uct.by.group.education$ED, ref = "Grad")

model = lm(UCT ~ GROUP + ED, data = uct.by.group.education)
anova = anova(model.base, model, test = "F")
round(summary(model)$r.squared - r2.reduced, digits = 3) # delta r2
round(anova$F[2], digits = 2) # anova F statistc
anova$Df[2] # d.f.
round(anova$"Pr(>F)"[2], digits = 4) # p-value
summary(model)
printCoef(model)

# Model for group + REGION
model = lm(UCT ~ GROUP + REGION, data = uct.by.group.region)
anova = anova(model.base, model, test = "F")
round(summary(model)$r.squared - r2.reduced, digits = 3) # delta r2
round(anova$F[2], digits = 2) # anova F statistc
anova$Df[2] # d.f.
round(anova$"Pr(>F)"[2], digits = 4) # p-value
summary(model)
printCoef(model)

# Model for group + OWNER
model = lm(UCT ~ GROUP + OWNER, data = uct.by.group.owner)
anova = anova(model.base, model, test = "F")
round(summary(model)$r.squared - r2.reduced, digits = 3) # delta r2
round(anova$F[2], digits = 2) # anova F statistc
anova$Df[2] # d.f.
round(anova$"Pr(>F)"[2], digits = 4) # p-value
summary(model)
printCoef(model)
