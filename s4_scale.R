# Analysis of responses to depth of adoption scale, and relationship with age.
source("s4_data.R")
library(dplyr)

attach(list.by.treat.age.depth)
# Mean answer to scale
summary(depth)
sd(depth)

# Relationship between depth of adoption and age
cor.test(depth, age)
# Distribution of scale
pdf(file = "figures/s4_scale.pdf")
par(mfrow=c(1,1))
label.depth = "Depth of adoption (scale 10-70)"
hist(depth, breaks = 7,
     main = "", 
     xlab = label.depth)
dev.off()
detach(list.by.treat.age.depth)

# Matrix of distribution of single items
pdf(file = "figures/s4_items.pdf", width = 7, height = 7 * 2)
ylim = 350
par(mar=c(2.5,2.1,1.5,1))
par(mfrow=c(5,2))
barplot(table(item1$Item.1), main = "Item-1", ylim = c(0,ylim))
barplot(table(item2$Item.2), main = "Item-2", ylim = c(0,ylim))
barplot(table(item3$Item.3), main = "Item-3", ylim = c(0,ylim))
barplot(table(item4$Item.4), main = "Item-4", ylim = c(0,ylim))
barplot(table(item5$Item.5), main = "Item-5", ylim = c(0,ylim))
barplot(table(item6$Item.6), main = "Item-6", ylim = c(0,ylim))
barplot(table(item7$Item.7), main = "Item-7", ylim = c(0,ylim))
barplot(table(item8$Item.8), main = "Item-8", ylim = c(0,ylim))
barplot(table(item9$Item.9), main = "Item-9", ylim = c(0,ylim))
barplot(table(item10$Item.10), main = "Item-10", ylim = c(0,ylim))
dev.off()
