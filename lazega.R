library(ggplot2)
library(plyr)

attributes = read.table("atts.csv", sep = ",")
colnames(attributes) <- c("ID", "status", "gender", "office", "seniority", "age", "practice", "lawschool")

network.coworker = read.table("ELwork.dat")
network.advice = read.csv("advice.csv")
network.friendship = read.csv("friends.csv")

network.valued = network.advice + network.coworker + network.friendship 

barplot(prop.table(table(attributes$Gender)))


plot.gender <- ggplot(attributes, aes(x = Gender, y = ))
plot.gender + geom_bar(aes(fill = drv), position = "fill")

a <- as.factor(attributes$Gender)
