library(ggplot2)
library(plyr)
library(reshape)
library(igraph)
library(NetData)

#importing data
attributes = read.table("atts.csv", sep = ",", header = T)
network.coworker = read.table("ELwork.dat")
network.advice = read.csv("advice.csv")
network.friendship = read.csv("friends.csv")

network.valued = network.advice + network.coworker + network.friendship 


#overview plots
attributes$gender <- as.factor(attributes$gender)
ggplot(attributes, aes(x = gender, fill = gender)) + geom_bar()

ggplot(attributes, aes(attributes$seniority, fill = as.factor(gender))) +
  geom_histogram(binwidth = 1) +
  labs(title = "Seniority and Gender", x = "Seniority - Years in the firm", y = "Number of staff") +
  ylim(0, 9)

ggplot(attributes, aes(attributes$seniority, fill = as.factor(status))) +
  geom_histogram(binwidth = 1) +
  labs(title = "Seniority and Affiliation", x = "Seniority - Years in the firm", y = "Number of staff") +
  ylim(0, 9)

#creating edge list
network.matrix.friendship <- data.matrix(network.friendship, rownames.force = NA)
g.friendship <- graph.adjacency(network.matrix.friendship, mode="directed")

#settting vertext attributes
V(g.friendship)$Gender <- attributes$gender
V(g.friendship)$Status <- attributes$status
V(g.friendship)$Practice <- attributes$practice

list.vertex.attributes(g.friendship)

get.edgelist(g.friendship)
V(g.friendship)$degree <- degree(g.friendship, mode = c("all"))

#visualizing the graph
layout1 <- layout.fruchterman.reingold(g)

#Vertice colored basde on gender
pdf("Gender.pdf")
gender_vertex_colors = get.vertex.attribute(g.friendship,"Gender")
colors = c('Black','Yellow')
gender_vertex_colors[gender_vertex_colors == 1] = colors[1]
gender_vertex_colors[gender_vertex_colors == 2] = colors[2]

plot(g.friendship, 
     layout=layout1, 
     vertex.color=gender_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3)
legend(1, 1.25, legend = c('Male', 'Female'), 
       col = c('Black', 'Yellow'), lty=1, cex = 0.5)
dev.off()

#Vertice colored based on Status
pdf("Status.pdf")
status_vertex_colors = get.vertex.attribute(g.friendship,"Status")
colors = c('Blue','Red')
status_vertex_colors[status_vertex_colors == 1] = colors[1]
status_vertex_colors[status_vertex_colors == 2] = colors[2]

plot(g.friendship, 
     layout=layout1, 
     vertex.color=status_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3)
legend(1, 1.25, legend = c('Partner', 'Associate'), 
       col = c('Blue','Red') , lty=1, cex = 0.5)
dev.off()


V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
#egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
#E(g)$color <- rgb(.5, .5, 0, egam)
#E(g)$width <- egam 
# plot the graph in layout1 
  plot(g, layout=layout1)

