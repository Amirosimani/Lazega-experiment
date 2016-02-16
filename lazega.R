library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)
library(igraph)
library(NetData)

###
#1.importing data

attributes = read.table("atts.csv", sep = ",", header = T)
network.coworker = read.table("ELwork.dat")
network.advice = read.csv("advice.csv")
network.friendship = read.csv("friends.csv")

network.valued = network.advice + network.coworker + network.friendship 


#overview plots
attributes$gender <- as.factor(attributes$gender)

pdf("gender.pdf")
ggplot(attributes, aes(x = gender, fill = gender)) + geom_bar()
dev.off()

pdf("Status.pdf")
ggplot(attributes, aes(attributes$status, fill = as.factor(gender))) + geom_bar() +
  labs(title = "Status and Gender", x = "Status of employment", y = "Number of staff", fill = "Gender")
dev.off()

pdf("seniority.pdf")
ggplot(attributes, aes(attributes$seniority, fill = as.factor(gender))) +
  geom_histogram(binwidth = 1) +
  labs(title = "Seniority based on gender", x = "Years in the firm", y = "Number of staff") +
  ylim(0, 9)
dev.off()

###
#2. Creating graph data

#creating edge list
network.matrix.friendship <- data.matrix(network.friendship, rownames.force = NA)
g.friendship <- graph.adjacency(network.matrix.friendship, mode="directed")
get.edgelist(g.friendship)

#settting vertext attributes
V(g.friendship)$Gender <- attributes$gender
V(g.friendship)$Status <- attributes$status
V(g.friendship)$Practice <- attributes$practice
V(g.friendship)$degree <- degree(g.friendship, mode = c("all"))

list.vertex.attributes(g.friendship)

###
#3. visualizing the graph

layout1 <- layout.fruchterman.reingold(g.friendship)

#Vertice colored basde on gender
pdf("Gender-Graph.pdf")
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
pdf("Status-Graph.pdf")
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

l2 <- layout_in_circle(g)

pdf("l2.pdf")
plot(g.friendship, 
     layout=l2, 
     vertex.color=status_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3)
legend(1, 1.25, legend = c('Partner', 'Associate'), 
       col = c('Blue','Red') , lty=1, cex = 0.5)
dev.off()

#Vertice colored based on practice
pdf("Practice-graph.pdf")
practice_vertex_colors = get.vertex.attribute(g.friendship,"Practice")
colors = c('Green','Red')
practice_vertex_colors[practice_vertex_colors == 1] = colors[1]
practice_vertex_colors[practice_vertex_colors == 2] = colors[2]

plot(g.friendship, 
     layout=layout1, 
     vertex.color=practice_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3)
legend(1, 1.25, legend = c('litigation', 'corporate'), 
       col = c('Green','Red') , lty=1, cex = 0.5)
dev.off()



###
#4. Network level statistics
deg_in <- degree(g.friendship, mode="in") 
deg_out <- degree(g.friendship, mode="out") 

#degree
stat.1 = matrix( c(mean(deg_in), mean(deg_out), sd(deg_in), sd(deg_out)), nrow=2, ncol=2)
rownames(stat.1) <- c("In Degree", "Out Degree")
colnames(stat.1) <- c("Mean","SD")
pdf("table.pdf")
grid.table(stat.1)
dev.off()

#Assortatitivty and Homphily
assortativity_nominal(g.friendship, V(g.friendship)$Gender, directed=T)
assortativity_nominal(g.friendship, V(g.friendship)$Status, directed=T)
assortativity_nominal(g.friendship, V(g.friendship)$Practice, directed=T)

#Homophilly HHI
get_iqvs <- function(graph, attribute) {
  mat <- get.adjacency(graph)
  attr_levels = get.vertex.attribute(graph, attribute, V(graph))
  num_levels = length(unique(attr_levels))
  iqvs = rep(0, nrow(mat))
  
  for (ego in 1:nrow(mat)) {
    alter_attr_counts = rep(0, num_levels)
    num_alters_this_ego = 0
    sq_fraction_sum = 0
    
    for (alter in 1:ncol(mat)) {
      
      if (mat[ego, alter] == 1) {
        num_alters_this_ego = num_alters_this_ego + 1
        alter_attr = get.vertex.attribute(graph, attribute, (alter - 1))
        alter_attr_counts[alter_attr + 1] = alter_attr_counts[alter_attr + 1] + 1
      }
    }
    for (i in 1:num_levels) {
      attr_fraction = alter_attr_counts[i] /
        num_alters_this_ego
      sq_fraction_sum = sq_fraction_sum + attr_fraction ^ 2
    }
    blau_index = 1 - sq_fraction_sum
    iqvs[ego] = blau_index / (1 - (1 / num_levels))
  }
  return(iqvs)
}

gender_iqvs <- get_iqvs(g.friendship, 'Gender')
status_iqvs <- get_iqvs(g.friendship, 'Status')
practice_iqvs <- get_iqvs(g.friendship, 'Practice')



#####
#advice network

network.matrix.advice <- data.matrix(network.advice, rownames.force = NA)
g.advice <- graph.adjacency(network.matrix.advice, mode="directed")
get.edgelist(g.advice)

#settting vertext attributes
V(g.advice)$Gender <- attributes$gender
V(g.advice)$Status <- attributes$status
V(g.advice)$Practice <- attributes$practice
V(g.advice)$degree <- degree(g.advice, mode = c("all"))

list.vertex.attributes(g.advice)
layout1 <- layout.fruchterman.reingold(g.advice)


pdf("Advice-Gender.pdf")
gender_vertex_colors = get.vertex.attribute(g.advice,"Gender")
colors = c('Black','Yellow')
gender_vertex_colors[gender_vertex_colors == 1] = colors[1]
gender_vertex_colors[gender_vertex_colors == 2] = colors[2]

plot(g.advice, 
     layout=layout1, 
     vertex.color=gender_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3)
legend(1, 1.25, legend = c('Male', 'Female'), 
       col = c('Black', 'Yellow'), lty=1, cex = 0.5)
dev.off()

#Vertice colored based on Status
pdf("Advice-Status.pdf")
status_vertex_colors = get.vertex.attribute(g.advice,"Status")
colors = c('Blue','Red')
status_vertex_colors[status_vertex_colors == 1] = colors[1]
status_vertex_colors[status_vertex_colors == 2] = colors[2]

plot(g.advice, 
     layout=layout1, 
     vertex.color=status_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3)
legend(1, 1.25, legend = c('Partner', 'Associate'), 
       col = c('Blue','Red') , lty=1, cex = 0.5)
dev.off()

#Vertice colored based on practice
pdf("Advice-Practice.pdf")
practice_vertex_colors = get.vertex.attribute(g.advice,"Practice")
colors = c('Green','Red')
practice_vertex_colors[practice_vertex_colors == 1] = colors[1]
practice_vertex_colors[practice_vertex_colors == 2] = colors[2]

plot(g.advice, 
     layout=layout1, 
     vertex.color=practice_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3)
legend(1, 1.25, legend = c('litigation', 'corporate'), 
       col = c('Green','Red') , lty=1, cex = 0.5)
dev.off()

