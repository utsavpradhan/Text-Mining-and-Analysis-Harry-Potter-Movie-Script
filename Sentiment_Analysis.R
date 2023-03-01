library(dplyr)
library(igraph) 

##Reading Data
setwd("C:/Users/Lenovo/Downloads")
data <- read.csv("HP_script.csv")

#data<- data[1:1371,] for before case
#data<- data[1371:1536,] for after case

##EDA and text preprocessing
data <- data %>% select(Speaker, Spoken.to)
# remove conditions when more than one person are speaking or heard by at same time and remove when there are empty redundant conditions
data <- data %>% filter(Speaker != "" && Spoken.to != "" && Spoken.to != "Everybody" && Speaker != "Everybody" )

data <- data %>% select(Speaker, Spoken.to)

#creating a DF which shows Speakers, spoken to and number of conversations
conversations <- data %>% group_by(Speaker, Spoken.to) %>% dplyr::summarise(counts = n()) 

#counting maximum number of conversations
max_conversations<-conversations[which.max(conversations$counts),]

#Adding additional column conbining speaker and spoken to column to make interactions plot
conversations$convo_combined<-paste(conversations$Speaker,"-",conversations$Spoken.to)

#Arrange DF in decreasing order of number of conversations
conversations<-conversations[order(-conversations$counts),]
View(conversations)

#Working on top 50 conversations
top50_conversations<-head(conversations,50)

# Vector of character names
nodes <- c(as.character(top50_conversations$Speaker), as.character(top50_conversations$Spoken.to))
#removing repeated data
nodes <- unique(nodes)

##SNA
# create the igraph object
my_graph <- graph_from_data_frame(d=top50_conversations, vertices=nodes, directed=FALSE)

# view the names of each node
V(my_graph)$name

# view the edges 
E(my_graph)

# Plotting with layout for better visibility 
plot(my_graph, vertex.label.color = "black", layout = layout_in_circle(my_graph))

#plot a graph where edges are weighed based on number of conversations
w1 <- E(my_graph)$counts
#dividing weight by 10 to prevent formation of too thick lines
plot(my_graph, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w1/10,  
     layout = layout_as_star(my_graph))

#Directed graph
g <- graph_from_data_frame(top50_conversations, directed = TRUE)

plot(g, 
     vertex.label.color = "black", 
     edge.color = 'blue',
     vertex.size = 0,
     edge.arrow.size = 0.2,
     layout = layout_as_star(g))


# calculate betweenness of each vertex
g.b <- betweenness(g, directed = TRUE)
g.b

# Create plot with vertex size determined by betweenness score
plot(g, 
     vertex.label.color = 'black',
     edge.color = 'black',
     vertex.size = sqrt(g.b) / 1.5,
     edge.arrow.size = 0.03,
     layout = layout.fruchterman.reingold(g))


# geodesic distances of harry potter, dolores umbridge, cornelius fudge, albus dumbledore
geo_Harry_Potter <- make_ego_graph(g, 2, nodes = 'Harry Potter', mode = c("all"))[[1]]
dists <- distances(geo_Harry_Potter, "Harry Potter")
colors <- c("black", "blue", "orange", "red", "green")
V(geo_Harry_Potter)$color <- colors[dists+1]
plot(geo_Harry_Potter, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05)


geo_umbridge <- make_ego_graph(g, 2, nodes = 'Dolores Umbridge', mode = c("all"))[[1]]
dists <- distances(geo_umbridge, "Dolores Umbridge")
colors <- c("black", "blue", "orange", "red", "green")
V(geo_umbridge)$color <- colors[dists+1]
plot(geo_umbridge, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05)

geo_Fudge <- make_ego_graph(g, 2, nodes = 'Cornelius Fudge', mode = c("all"))[[1]]
dists <- distances(geo_Fudge, "Cornelius Fudge")
colors <- c("black", "blue", "orange", "red", "green")
V(geo_Fudge)$color <- colors[dists+1]
plot(geo_Fudge, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05)

geo_Albus_Dumbledore <- make_ego_graph(g, 2, nodes = 'Albus Dumbledore', mode = c("all"))[[1]]
dists <- distances(geo_Albus_Dumbledore, "Albus Dumbledore")
colors <- c("black", "blue", "orange", "red", "green")
V(geo_Albus_Dumbledore)$color <- colors[dists+1]
plot(geo_Albus_Dumbledore, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05)
