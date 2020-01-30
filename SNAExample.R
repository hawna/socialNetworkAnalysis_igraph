##Load file for SNA analysis -- Reports as rows, sessions as columns

#repRowSessCol.csv generated from 
#Created matrix with each report as a row and the number of actions in each session as columns
data <- read.csv("/Users/file.csv", 
                 na.strings = c("", "NA", "#DIV/0!"), 
                 check.names = FALSE, header = TRUE, row.names = 2)

dim(data)
data <- data[, c(2:10346)];
dim(data)

data <- as.matrix(data)

##SNA Analysis Preparatory Metrics
##Multiply matrix by inverted matrix
report.net <- data %*% t(data)
diag(report.net) <- NA
dim (report.net)

report.g <- graph.adjacency(report.net, weighted=TRUE,
                            mode="undirected", diag=FALSE, 
                            add.colnames = TRUE, add.rownames = NA)

V(report.g)$name <- colnames(report.net)

#Remove loops
report.g <- simplify(report.g, remove.multiple = F, remove.loops = T)

##Possible layout algorithms
#fruchter-rheingold
la <- layout_with_fr(report.g)
la <- norm_coords(la, ymin=-1, ymax = 1, xmin = -1, xmax = 1) 

#large graph layout
lgl <- layout_with_lgl(report.g)
lgl <- norm_coords(lgl, ymin=-1, ymax = 1, xmin = -1, xmax = 1) 

#kamada kawaii 
kk <- layout_with_kk(report.g)
kk <- norm_coords(kk, ymin=-1, ymax = 1, xmin = -1, xmax = 1) 

#graphopt algorithm
l <- layout_with_graphopt(report.g)
l <- norm_coords(l, ymin=-1, ymax = 1, xmin = -1, xmax = 1)

#Layout on grid
lg <- layout_on_grid(report.g)
lg <- norm_coords(lg, ymin=-1, ymax = 1, xmin = -1, xmax = 1) 

#Get weight attribute
e.wt <- get.edge.attribute(report.g, "weight")

#cut.off <- median(e.wt)
#cut.off <- mean(e.wt)

report.g.demo <- delete_edges(report.g, E(report.g)[weight<1500])
#report.g.demo <- delete_vertices(report.g, E(report.g)[size<7])

#size vertices 
deg <- degree(report.g, mode = "all")
V(report.g.demo)$size <- deg*0.035

edges.demo <- as_data_frame(report.g.demo, what = "edges")
nodes.demo <- as_data_frame(report.g.demo, what = "vertices")

##Reports as Vertices, Sessions as Edges -- Zoom Out for Whole Network

##Community algorothms
#clreport <- cluster_optimal(report.g.sp, weights = E(report.g.sp)$weight, directed = FALSE)
clreport.demo <- cluster_edge_betweenness(report.g.demo, weights = E(report.g.demo)$weight)
#clreport <- cluster_fast_greedy(report.g.sp, weights = E(report.g.sp)$weight, directed = FALSE)
#clreport <- cluster_walktrap(report.g.sp, weights = E(report.g.sp)$weight, directed = FALSE)
#clreport <- cluster_leading_eigen(report.g.sp, weights = E(report.g.sp)$weight, directed = FALSE)
#plot(clreport, report.g.sp)

##2-D interactive plot
#pInteractive <- tkplot(report.g.sp)

##3-D interactive plot
#library(rgl)
##Simple Plot
#p3d <- rglplot(report.g.sp, layout=kk)

##3-D plot with community metric
#p3d <- rglplot(report.g,  
#         layout=kk*12,
#        #vertex.size=4,
#       vertex.color=membership(clreport),
#      vertex.label=V(report.g)$name,
#     edge.width=E(report.g)$weight/200,
#    edge.curved=.5,
#   rescale=T, frame=TRUE, main="SNA Title")

##SNA with communties plotted. cool looking but un-interpreatble
#sna1 <- norm_coords(disp, ymin=-1, ymax = 1, xmin = -1, xmax = 1)
#pCenter <- plot(clreport, report.g.sp,  
#          layout=disp*1,
#          #vertex.size=4,
#          vertex.label=V(report.g)$name,
#          edge.width=E(report.g)$weight/600,
#          edge.curved=.5,
#          rescale=T, frame=TRUE, main="SNA Title")

##SNA with vertex.color by community membership
#neigh.nodes <- neighbors(report.g.sp, v(report.g.sp) [name=="A1"], mode="out")
sna2 <- norm_coords(kk, ymin=-1, ymax = 1, xmin = -1, xmax = 1)
sna2 <- plot(report.g.demo,  
             layout=kk*1,
             #vertex.size=4,
             vertex.color=membership(clreport.demo),
             vertex.label=V(report.g.demo)$name,
             edge.width=E(report.g.demo)$weight/900,
             edge.curved=0.5,
             rescale=T, frame=TRUE, main="Social Network Analysis V1")
sna2

##SNA with vertex.color by neighbor to specified report (A1)
inc.edges_a1 <- incident(report.g.demo, V(report.g.demo) [name=="A1"], mode="all")
ecol_a1 <- rep("gray80", ecount(report.g.demo))
ecol_a1[inc.edges_a1] <- "orange"

sna2_a1 <- norm_coords(kk, ymin=-1, ymax = 1, xmin = -1, xmax = 1)
sna2_a1 <- plot(report.g.demo,  
                layout=kk*1,
                #vertex.size=4,
                edge.color=ecol_a1,
                vertex.color=membership(clreport.demo),
                vertex.label=V(report.g.demo)$name,
                edge.width=E(report.g.demo)$weight/900,
                edge.curved=0.5,
                rescale=T, frame=TRUE, main="Social Network Analysis V2")

##SNA with vertex.color by neighbor to specified report (R1)
inc.edges_r1 <- incident(report.g.demo, V(report.g.demo) [name=="R1"], mode="all")
ecol_r1 <- rep("gray80", ecount(report.g.demo))
ecol_r1[inc.edges_r1] <- "orange"

sna2_r1 <- norm_coords(kk, ymin=-1, ymax = 1, xmin = -1, xmax = 1)
sna2_r1 <- plot(report.g.demo,  
                layout=kk*1,
                #vertex.size=4,
                edge.color=ecol_r1,
                vertex.color=membership(clreport.demo),
                vertex.label=V(report.g.demo)$name,
                edge.width=E(report.g.demo)$weight/900,
                edge.curved=0.5,
                rescale=T, frame=TRUE, main="Social Network Analysis V3")

##Other unused pieces of SNA code

#adjReport <- get.adjacency(report.g, attr = "weight")
#heatmap(adjReport)

###Code to save to png file -- need to alter
#png(file="/Users/mah102/Documents/Nassau Data/ActiveFiles/nassauCollectedCodeRMD/figures/SNAView_repVert_sessEdg__wholeNetwork.png", 
#    width=23000, height=23000, res=400)
#plot(report.g,  
#     layout=kk*0.07,
#     vertex.size=4,
#     vertex.label=V(report.g)$name,
#     edge.width=E(report.g)$weight/150,
#     edge.curved=.5,
#     rescale=F
#)
