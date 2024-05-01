library(igraph)
library(tidyverse)
library(Matrix)

##################################### Reading in the files #####################
actors <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-actor.csv")
crew <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-crew.csv")
movies <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-movies.csv")
cast_edges <- read_csv("~/MATHCOMP479/tr1/movienetwork/edge-cast.csv")

# processing  to include only the last 10 years
movies <- movies %>% filter(Year >= 2007)
cast_edges <- cast_edges %>% filter(Year >= 2007)

actors_after_2007 <- actors %>% filter(Id %in% cast_edges$Target)
write_csv(actors_after_2007, "actrs_after_2007.csv")
################################################################################


############################## creating links between actors ###################
actr_edge_list <- list()

t <- 1

i1 <- 0
i2 <- 0
i3 <- 0

for (movie in unique(cast_edges$Source)){
  
  t1 <- Sys.time()  
  
  actors_in_movie <- cast_edges$Target[which(cast_edges$Source==movie)]
  
  t2 <- Sys.time() 
  
  if (length(actors_in_movie )>= 2){
    
    actor_pairs <- combn(actors_in_movie,2)
    
    t3 <- Sys.time() 
    
    actr_edge_list[[t]] <- t(actor_pairs)
    
    t4 <- Sys.time() 
    
    i1 <- i1 + t2-t1
    i2 <- i2 + t3-t2
    i3 <- i3 + t4-t3
    
    
    if (t%%1000==0){
      print(paste("i1: ", i1, " i2: ", i2, " i3: ", i3, " total movies processed: ", t))
    }
    
    t <- t+1
    
  }
  
}

actr_edges <- do.call(rbind, actr_edge_list)
actr_edges_vctr <- c(t(actr_edge_mat))
###############################################################################


############################# setting up the network ###########################
actr_actr_graph <- make_graph(actr_edges_vctr, directed = FALSE)
E(actr_actr_graph)$weight <- 1
actr_actr_graph <- igraph::simplify(actr_actr_graph, edge.attr.comb = list(weight="sum"))
################################################################################


################################### centrality #################################
eigen_cent <- eigen_centrality(actr_actr_graph)
eigen_cent_vals <- sort(eigen_cent$vector, decreasing = TRUE)

actors_after_2007 <- read_csv("actrs_after_2007.csv")

actors_after_2007$Label[which(actors_after_2007$Id %in% names(eigen_cent_vals[1:25]))] # display the 25 most eigencentral actors
################################################################################