#### Script to run JaB with Game of Thrones network

### Set up  ----------------------

library(igraph)
library(dplyr)
library(GGally)

source("R/local-boot-functions.R")
source("R/ice-distance.R")


#### Network Data ------------------------------------

cok.char <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/character-predictions.csv")
cok.key <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/book2-nodes.csv")
cok <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/book2-edges.csv")
g.cok <- graph_from_edgelist(as.matrix(cok[, 1:2]), directed = FALSE)
E(g.cok)$weight <- cok$weight
g.adj <- as.matrix(as_adjacency_matrix(g.cok, attr = "weight"))


## Set up for JaB part
network <- g.cok
n <- igraph::gorder(network)
names <- get_nodes(network)
nodes <- names
ids <- 1:n


### Simulaitons need to be run in individual steps, rather than the jab_networks
### wrapper function due to naming convention of loaclboot

## Generate 3000 bootstrap samples
#cok quantile_n = 0.146
boot.result <- localboot_sparce(g.cok, 3000, dist_func = dist_ASE, quantile_n = 0.146, weighted = TRUE)


### Strength -------------------
central.func.name = "strength"
central.package.name = NULL
central.func.args = list()
quant = 0.99

# get original degree statistics
orig.stat <- get_centrality(network, central.func.name, central.package.name , central.func.args)

## Get centrality statistics of boot samples
obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


#node list in each bootstrap sample
m <- length(ids)
node.list <- lapply(boot.result, get_nodes)

#covert from v_i's to names
get_names <- function(x){
  y <- substr(x, 2, nchar(x))
  y <- as.numeric(y) + 1
  return(nodes[y])
}

node.list <- lapply(node.list, get_names)


### Jackknife after Part
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)


#centrality_storage <- list() # used for exploring individual simulated null distributions
counter <- 0
for(i in ids){
  counter <- counter+1
  #get index of all bootstrap samples that do not have node i
  keep_index <- unlist(lapply(node.list, function(x){all(names[i] !=x)}))

  #get number of bootstrap samples node i appeared in
  number.boot.samps[i] <- sum(keep_index)

  #if there are no bootstrap samples without node i, break
  if(all(!keep_index)){
    can.jackknife[counter] <- FALSE
    next
  }

  #Get centrality statistic for all networks in keep_index
  #This repeats each vertex each number of times it was samples
  # i.e. if in bootstrap sample b, node i was sampled 3 times, there will be 3 repetitions of the i^th statistics in the null distribution
  centrality_vector <- c(unlist(central.result[keep_index]))

  #just for exploring null distribution shape, not actually needed to run the algorithm
  #this generally uses a lot of storage even for moderately large B
  #centrality_storage[[i]] <- centrality_vector

  #Get Quantiles
  jack.result[counter] <- stats::quantile(centrality_vector, quant, na.rm = T)


}



### Return something useful
ret <- data.frame(Node_Number = ids,
                  Node_Name = names[ids],
                  Orig_Stat = orig.stat[ids],
                  Upper_Quantile = jack.result,
                  Can_Jackknife = can.jackknife,
                  Num_Boot_Samps = number.boot.samps)

#order from most outside on CI to least
ret$top <- ! (ret$Orig_Stat > ret$Upper_Quantile )
ret$diff <- ret$Orig_Stat - ret$Upper_Quantile
ret <- ret %>%
  dplyr::group_by(top) %>%
  dplyr::arrange(dplyr::desc(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(diff))) %>%
  dplyr::mutate(Influential = !top)%>%
  dplyr::select(-c( top, diff)) %>%
  dplyr::select(Node_Number, Node_Name, Orig_Stat, Upper_Quantile,
                Influential, Rank, Can_Jackknife, Num_Boot_Samps)

save(ret, file = "data/cok-strength.Rdata")


### Eigenvector
# Can use the same set of bootstrap samples
my_eigen <- function(graph){
  igraph::eigen_centrality(graph)$vector
}


central.func.name = "my_eigen"
central.package.name = NULL
central.func.args = list()
quant = 0.95

# get original statistics
orig.stat <- get_centrality(network, central.func.name, central.package.name , central.func.args)


## Get centrality of bootstrap samples
obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


#node list in each bootstrap sample
m <- length(ids)
node.list <- lapply(boot.result, get_nodes)

#covert from v_i's to names
get_names <- function(x){
  y <- substr(x, 2, nchar(x))
  y <- as.numeric(y) + 1
  return(nodes[y])
}

node.list <- lapply(node.list, get_names)


### Jackknife after Part
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)


#centrality_storage <- list() # used for exploring individual simulated null distributions
counter <- 0
for(i in ids){
  counter <- counter+1
  #get index of all bootstrap samples that do not have node i
  keep_index <- unlist(lapply(node.list, function(x){all(names[i] !=x)}))

  #get number of bootstrap samples node i appeared in
  number.boot.samps[i] <- sum(keep_index)

  #if there are no bootstrap samples without node i, break
  if(all(!keep_index)){
    can.jackknife[counter] <- FALSE
    next
  }

  #Get centrality statistic for all networks in keep_index
  #This repeats each vertex each number of times it was samples
  # i.e. if in bootstrap sample b, node i was sampled 3 times, there will be 3 repetitions of the i^th statistics in the null distribution
  centrality_vector <- c(unlist(central.result[keep_index]))

  #just for exploring null distribution shape, not actually needed to run the algorithm
  #this generally uses a lot of storage even for moderately large B
  #centrality_storage[[i]] <- centrality_vector

  #Get Quantiles
  jack.result[counter] <- stats::quantile(centrality_vector, quant, na.rm = T)


}



### Return something useful
ret <- data.frame(Node_Number = ids,
                  Node_Name = names[ids],
                  Orig_Stat = orig.stat[ids],
                  Upper_Quantile = jack.result,
                  Can_Jackknife = can.jackknife,
                  Num_Boot_Samps = number.boot.samps)

#order from most outside on CI to least
ret$top <- ! (ret$Orig_Stat > ret$Upper_Quantile )
ret$diff <- ret$Orig_Stat - ret$Upper_Quantile
ret <- ret %>%
  dplyr::group_by(top) %>%
  dplyr::arrange(dplyr::desc(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(diff))) %>%
  dplyr::mutate(Influential = !top)%>%
  dplyr::select(-c( top, diff)) %>%
  dplyr::select(Node_Number, Node_Name, Orig_Stat, Upper_Quantile,
                Influential, Rank, Can_Jackknife, Num_Boot_Samps)

save(ret, file = "data/cok-eigen.Rdata")




### Plots -----------------------

## Strength
load("data/cok-strength.Rdata")
cok.strength <- ret

cok.strength %>%
  slice(1:25) %>%
  mutate(Node_Name = gsub("-", " ", Node_Name)) %>%
  ggplot() +
  geom_segment(aes(x = forcats::fct_reorder(Node_Name, Rank)
                   , y = 0, yend = Upper_Quantile), color = "darkgray") +
  geom_point(aes(x = forcats::fct_reorder(Node_Name, Rank) ,
                 y = Orig_Stat, color = Influential),
             size = 3) +
  # ggtitle("Result of JaB Algorithm: Strength") +
  xlab("Character") +
  ylab("Strength") +
  theme_minimal()+
  scale_color_manual(name = "Flagged as \nInfluential",
                     values = c( "TRUE" =  "#22a884", "FALSE" = "#414487"),
                     breaks = c(TRUE, FALSE),
                     labels = c("True", "False"))+
  theme(axis.text.x = element_text(colour="black",
                                   angle = 45,
                                   hjust=0.9,
                                   vjust = 1),
        axis.text.y = element_text(colour="black"))


## Eigenvector

load("data/cok-eigen.Rdata")
cok.eigen <- ret

cok.eigen %>%
  slice(1:25) %>%
  mutate(Node_Name = gsub("-", " ", Node_Name)) %>%
  ggplot() +
  geom_segment(aes(x = forcats::fct_reorder(Node_Name, Rank)
                   , y = 0, yend = Upper_Quantile), color = "darkgray") +
  geom_point(aes(x = forcats::fct_reorder(Node_Name, Rank) ,
                 y = Orig_Stat, color = Influential),
             size = 3) +
  # ggtitle("Result of JaB Algorithm: Eigenvector") +
  xlab("Character") +
  ylab("Eigenvector") +
  theme_minimal()+
  scale_color_manual(name = "Flagged as \nInfluential",
                     values = c( "TRUE" =  "#22a884", "FALSE" = "#414487"),
                     breaks = c(TRUE, FALSE),
                     labels = c("True", "False"))+
  theme(axis.text.x = element_text(colour="black",
                                   angle = 45,
                                   hjust=0.9,
                                   vjust = 1),
        axis.text.y = element_text(colour="black"))

