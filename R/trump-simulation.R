### Script to replicate TrumpWorld simulation


### Load libraries and data -------------------
library(JaB)
library(localboot)
library(Matrix)
library(igraph)
library(tictoc)
data("trump.world")


source("R/local-boot-functions.R", local = FALSE)
source("R/ice-dist.R", local = FALSE)



### Generate bootstrap samples ------------------------------
# Done in ROAR
boot.samps <- localboot_sparce(trump.world, 3000, dist_func = dist_ASE, quantile_n = 0.05436)
save(list = boot.samps, file =paste0("data/trump-boot-samps.Rdata"))


### Jackknife after part ----------------------------

## General parameters
network <- trump.world
bootstrap.func.name = "localboot"
bootstrap.package.name = NULL
bootstrap.func.args = NULL
B = 3000
nodes = NULL
return.boot.samples = FALSE

# load boot samples and give nodes the correct names
load("data/trump-boot-samps.Rdata")

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

## Betweenness ------------------------------
central.func.name = "betweenness"
central.package.name = NULL
central.func.args = list(normalized = TRUE)
quant = 0.98


# Centrality
orig.stat <- get_centrality(trump.world, central.func.name, central.package.name , central.func.args)

obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


## Jack after
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)

#centrality_storage
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


save(ret, file = "data/trump-betweenness.Rdata")



## Degree ------------------------------
central.func.name = "degree"
central.package.name = NULL
central.func.args = list(normalized = TRUE)
quant = 0.98


# Centrality
orig.stat <- get_centrality(trump.world, central.func.name, central.package.name , central.func.args)

obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


## Jack after
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)

#centrality_storage
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


save(ret, file = "data/trump-degree.Rdata")




## Eigen ------------------------------
central.func.name = "my_eigen"
central.package.name = NULL
central.func.args = list(normalized = TRUE)
quant = 0.96


# Centrality
orig.stat <- get_centrality(trump.world, central.func.name, central.package.name , central.func.args)

obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


## Jack after
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)

#centrality_storage
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


save(ret, file = "data/trump-eigen.Rdata")



### Plot results ------------------------------

### Load in the results
## all used local boot, B=3000, q=sqrt(logn/n)


#betweenness
load("trump-betweenness.Rdata")
ret -> JaB_trump_between
sum(JaB_trump_between$Influential)

# degree
load("trump-degree.Rdata")
ret -> JaB_trump_degree
sum(JaB_trump_degree$Influential)

# # eigen
load("trump-eigen.Rdata")
ret -> JaB_trump_eigen
sum(JaB_trump_eigen$Influential)




## Make a table of all Results
all_results <- rbind( cbind(JaB_trump_degree, stat = "degree"),
                      cbind(JaB_trump_between, stat = "between"),
                      cbind(JaB_trump_eigen, stat = "eigen")) %>%
  filter(Node_Name != "DONALD J. TRUMP")


#plot relationship among all three statistics
all_results %>%
  select(Node_Name, Orig_Stat, stat) %>%
  pivot_wider(names_from = stat, values_from = Orig_Stat)  %>%
  ggplot(aes(color = (eigen), x = between, y = log(degree))) +
  geom_point() +
  theme_minimal() +
  scale_color_viridis(name = "Eigenvector Centrality") +
  xlab("Betweenness Centrality") +
  ylab("log(Degree Centrality)") +
  ggtitle("Centrality Measures for All Nodes")

# correlations
all_results %>%
  select(Node_Name, Rank, stat) %>%
  pivot_wider(names_from = stat, values_from = Rank)  %>%
  select(-Node_Name) %>%
  cor()



influence_pivot <-
  all_results %>%
  select(Node_Name, stat, Influential) %>%
  pivot_wider(names_from = stat, values_from = Influential) %>%
  mutate(and = degree & between & eigen ) %>%
  mutate(or = degree | between | eigen)

union_names <- na.omit(influence_pivot$Node_Name[influence_pivot$or])
intersect_names <- na.omit(influence_pivot$Node_Name[influence_pivot$and])


## Intersection results
intersect_results <-
  all_results %>%
  filter(Node_Name %in% intersect_names) %>%
  select(Node_Name, stat, Orig_Stat) %>%
  pivot_wider(names_from = stat, values_from = Orig_Stat) %>%
  arrange(Node_Name)

cool.names <- intersect_names[c(1, 2, 3, 4, 5, 7, 10, 14, 22, 15)]


#plot results
intersect_results %>%
  mutate(name = ifelse(Node_Name %in% cool.names, Node_Name, NA)) %>%
  ggplot(aes(color = log(between), x = eigen, y = degree)) +

  #ggplot(aes(color = log(degree), x = between, y = eigen)) +
  geom_point() +
  theme_minimal() +
  scale_color_viridis(name = "log(Betweenness)",
                      breaks = log(c(0.01, 0.10, 0.025, 0.05)),
                      labels = c(0.01, 0.10, 0.025, 0.05)) +
  # breaks = c(-5, -4, -3),
  # labels = round(exp(c(-5, -4, -3)),4)) +
  #ylim(c(0.03, 0.08))+
  geom_label_repel( aes(label = name),
                    size = 3,
                    box.padding = 1,
                    min.segment.length	=0,
                    color = "black",
                    force = 10) +
  xlab("Eigenvector Centrality") +
  ylab("Degree Centrality") +
  ggtitle("Centrality Measures for Influential Nodes")

#intersection results
intersect_results  %>%
  select(-Node_Name) %>%
  cor()





## union results
union_results <-
  all_results %>%
  filter(Node_Name %in% union_names) %>%
  select(Node_Name, stat, Orig_Stat) %>%
  pivot_wider(names_from = stat, values_from = Orig_Stat) %>%
  arrange(Node_Name)

# cool.names <- sample(union_results$Node_Name, 5)


cool.names <- c( intersect_names[c(1, 2, 3, 4, 5, 7, 10, 14, 22, 15)],
                 "THRIVE CAPITAL", "KUSHNER COMPANIES", "MICHAEL D. COHEN",
                 "BAYROCK GROUP", "IVANKA TRUMP", "JASON GREENBLATT",
                 "PAUL MANAFORT", "JEFF SESSIONS", "STEVE BANNON")

#plot results
x = c(0.05, 0.15, 0.3)
union_results %>%
  mutate(name = ifelse(Node_Name %in% cool.names, Node_Name, NA)) %>%
  ggplot(aes(color = log(between+0.001, base = 10), x = eigen, y = degree)) +
  geom_point() +
  theme_minimal() +
  scale_color_viridis(name = "Betweenness\nCentrality",
                      breaks = log(x + 0.001),
                      labels = x) +
  #ylim(c(0,0.06))+
  #xlim(c(0, 0.08))+
  geom_label_repel( aes(label = name),
                    size = 2,
                    box.padding = 1,
                    min.segment.length	=0,
                    color = "black",
                    force = 10) +
  ylab("Degree Centrality") +
  xlab("Eigenvector Centrality") +
  # ggtitle("Centrality Measures for Influential Nodes")+
  # labs(subtitle = "Union Nodes")+
  scale_y_continuous(transform = "log",
                     breaks = c( 0.001,0.005,0.01, 0.02, 0.03,0.04))


# tiff("test.tiff", units="in", width=5, height=5, res=300)
#
# ggsave("../trump-union-central.png",
#        dpi = "print",
#        height = 6,
#        width = 8,
#        units = "in")

#union results
union_results  %>%
  select(-Node_Name) %>%
  cor()


################################
### Greenblatt
###############################

## General params
network <- trump.world
bootstrap.func.name = "localboot"
bootstrap.package.name = NULL
bootstrap.func.args = NULL
B = 3000
nodes = NULL
return.boot.samples = FALSE

ids <- which(V(network)$name %in% c("JASON D. GREENBLATT" ))


# load boot samples and give nodes the correct names
load("data/trump-boot-samps.Rdata")

#node list in each bootstrap sample
m <- length(ids)
node.list <- lapply(boot.result, JaB::get_nodes)


#covert from v_i's to names
get_names <- function(x){
  y <- substr(x, 2, nchar(x))
  y <- as.numeric(y) + 1
  return(nodes[y])
}

node.list <- lapply(node.list, get_names)


# betweenness ------------------------
central.func.name = "betweenness"
central.package.name = "igraph"
central.func.args = list(normalized = TRUE)
quant = 0.98


## Centrality
orig.stat <- JaB::get_centrality(trump.world,
                            central.func.name,
                            central.package.name ,
                            central.func.args)

obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


## Get the simulated null distribution
m=1
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)

centrality_storage <- list()
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
  centrality_storage[[counter]] <- centrality_vector

  #Get Quantiles
  jack.result[counter] <- stats::quantile(centrality_vector, quant, na.rm = T)


}



idx <- 1
hist( centrality_storage[[idx]], freq = F, xlim = c(0, 0.01), breaks = 100)
abline(v = orig.stat[ids[idx]], col = "red", lwd = 4)
abline(v = jack.result[idx], col = "black", lwd = 4)

orig.stat[ids[idx]]
jack.result[idx]


# degree ------------------------
central.func.name = "degree"
central.package.name = "igraph"
central.func.args = list(normalized = TRUE)
quant = 0.98


## Centrality
orig.stat <- JaB::get_centrality(trump.world,
                                 central.func.name,
                                 central.package.name ,
                                 central.func.args)

obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


## Get the simulated null distribution
m=1
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)

centrality_storage <- list()
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
  centrality_storage[[counter]] <- centrality_vector

  #Get Quantiles
  jack.result[counter] <- stats::quantile(centrality_vector, quant, na.rm = T)


}



idx <- 1
hist( centrality_storage[[idx]], freq = F, xlim = c(0, 0.004), breaks = 50)
abline(v = orig.stat[ids[idx]], col = "red", lwd = 4)
abline(v = jack.result[idx], col = "black", lwd = 4)

orig.stat[ids[idx]]
jack.result[idx]



# eigen ------------------------
central.func.name = "my_eigen"
central.package.name = NULL
central.func.args = NULL
quant = 0.96


## Centrality
orig.stat <- JaB::get_centrality(trump.world,
                                 central.func.name,
                                 central.package.name ,
                                 central.func.args)

obj.type <- detect_type(boot.result)
central.result <-
  get_bootstrap_centrality(boot.result = boot.result,
                           func.name = central.func.name,
                           package.name = central.package.name,
                           func.args = central.func.args)


## Get the simulated null distribution
m=1
number.boot.samps <- rep(NA, m)
jack.result <- rep(NA, m)
can.jackknife <- rep(TRUE,m)

centrality_storage <- list()
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
  centrality_storage[[counter]] <- centrality_vector

  #Get Quantiles
  jack.result[counter] <- stats::quantile(centrality_vector, quant, na.rm = T)


}



idx <- 1
hist( centrality_storage[[idx]], freq = F, xlim = c(0, 0.2), breaks = 100)
abline(v = orig.stat[ids[idx]], col = "red", lwd = 4)
abline(v = jack.result[idx], col = "black", lwd = 4)

orig.stat[ids[idx]]
jack.result[idx]
