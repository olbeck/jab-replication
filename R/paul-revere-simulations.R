### Script for Paul revere simulation study

# Libraries
library(JaB)
library(localboot)
library(foreach)
library(parallel)
library(doParallel)
library(fossil)
library(gridExtra)

sqrt(log(254)/254)


## Set up simulation options
funcs  <- c("degree", "closeness", "betweenness", "my_eigen")
qs <- c(0.9, 0.75, 0.5, 0.25, 0.18, 0.1476, 0.1, 0.05, 0.01)
cutoffs <- c(0.85, 0.9, 0.95, 0.99)
dist_func <- c("dist_ASE", "dist_max", "dist_ICE")

opts <- expand.grid(funcs=funcs, qs=qs, cutoffs= cutoffs, dist_func = dist_func)


## Set up parallization
num.cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(num.cores)
registerDoParallel(cl)


### Run simulation
ret <- foreach(i=1:nrow(opts),
               .packages = c("JaB", "localboot", "igraph")) %dopar% {

                 source("R/bootstrap-local.R", local = FALSE)
                 source("R/ice-dist.R", local = FALSE)

                 # set distance function
                 if(opts$dist_func[i] == "dist_ASE"){
                   dist.func.use <- dist_ASE
                 }else if(opts$dist_func[i] == "dist_max"){
                   dist.func.use <- dist_max
                 }else if(opts$dist_func[i] == "dist_ICE"){
                   dist.func.use <- dist_ICE
                 }

                 jab_network(
                   paul.revere,
                   central.func.name = as.character(opts$funcs[i]),
                   central.package.name = NULL,
                   central.func.args = list(normalize = TRUE),
                   bootstrap.func.name = "bootstrap_local",
                   bootstrap.package.name = NULL,
                   bootstrap.func.args = list(quantile_n = opts$qs[i], dist.func = dist.func.use),
                   B = 3000,
                   quant = opts$cutoffs[i],
                   nodes = NULL,
                   return.boot.samples = FALSE
                 )
               }


save(opts,ret, file = "data/pr-distances.Rdata" )

###### Plotting ------------------------------
load("data/pr-distances.Rdata")

plot_ri <- function(data.name, var.name, cut.val, dist.type = "ASE"){



  keep <- which(opts$funcs == var.name & opts$cutoffs == cut.val & opts$dist_func == dist.type)
  N <- length(keep)
  ret.temp <- ret[keep]

  # order all results the same
  ret.temp <- lapply(ret.temp,
                     function(x){
                       x %>% arrange(Node_Number)
                     })


  #get rand index
  ri <- matrix(NA, ncol = N, nrow = N)
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      results1 <- as.numeric(ret.temp[[i]]$Influential)
      results2 <- as.numeric(ret.temp[[j]]$Influential)
      ri[i,j] <- round(rand.index(results1, results2), 2)
    }
  }

  #254 = gorder paul.revere
  rownames(ri) <- colnames(ri) <- paste0("q", opts$qs[keep])

  num.inf  <- sapply(ret.temp,
                     function(x){sum(x$Influential)})

  avg.cut <- sapply(ret.temp,
                    function(x){mean(x$Upper_Quantile)})


  # make plot data
  mat_df <- as.data.frame(ri[, -1])
  mat_df$Influential = num.inf
  mat_df$Cutoff = avg.cut
  mat_df$Row <- rownames(mat_df)

  mat_melted <- data.table::melt(mat_df, id.vars = "Row")
  mat_melted$value <- as.numeric(as.character(mat_melted$value))
  mat_melted_filtered <- mat_melted[mat_melted$variable != "Cutoff" & mat_melted$variable != "Influential"  , ]

  ggplot() +
    geom_tile(data = mat_melted_filtered, aes(x = variable, y = Row, fill = value), color = "white") +
    geom_text(data = mat_melted, aes(x = variable, y = Row, label = round(value, 3)), na.rm = TRUE, color = "black") +
    scale_fill_gradient(
      name = "rand index",
      low = "red",
      high = "blue",
      na.value = "transparent",
      limits = c(0, 1),
    ) +
    theme_minimal() +
    guides(fill="none")+
    scale_x_discrete(position = "top",
                     labels = c(floor(opts$qs[keep]*254)[-1], "Flag", "Cutoff")) +
    scale_y_discrete(
      labels = rev(floor(opts$qs[keep]*254))) +
    theme(axis.text.x = element_text(color = "black", face = "plain", hjust = 0, angle = 45),
          axis.text.y = element_text(colour="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("") +
    ylab("") +
    theme()
  #ggtitle(paste(data.name, "with", var.name, "centrality,", cut.val, "cutoff, and", dist.type))

}

plot.opts <-
  opts  %>%
  select(-qs) %>%
  distinct() %>%
  mutate(row = row_number())


plots <- vector(mode = "list", length = nrow(plot.opts))

for(i in 1:nrow(plot.opts)){
  plots[[i]] <- plot_ri("PR",plot.opts$funcs[i], plot.opts$cutoffs[i], plot.opts$dist_func[i])
}

# Degree
which.degree <- plot.opts$row[plot.opts$funcs == "degree"]
grid.arrange(grobs = plots[which.degree], ncol = 4, nrow = 3)

# Closeness
which.closeness <- plot.opts$row[plot.opts$funcs == "closeness"]
grid.arrange(grobs = plots[which.closeness], ncol = 4, nrow = 3)

#betweenness
which.betweenness <- plot.opts$row[plot.opts$funcs == "betweenness"]
grid.arrange(grobs = plots[which.betweenness], ncol = 4, nrow = 3)

# eigen
which.my_eigen <- plot.opts$row[plot.opts$funcs == "my_eigen"]
grid.arrange(grobs = plots[which.my_eigen], ncol = 4, nrow = 3)



