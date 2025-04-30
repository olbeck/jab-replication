### Internal functions for Local Boot
## editing output to be sparce matrix for efficent storage

# This code chuck of code is taken directly from internal functions of locaboot package
# found here https://github.com/cran/localboot
# I copied the code here for easier manipulability to work with the JaB package environment

get_dist_default_eigen <- function(A) {
  .Call('_localboot_get_dist_default_eigen', PACKAGE = 'localboot', A)
}

calculate_p_hat_matrix <- function(A, neibors_matrix) {
  .Call('_localboot_calculate_p_hat_matrix', PACKAGE = 'localboot', A, neibors_matrix)
}

sample_from_p_cpp <- function(p_hat_matrix, blist, random_matrix, no_loop) {
  .Call('_localboot_sample_from_p_cpp', PACKAGE = 'localboot', p_hat_matrix, blist, random_matrix, no_loop)
}


#' Local Bootstrap for Sparce Network Data
#'
#' This function is the \link[localboot]{localboot} function from the localboot package
#' found here https://github.com/cran/localboot.
#' The function has been modified slightly to fit into the JaB environment:
#' (1) Bootstrap matrices are stored as spare matrices for storage efficiency and
#' (2) bootstrap matrices have row and column names to know which node belongs to which
#' bootstrap samples.
#'
#'
#' @param A A square adjacency matrix of the network.
#' @param B The number of bootstrap samples to generate.
#' @param quantile_n The quantile used for neighborhood selection in some methods.
#'     If set to 0 (default), it's calculated as (log(N) / N)^0.5.
#' @param returns Specifies the type of output returned. Possible values are
#'     "boot" (default), "p_and_time", "p_and_boot", and "T".
#' @param method The method used for bootstrapping. Options are "own" and "zhu".
#' @param dist_func A function to compute the distance matrix. Default is
#'     `get_dist_default_eigen`.
#' @param kowning_u An optional known 'u' vector for distance calculation.
#' @param induced_sampling A logical indicating whether to use induced sampling.
#'     Defaults to TRUE.
#' @param weighted A logical indicating if the network is weighted. Defaults to FALSE.
#' @param getT An optional function to apply to each bootstrapped sample.
#' @param user_blist An optional user-provided bootstrap list.
#' @param fast A logical indicating if a faster, approximate method should be used.
#'     Automatically set based on network size if NULL.
#' @param ... Additional arguments passed to other methods.
#'
#' @return Depending on the `returns` argument, this function can return various types
#'     of outputs including bootstrapped networks, estimated probabilities, computation
#'     times, and statistics from the `getT` function.
#'
#'
#' @examples
#' # Example usage
#' P = generate_graphon(100, 1)
#' A = generate_network_P(P, replicate = 1, symmetric.out = TRUE)
#' result <- localboot(A = A, B = 100, returns = "boot")
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib localboot
#' @export
localboot_sparce <- function(network, B, quantile_n = 0, returns = "boot", method = "own", dist_func = dist_ASE,
                             kowning_u = NULL, induced_sampling = TRUE, weighted = FALSE, getT = NULL,
                             user_blist = NULL, fast = NULL, ...) {
  # For timing purpose
  now <- Sys.time()
  if(weighted){
    A <-as.matrix(as_adjacency_matrix(network, attr = "weight"))
  }else{
    A <-as.matrix(as_adjacency_matrix(network))
  }


  # Network size
  N <- NROW(A)

  # Set 'fast' based on 'N'
  fast <- if(is.null(fast)) N > 400 else fast

  # Additional configurations
  no_loop <- TRUE# sum(diag(A)) == 0
  max_A <- max(A)
  quantile_n <- if(quantile_n == 0) (log(N) / N)^0.5 else quantile_n

  #get dist.matrix
  if(is.null(kowning_u)){
    dist.matrix <- dist_func(A)
  }else{
    dist.matrix <- as.matrix(stats::dist(kowning_u))
  }


  # Main computation block
  if(method == "own") {

    #get neighbors
    nb_actual <- ifelse(fast==1,min(30,(ceiling(quantile_n*N))),(ceiling(quantile_n*N))) #(ceiling(quantile_n*N)
    neibors_matrix <- matrix(as.integer(0),nrow = NROW(A),ncol = nb_actual)

    for(i in (1:N)){
      neibor_index <- order(dist.matrix[i,], decreasing=FALSE)[1:ceiling(quantile_n*N)]
      if(fast==1){
        sample_fast_index = sample(1:ceiling(quantile_n*N),size = nb_actual)
        neibors_matrix[i,] <- neibor_index[sample_fast_index]
      }else{
        neibors_matrix[i,] <- neibor_index
      }
    }
    #not weighted graph
    if(weighted==FALSE){
      #estimate p for each node pair
      neibors_matrix = neibors_matrix - 1
      mode(neibors_matrix) <- "integer"
      p_hat_matrix <- calculate_p_hat_matrix(A,neibors_matrix)
    }else{
      nb_array <- array(0,c(N,N,max_A+1))
      for(a in 1:(-1+N)){
        for(b in (a):(N)){
          ab_connection <- as.vector(A[neibors_matrix[a,],neibors_matrix[b,]])
          freq_table <- (table(ab_connection)/length(ab_connection))
          full_freq_table <- sapply(0:max_A,function(x){ifelse(sum(names(freq_table)==x)>0,freq_table[names(freq_table)==x],0)})
          nb_array[a,b,] <- cumsum(full_freq_table)
          nb_array[b,a,] <- nb_array[a,b,]
        }
      }
    }
  } else if(method == "zhu") {
    # 3. quantile as logical
    kernel_mat = matrix(0,N,N)
    for (i in 1:N){
      kernel_mat[i,] = as.double(dist.matrix[i,]<stats::quantile(dist.matrix[i,],quantile_n))
    }
    # 4. L1 normalization of each row
    kernel_mat = kernel_mat/(outer(rowSums(kernel_mat),rep(1,N))+1e-10)

    # 5. Compute P
    P = kernel_mat %*% A;
    P = (P+t(P))/2;
    p_hat_matrix <- P
  }

  finish_time <- Sys.time() - now

  # Returning results based on 'returns' parameter
  if(returns == "p_and_time") {
    return(list(p_hat_matrix = p_hat_matrix, finish_time = finish_time))
  }

  # Induced sampling to generate bootstrap networks as lists
  nb_boot.list <- list()
  for(i in 1:B) {
    # Sampling computation
    if(induced_sampling){
      blist <- sample((0:(N-1)),N, replace = T) # N-1 as Cpp starts from 0
    }else{
      blist <- 0:(N-1)
    }
    if(!is.null(user_blist)){
      blist <- user_blist
    }
    blist <- sort(blist)
    if(weighted==FALSE){
      rmatrix <- matrix(stats::runif(N*N,0,1),N,N)
      g.adj.nb <- sample_from_p_cpp(p_hat_matrix,blist,rmatrix,no_loop)
      # next two lines are my addition to name the sampled nodes and store as sparce matrix
      rownames(g.adj.nb) <- colnames(g.adj.nb) <- paste0("v",blist)
      nb_boot.list[[i]]<- as(g.adj.nb, "sparseMatrix")
    }else{
      nb_array_b <- nb_array[blist+1,blist+1,]
      random.matrix <- matrix(stats::runif(N*N,0,1),N,N)
      random.matrix[lower.tri(random.matrix)] <- t(random.matrix)[lower.tri(t(random.matrix))]
      g.adj.nb <- Reduce("+",lapply((1:(max_A+1)),function(x){
        1*(random.matrix>nb_array_b[,,x])
      }))
      # next two lines are my addition to name the sampled nodes and store as sparce matrix
      rownames(g.adj.nb) <- colnames(g.adj.nb) <- paste0("v",blist)
      nb_boot.list[[i]]<- as(g.adj.nb, "sparseMatrix")

    }
  }

  # Final return based on 'returns' parameter
  if(returns == "p_and_boot") {
    return(list(p_hat_matrix = p_hat_matrix, nb_boot.list = nb_boot.list))
  } else if(returns == "T") {
    boot.Tlist <- lapply(nb_boot.list, getT)
    return(list(boot.Tlist = boot.Tlist, se = stats::sd(unlist(boot.Tlist))))
  } else {
    #return the bootstrap networks as a list
    return(nb_boot.list)
  }
}





#' Local Bootstrap
#'
#' Generate bootstrap samples of a network with the local bootstrap method described in \insertCite{zu-quin-2025;textual}{JaB}
#' from the \pkg{localboot} package.
#'
#' @details
#'
#' This is a wrapper function to generate bootstrap samples of a network with
#' the local bootstrap method described in \insertCite{zu-quin-2025;textual}{JaB}.
#' This function uses the \link[localboot:localboot]{localboot} function to
#' generate bootstrap samples, then formats them to fit in the JaB package syntax.
#'
#'
#'
#' Let \eqn{\boldsymbol{A}} be the adjacency matrix of `network` \eqn{G } with \eqn{n} nodes
#' in node set \eqn{V}.
#'
#' Let \eqn{\boldsymbol{D}} be the distance matrix for all pairs of nodes where \eqn{d_{i,j}}
#' is the distance between node \eqn{i} and node \eqn{i} according to the distance function `dist.func`.
#'
#' Construct a neighborhood for each node as \eqn{\mathcal{N}(v_i) = \{v_j \in V\ : d_{i,j} < d_i^{(s)} \}}
#' where \eqn{d_i^{(s)} } is the \eqn{s^{th}} smallest distance of \eqn{d_{i,1}, ..., d_{i, n}}.
#' This construction ensures each neighborhood always has \eqn{s = |\mathcal(N)(v_i)|} neighbors.
#'
#'
#' To generate a bootstrap adjacency matrix \eqn{\boldsymbol{A}^{(b)}}, for
#' \eqn{1 \leq i < j \leq n} independently draw
#' \deqn{
#' \begin{align}
#' A_{ij}^{(b)} &\sim \text{Uniform} \Big(\{A_{k\ell} : k \in \mathcal{N}(v_{i}^{(b)}), \ell \in \mathcal{N}(v_{j}^{(b)})\} \Big) \\
#' v_i^{(b)} &\sim \text{Uniform}(V)
#' \end{align}
#' }
#'
#' There may be cases in which \eqn{\mathcal{N}(v_{i}^{(b)})} and \eqn{\mathcal{N}(v_{j}^{(b)})}
#' are overlapping sets. In these cases, set  \eqn{\mathcal{N}(v_{i}^{(b)}) \cap \mathcal{N}(v_{j}^{(b)}) = \emptyset}.
#'
#'
#'
#' @param network An `igraph` object with \eqn{n} nodes.
#' @param B number of bootstrap samples to calculate
#' @param quantile_n Parameter for \link[localboot:localboot]{localboot} function. The quantile used for neighborhood selection in the \link[localboot:localboot]{localboot} function. Defaults to `NULL` which uses \eqn{\sqrt{log(n)/n}}.
#' @param dist.func Parameter for \link[localboot:localboot]{localboot} function. The distance function used to calculate distances between nodes in `network`. Defaults to `get_dist_default_eigen`, an internal function in the \pkg{localboot} package.
#' @param weighted Parameter for \link[localboot:localboot]{localboot} function. A logical indicating if the network is weighted. Defaults to `FALSE`.
#' @param fast Parameter for \link[localboot:localboot]{localboot} function. A logical indicating if a faster, approximate method should be used. Automatically set based on network size if NULL.
#'
#' @returns A list of length `B` where each element is a bootstrapped network as an igraph object.
#' Each bootstrapped network, \eqn{G^{(b)}} has the following vertex attributes:
#' \itemize{
#'   \item `boot.index` is the index of the node in the bootstrap sample as \eqn{vb1, vb2, ..., vbn}.
#'   \item `original.index.cpp` this is the index of the corresponding node in
#'   the original network as \eqn{v0, v1, ..., v(n-1)}.  The \link[localboot:localboot]{localboot}
#'    function uses C++ which is the reason for this index.
#'    For example if \eqn{v_1^{b)}} is the \eqn{4^{th}} node in the original `network`, the `original.index.cpp[1]` is \eqn{v3}.
#'   \item `original.index.r`this is the index of the corresponding node in
#'   the original network as \eqn{v1, v2, ..., vn}.  The \link[localboot:localboot]{localboot}
#'    function uses C++, but we want to use R's indexing from 1 to n.
#'    For example if \eqn{v_1^{b)}} is the \eqn{4^{th}} node in the original `network`, then `original.index.cpp[1]` is \eqn{v4}.
#'   \item `name` is the name of the corresponding node in the original network.
#'   For example if \eqn{v_1^{b)}} is the \eqn{4^{th}} node in the original `network`,
#'   then `name[1]` is `V(network)$name[4]`.
#' }
#'
#' @references \insertAllCited{}
#'
#'
#' @examples
#' library(JaB)
#' library(igraph)
#' library(localboot)
#' data("paul.revere")
#'
#' # generate 10 bootstrap samples
#' bootstrap_local(paul.revere, B = 10)
#'
#' # Use a new distance function
#' @export
bootstrap_local <- function(
    network,
    B = 3000,
    quantile_n = NULL,
    dist.func = dist_ASE,
    weighted = FALSE,
    fast = NULL,
    node.names = NULL){


  #check if network is igraph object
  cl <- class(network)
  if(!("igraph" %in% cl)) {stop("network must be an igraph object")}

  N <- igraph::gorder(network)

  # get node names
  if(is.null(node.names) | length(node.names) != N){
    node.names <- V(network)$name
  }

  #make adjacency matrix
  adj.mat <- as.matrix(as_adjacency_matrix(network, type = "both"))


  #get bootstrap samples
  if(is.null(quantile_n)){quantile_n <- 0}

  local_boot_res = localboot(adj.mat,B,
                             returns = "boot",
                             quantile_n = quantile_n,
                             weighted = weighted,
                             fast = fast,
                             dist_func = dist.func)

  # Change the name of the nodes and make them igraph objects
  boot.result <- vector(mode = "list", length = B)
  node.names.dat <- data.frame(name = node.names,
                               v = paste0("v", 0:(N-1)),
                               v.r = paste("v", 1:N)) # they name things from 0 to N-1 becuase they use C++


  for(i in 1:B){
    #get the index in c++
    sampled.v.boot  <- rownames(local_boot_res[[i]])
    #get the index in R
    orig.node.numbers <- as.numeric(substring(sampled.v.boot , 2, nchar(sampled.v.boot ))) + 1
    #get the name of the node in the original nework
    sampled.nodes.boot <-  node.names.dat$name[orig.node.numbers]

    #make each node in the bootstrap sample have its own name so igraph identifies them individually
    rownames(local_boot_res[[i]]) <-  paste0("vb", 1:N)
    colnames(local_boot_res[[i]]) <- paste0("vb", 1:N)

    #make the bootstrap sample an igraph object then add in node information
    boot.result[[i]] <- graph_from_adjacency_matrix(local_boot_res[[i]], mode = "undirected")
    V(boot.result[[i]])$original.index.cpp <- sampled.v.boot # c++ node index
    V(boot.result[[i]])$original.index.r <- paste0("v", orig.node.numbers) # R node index
    V(boot.result[[i]])$boot.index <-  paste0("vb", 1:N) # index in the bootstrap sample
    V(boot.result[[i]])$name <- sampled.nodes.boot # name of the sampled node from the original sample

  }

  return(boot.result)


}

## Alternate distance functions for the localboot


## ICE distance is found here https://github.com/Siva-47/ICE/blob/main/methods/ICE.R
# source("https://raw.githubusercontent.com/Siva-47/ICE/refs/heads/main/methods/ICE.R")

dist_ICE_ij <- function(Phat, i,j){
  n <- nrow(Phat)
  inner <- sapply(
    (1:n)[-c(i,j)],
    function(k){(Phat[i, k] - Phat[j,k])^2 }
  )
  return(sqrt(sum(inner)))
}

dist_ICE <- function(A, delta_0=0.5){

  n <- nrow(A)
  P.hat <- ICE(A, C_it = 1, C_est=1, P_hat_0 = NULL, delta_0)
  P.hat <- P.hat$final_P_hat

  D <- matrix(0, n, n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      D[i,j] <- dist_ICE_ij(P.hat, i, j)
      D[j,i] <- D[i,j]
    }
  }

  return(D)

}


# from Estimating network edge probabilities by neighborhood smoothing
# BYYUAN ZHANG, ELIZAVETA LEVINA, AND JI ZHU

#' Upper Bound distance
#'
#' Compute the upper bound distance defined in \insertCite{zhang-levina-zhu-2017;textual}{JaB}.
#'
#'
#' @param A An \eqn{n \times n} adjacency matrix
#'
#' @return An \eqn{n \times n} matrix \eqn{D}, where \eqn{D_{i,j}} represents the computed distance
#'         between nodes \eqn{v_i} and \eqn{v_j}.
#'
#'
#' @details
#'
#' Computes distance matrix \eqn{D} where each
#' \eqn{d_{i,j} = \max_{k\neq i,j}| \langle A_{i, \cdot} - A_{j, \cdot} , A_{k, \cdot} \rangle | /n}
#'
#' \eqn{d_{i,j}} is computed by `dist_max_ij` internal function.
#'
#' @examples
#' library(JaB)
#' library(igraph)
#' data("paul.revere")
#'
#' A <- as_adjacency_matrix(paul.revere, sparse = FALSE)
#' D <- dist_max(A)
#'
#' @references \insertAllCited{}
#'
#' @export
dist_max <- function(A){
  n <- nrow(A)
  D <- matrix(0,n,n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      D[i,j] <- dist_max_ij(A, i, j)
      D[j,i] <- D[i,j]
    }
  }
  return(D)
}


#' Internal function for Upper Bound distance d_ij
dist_max_ij <- function(A, i, j) {
  n <- nrow(A)
  diff <- A[i, ] - A[j, ]
  inner <- sapply(
    (1:n)[-c(i,j)],
    function(k){ abs(sum(diff * A[k, ]))/n}
  )
  return(max(inner))
}



## Euclidiean distance of adjacency spectal embeddings


#' Euclidean distance of adjacency spectral embeddings
#'
#' Compute the Euclidean distance of adjacency spectral embeddings
#'
#'
#' @param A An \eqn{n \times n} adjacency matrix
#' @param d dimension of the latent space embed in
#'
#' @return An \eqn{n \times n} matrix \eqn{D}, where \eqn{D_{i,j}} represents the distance
#'         between nodes \eqn{v_i} and \eqn{v_j}.
#'
#' @details
#' For adjacency matrix \eqn{\boldsymbol{A}} with \eqn{n} nodes,
#' let \eqn{\hat{\boldsymbol{\Lambda}} \in \mathbb{R}^{d \times d}}
#' be the diagonal matrix formed by the top `d` largest-magnitude eigenvalues
#' of the adjacency matrix and \eqn{\hat{\boldsymbol{U}} \in \mathbb{R}^{n \times d}}
#' be the matrix with the corresponding eigenvectors as its columns.
#' The adjacency spectral embedding of \eqn{\boldsymbol{A}} is
#' \eqn{\hat{\boldsymbol{X}} = \hat{\boldsymbol{U}}\hat{\boldsymbol{\Lambda}}^{1/2} \in \mathbb{R}^{n \times d}}.
#'
#' Then, \eqn{D_{i,j}} is the Euclidean norm of \eqn{\hat{\boldsymbol{X}}_{i, \cdot}} and \eqn{\hat{\boldsymbol{X}}_{j, \cdot}}.
#'
#' @examples
#' library(JaB)
#' library(igraph)
#' data("paul.revere")
#'
#' A <- as_adjacency_matrix(paul.revere, sparse = FALSE)
#' D <- dist_ASE(A)
#'
#' @export
dist_ASE <- function(A, d=2){

  if(nrow(A) < 400){
    e <- base::eigen(A)
    vecs <- e$vectors[, 1:d]
    vals <- e$values[1:d]
  }else{
    eis <- RSpectra::eigs_sym(A, d, which = "LA")
    vecs <- eis$vectors[, 1:d]
    vals <- eis$values[1:d]
  }

  S.half <- sqrt(diag(vals))

  ase.mat <- vecs %*% S.half

  D <- as.matrix(proxy::dist(ase.mat, method = "euclidean"))

  return(D)
}


### Function for eigenvector centrality that is in a more convienent format

my_eigen <- function(graph, normalize = TRUE){
  igraph::eigen_centrality(graph)$vector
}

### ICE Distance
# File directly coppied from https://raw.githubusercontent.com/Siva-47/ICE/refs/heads/main/methods/ICE.R

# library(Rfast)
# library(Matrix)

random_init <- function(n, size){

  "
  get random neighbors

  Inputs:
    n: number of vertexes
    size: size of neighborhood sets

  Outputs:
    S: random neighborhood sets
  "

  S <- list()
  for(i in 1:n){
    S[[i]] <- sample((1:n)[-i], size = size, replace = FALSE)
  }

  return(S)
}

neighbor <- function(P_hat, size){

  "
  get neighbors with current estimate P_hat

  Inputs:
    P_hat: the current estimate
    size: size of neighborhood sets

  Outputs:
    S: the new neighborhood sets
  "

  n <- dim(P_hat)[1]
  D <- as.matrix(Rfast::Dist(P_hat, method = "euclidean"))

  each_neighbor <- function(x){
    index <- which(x <= sort(x)[size+1])
    return(index)
  }

  S <- apply(D, 1, each_neighbor)

  temp <- list()
  if(is.matrix(S)){
    for(i in 1:n){
      temp[[i]] <- S[, i][which(S[, i] != i)]
    }
    S <- temp
  }else{
    for(i in 1:n){
      S[[i]] <- S[[i]][which(S[[i]] != i)]
    }
  }

  return(S)
}

estimate_with_neighbor <- function(A, S, quick = TRUE){

  "
  update the estimate with the current neighborhood sets S

  Inputs:
    A: adjacency matrix
    S: neighborhood sets
    quick: whether to use quick estimation

  Outputs:
    P_hat: new estimate
  "
  n <- dim(A)[1]
  P_hat <- matrix(0, n, n)

  if(quick == TRUE){
    # quick estimation proposed by (Zhang et al.,2017)
    for(i in 1:n){
      index <- S[[i]]
      P_hat[i, ] <- Rfast::colmeans(A[index, ])
    }
    P_hat <- (P_hat+t(P_hat))/2
  }else{
    # our proposed estimation
    for(i in 1:n){
      for(j in 1:n){
        index_i <- S[[i]]
        index_j <- S[[j]]
        overlap <- intersect(index_i, index_j)
        P_hat[i, j] <- sum(A[index_i, index_j])/(length(index_i)^2-length(overlap))
      }
    }
  }

  return(P_hat)
}


ICE <- function(A, C_it, C_est, P_hat_0 = NULL, delta_0, rounds = NULL){

  "
  apply ICE with two-stage strategy to get an estimate

  Inputs:
    A: adjacency matrix
    C_it: C used during iterations
    C_est: C used for final estimation
    P_hat_0: initial estimate
    delta_0: the metric to stop the iterations
    rounds: the rounds of the iterations

  Outputs:
    P_hat_list: estimate in each iteration
    final_P_hat: final estimate
    final_S: final neighborhood sets
  "

  n <- dim(A)[1]
  size_it <- round(C_it*sqrt(n*log(n)))
  size_est <- round(C_est*sqrt(n*log(n)))

  # use random initial value if not provided
  if(is.null(P_hat_0)){
    S <- random_init(n, size = size_it)
    P_hat_0 <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
  }

  delta_P <- Inf
  P_hat_list <- list()
  P_hat_list[[1]] <- P_hat_0

  if(!is.null(rounds)){
    # fix the rounds
    for(m in 1:rounds){
      S <- neighbor(P_hat = P_hat_list[[m]], size = size_it)
      P_hat_list[[m+1]] <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
      delta_P <- norm(P_hat_list[[m+1]]-P_hat_list[[m]], 'F')/norm(P_hat_list[[m]], 'F')
      print(paste('round', m, 'delta_P =', delta_P))
    }
  }else{
    # stop the iterations according to delta_P
    m <- 0
    while(delta_P > delta_0 & m<1000){
      S <- neighbor(P_hat = P_hat_list[[m+1]], size = size_it)
      P_hat_list[[m+2]] <- estimate_with_neighbor(A = A, S = S, quick = TRUE)
      delta_P <- norm(P_hat_list[[m+2]]-P_hat_list[[m+1]], 'F')/norm(P_hat_list[[m+1]], 'F')
      m <- m+1
      print(paste('round', m, 'delta_P =', delta_P))
    }
  }

  final_S <- neighbor(P_hat = P_hat_list[[m+1]], size = size_est)
  final_P_hat <- estimate_with_neighbor(A = A, S = final_S, quick = TRUE)

  return(list(P_hat_list = P_hat_list, final_P_hat = final_P_hat, final_S = final_S))
}

validation_ICE <- function(A, p = 0.9, can_it, can_est){

  "
  selecting tuning parameters for ICE via network cross-validation

  Inputs:
    A: adjacency matrix
    p: proportion of the edges in the training set
    can_it: candidates of C used during iterations
    can_est: candidates of C used for final estimation

  Outputs:
    loss_: the log-likelihood loss of all the combinations
    rmse_: the RMSE of all the combinations
  "

  n <- dim(A)[1]
  rank_ <- rankMatrix(A)

  # random split of training set and validation set
  is_in <- rbinom(n*n, 1, p)
  in_matrix <- matrix(is_in, n, n)
  in_matrix[lower.tri(in_matrix)] <- 0
  in_matrix <- in_matrix + t(in_matrix)
  diag(in_matrix) <- 0

  # generate adjacency matrix for the training set
  A_train <- A*in_matrix
  svd_ <- svd(A_train / p)
  A_hat <- svd_$u[, 1:rank_]%*%diag(svd_$d[1:rank_])%*%t(svd_$v[, 1:rank_])

  # get the label of the validation set
  A_val <- list()
  label_val <- c()
  count <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if(in_matrix[i, j] == 0){
        A_val[[count]] <- c(i, j)
        label_val <- c(label_val, A[i, j])
        count <- count+1
      }
    }
  }

  num_it <- length(can_it)
  num_est <- length(can_est)

  loss_ <- rmse_ <- matrix(0, num_it, num_est)

  # evaluate the performance of different combinations on the validation set
  for(i in 1:num_it){
    for(j in 1:num_est){
      print(paste('C_it =', can_it[i], ';', 'C_est =', can_est[j]))
      ice_res <- ICE(A = A_hat, C_it = can_it[i], C_est = can_est[j], P_hat_0 = NULL, rounds = 10)
      phat <- ice_res$final_P_hat
      loss <- 0
      sse <- rep(0, length(label_val))
      label_pre <- rep(0, length(label_val))
      for(l in 1:length(label_val)){
        idx <- A_val[[l]][1]
        jdx <- A_val[[l]][2]
        label_pre[l] <- min(1-10^-8, max(10^-8, phat[idx, jdx]))
        loss <- loss-label_val[l]*log(label_pre[l])-(1-label_val[l])*log(1-label_pre[l])
        sse[l] <- (label_pre[l] - label_val[l])^2
      }
      loss_[i, j] = loss / length(label_val)
      rmse_[i, j] = sqrt(mean(sse))
    }
  }

  return(list(loss_ = loss_, rmse_ = rmse_))
}
