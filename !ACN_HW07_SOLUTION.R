library('igraph')
#random.graph.game(15, p.or.m = 0.2, directed = FALSE) # you can use this function to generate random functions
g <-
  make_graph(c(1, 2, 1, 4, 1, 5, 2, 4, 3, 4, 3, 5), directed = FALSE)
plot(g)

#Ex.1:
#This alg. must work on simple, multi-edges, multi-loops, and directed graphs
# It is better to generate random graphs by the same node numbers of the main graph, 
# but here we produce random graphs with a same graph degree sequence
assortativity.func <- function(g) {
  adj <- as_adjacency_matrix(g)
  adj <- as.matrix(adj)
  
  degrees = degree(g)
  
  x <-
    rep(0, sum(degrees) / 2)
  y <- rep(0, sum(degrees) / 2)
  
  c <- 0
  for (i in 1:(length(V(g)) - 1)) {
    for (j in i:length(V(g))) {
      if (adj[i, j] > 0) {
        c <- c + 1
        
        x[c] <- degrees[i]
        y[c] <- degrees[j]
      }
    }
  }
  
  ave.x <- mean(x)
  ave.y <- mean(y)
  
  x_minus_ave.x <- rep(0, length(x))
  y_minus_ave.y <- rep(0, length(y))
  
  x_minus_ave.x <- x - ave.x
  y_minus_ave.y <- y - ave.y
  
  x_minus_ave.x_mult_y_minus_ave.y <- x_minus_ave.x * y_minus_ave.y
  s <- sum(x_minus_ave.x_mult_y_minus_ave.y)
  
  sum_pow_x_minus_ave <- 0
  sum_pow_y_minus_ave <- 0
  for (i in 1:length(x)) {
    sum_pow_x_minus_ave <- sum_pow_x_minus_ave + x_minus_ave.x[i] ^ 2
    sum_pow_y_minus_ave <-
      sum_pow_y_minus_ave + y_minus_ave.y[i] ^ 2
  }
  
  t <- sqrt(sum_pow_x_minus_ave) * sqrt(sum_pow_y_minus_ave)
  
  r <- s / t
  return(r)
}

#applying assortativity.func on "rep" different graphs, and calculate p_value
stat_sig_assort <- function (g, rep) {
  if (class(g) != 'igraph') {
    return("please use an igraph format")
  }
  
  assort_obs <- assortativity.func(g)
  degSeq <- sort(degree(g), decreasing = TRUE)
  
  n_more_extreme <- 0
  for (i in 1:rep) {
    rnd_g <- configuration(degSeq)
    assort_perm <- assortativity.func(rnd_g)
    
    if (is.nan(assort_perm) && is.nan(assort_obs)) {
      if (assort_per >= assort_obs) {
        n_more_extreme <- n_more_extreme + 1
      }
    }
  }
  print(paste('P-value =', (n_more_extreme + 1) / (rep + 1)))
}

#produce random graph with a same degree sequence
configuration <- function(deg_seq) {
  stubs <- c()
  g <- make_empty_graph(length(deg_seq), directed = FALSE)
  for (i in 1:length(deg_seq)) {
    stubs <- c(stubs, rep(i, deg_seq[i]))
  }
  used <- rep(0, length(stubs))
  for (i in 1:(length(stubs) / 2)) {
    s <- sample(which(used == 0), 2)
    used[s] <- 1
    g <- add.edges(g, c(stubs[s[1]], stubs[s[2]]))
  }
  return(g)
}

stat_sig_assort(g, 2 ^ length(V(g)))
#..............................................................................#
#Ex.2:
#calculate P_value
stat_sign_Adja <- function(g, rep, k1, k2) {
  if (class(g) != 'igraph') {
    return("please use an igraph format")
  }
  #you can check if k1 and k2 < length(V(g)) for simple graphs, if you code it for simple graphs
  node_cnt_main_g <- adj_nodes_cnt(g, k1, k2)
  c <- 0
  for (i in 1:rep) {
    samp_graph <- rand_graph(length(V(g)))
    node_cnt_rand_g <-  adj_nodes_cnt(samp_graph, k1, k2)
    if (node_cnt_rand_g >= node_cnt_main_g) {
      c <- c + 1
    }
  }
  print(paste('P-value =', (c + 1) / (rep + 1)))
}

#count the number of nodes with degrees k1 and k2, that are neighbors
adj_nodes_cnt <- function (g, k1, k2) {
  degrees <- degree(g)
  k1_idx <- which(degrees == k1)
  k2_idx <- which(degrees == k2)
  
  cnt <- 0
  for (i in k1_idx) {
    for (j in k2_idx) {
      if (are_adjacent(g, i, j)) {
        cnt <- cnt + 1
      }
    }
  }
  return(cnt)
}

#generates random graph with a size of length(V(main graph))
#you can use it for Ex.1 also!
rand_graph <- function(d) {
  a <- sample(1:2 ^ d, 1)
  samp_deg_seq <- sample(1:d, a, replace = TRUE)
  if (sum(samp_deg_seq %% 2) == 1) {
    samp_deg_seq[1] <- samp_deg_seq[1] + 1
  }
  rand_g <- configuration(samp_deg_seq) #call configuration func.
  return(rand_g)
}

stat_sign_Adja(g, 300, 3, 2)
