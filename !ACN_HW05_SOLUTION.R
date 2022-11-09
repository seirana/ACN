library(dequer)
library(igraph)
library(MASS)

#make erdos-renyi graph in two different ways
ER1 = sample_gnp(5, p = 0.8, directed = F)
plot(ER1)
ER2 <- erdos.renyi.game(5, 0.8, type = "gnp")
plot(ER2)

#make barabasi graph in two different ways 
BA1 = sample_pa(5,
                power = 1,
                m = 1,
                directed = F)
plot(BA1)
BA2 <-  barabasi.game(5, directed = FALSE)
plot(BA2)

#Ex.1
queue_empty <- function(q){
  if (length(q) == 0)
    s <- TRUE
  else
    s <- FALSE
  return(s)
}

bfs <- function(g, v){ 
  q <- queue()
  num_nodes <- length(V(g))
  path <- rep(0, num_nodes)
  dist <- rep(Inf, num_nodes)
  mark <- rep(0, num_nodes)
  mark[v] <- 1
  dist[v] <- 0
  path[v] <- 1
  pushback(q, v)
  while(queue_empty(q) == F)
  {
    u <- pop(q)
    adj <- (which(g[u,]!=0))  
    for(w in adj)
    { 
      
      if (dist[w] > dist[u]+1){
        dist[w]=dist[u]+1
        path[w]=path[u]
        mark[w]==1
        pushback(q,w)
      }else if(dist[w]==dist[u]+1){
        path[w]=path[u]+path[w]
      }
    }
  }
  b=list("path"=path, "dist"=dist)
  return(b)
}

short_path_via_node <- function(x){
  n=length(V(x))
  dist=matrix(0, nrow = n, ncol = n)
  path=matrix(0, nrow = n, ncol = n)
  a=rep(0,n)
  for (i in 1:n){
    b=bfs(x,i)
    dist[i,]=b$dist   
    path[i,]=b$path   
  }
  tmp <- c()
  for(i in 1:n){
    for (j in 1:(n-1)){
      for (k in (j+1):n){   
        if(i!=j && i!=k && dist[j,k]==dist[j,i]+dist[i,k]){
          
          a[i]=a[i]+path[j,i]*path[i,k] 
          tmp <- c(tmp, j,k)
        }
        else{a[i]=a[i]+0}
      }
    }
  }
  return(a)
}

ecc <- function(g) {
  distance <- floyd(g)
  print(distance)
  ecc <- c()
  for (i in 1:nrow(distance)) {
    ecc <- c(ecc, max(distance[i, ]))
  }
  return (ecc)
}

corr <- function(g) {
  sh_path <- short_path_via_node(g)
  eccen <- eccentricity(g)
  #eccen <- ecc(g)
  stats <- cor.test(sh_path, eccen)
  if (any(is.na(c(stats$p.value, stats$estimate)))) {
    return("Correlation calculation not possible")
  }
  if (stats$p.value > 0.05) {
    cat(
      'No significant linear correlation between shortest path and eccentricity. \n P-value =',
      stats$p.value,
      'Correlation =',
      stats$estimate
    )
  }
  else {
    if (stats$estimate < -0.05) {
      cat(
        'There is a significant negative linear correlation between shortest path and eccentricity. \n P-value =',
        stats$p.value,
        'Correlation =',
        stats$estimate
      )
    }
    if (stats$estimate > 0.05) {
      cat(
        'There is a significant positive linear correlation between shortest path and eccentricity. \n P-value =',
        stats$p.value,
        'Correlation =',
        stats$estimate
      )
    }
    if (stats$estimate >= -0.05 && stats$estimate <= 0.05){
      cat(
        'No significant linear correlation between shortest path and eccentricity. \n P-value =',
        stats$p.value,
        'Correlation =',
        stats$estimate
      )
    }
  }
}
corr(g)
# stats$estimate must be significant greater than zero for positive correlation or significant smaller than zero for negative correlation
# stats$p.value < alpha, alpha can be 0.05, 0.01 0r 0.005
# you have asked to write a function for corr function, so you could use "eccen <- eccentricity(g)" in line 97
# function ecc(g)(lines 85 to 93), equals to eccentricity(g), so you can replace "eccen <- eccentricity(g)" with "eccen <- ecc(g)" in line 98
#..............................................................................#
#Ex.2
floyd <- function(g) {
  0
  D <- as.matrix(g[])
  D[which(D == 0)] <- 100000
  for (i in 1:nrow(D)) {
    D[i, i] <- 0
  }
  for (k in 1:nrow(D)) {
    for (i in 1:nrow(D)) {
      for (j in 1:nrow(D)) {
        if (D[i, j] > D[i, k] + D[k, j]) {
          D[i, j] <- D[i, k] + D[k, j]
        }
      }
    }
  }
  return(D)
}

close_centrality <- function (g) {
  D <- floyd(g)
  cc_matrix <- array(0, length(V(g)))
  total <- colSums(D)
  for (i in 1:length(V(g))) {
    cc_matrix[i] <- 1 / total[i]
  }
  return(cc_matrix)
}

close_centrality(g)
#https://en.wikipedia.org/wiki/Closeness_centrality
#..............................................................................#
#Ex.3
pagerank <- function(g, alpha = 0.85, k = 1000) {
  n = length(V(g))
  x = rep(1 / n, n)
  E = matrix(1, n, n)
  D_out = matrix(0, n, n)
  D_out = diag(degree(g, mode = "out"))
  A = as.matrix(as_adj(g))
  P = ginv(D_out) %*% A
  P_alpha = alpha * P + 1 / n * (1 - alpha) * E
  
  while (k > 0) {
    outs = apply(X = P_alpha,
                 MARGIN = 1,
                 FUN = sum)
    D_out = diag(outs, n)
    P_alpha = ginv(D_out) %*% P_alpha
    P_alpha = P_alpha %*% P_alpha
    k = k - 1
  }
  return(P_alpha[1,])
}

pagerank(g1)