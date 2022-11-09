library(Hmisc)
library(igraph)
library(dequer)
library(matlib)
#ex.1:
Bonacich_centrality <-
  function(graph, alpha = 1, beta = 1) {
    #beta could be  = eigenvalue or inverse_eigenvalue
    #eigen_vector <- eigen(g[])
    #eigenvalue <- eigen_vector$values[1]
    #inverse_eigenvalue <- 1 / eigenvalue
    check(graph)
    
    adj <- as.matrix(graph[])
    I <- diag(nrow = nrow(adj), ncol = ncol(adj))
    one <- matrix(1, nrow = nrow(adj), ncol = 1)
    M <- I - beta * adj
    if (det(M) == 0) {
      return("The matrix (I - beta * adj) is not invertible")
    } else{
      bc <-
        (alpha * inv(M)) %*% adj %*% one #inv(m) = solve(m) = inverse(m)
      ec <-
        eigen(adj) #you can use eigen_centrality(g) instead of eigen(adj)
     if (all.is.numeric(ec$value)){
      ans = corr(bc, ec$value)
     }
      else{
        return('ec$value has some non-real values! Correlation calculation is not possible')
      }
    }
  }

corr <- function(bc, ec) {
  stats <- cor.test(bc, ec, method = "pearson")
  if (any(is.na(c(stats$p.value, stats$estimate)))) {
    return("Correlation calculation is not possible")
  }
  if (stats$p.value > 0.05) {
    cat(
      'No significant linear correlation between Bonacich and eigenvalue centrality P-value =',
      stats$p.value,
      'Correlation =',
      stats$estimate
    )
  }
  else {
    if (stats$estimate < -0.05) {
      print(stats$estimate)
      cat(
        'There is a significant negative linear correlation between Bonacich and eigenvalue centrality P-value =',
        stats$p.value,
        'Correlation =',
        stats$estimate
      )
    }
    if (stats$estimate > 0.05) {
      cat(
        'There is a significant positive linear correlation between Bonacich and eigenvalue centrality P-value =',
        stats$p.value,
        'Correlation =',
        stats$estimate
      )
    }
    if (stats$estimate >= -0.05 && stats$estimate <= 0.05) {
      cat(
        'No significant linear correlation between Bonacich and eigenvalue centrality P-value =',
        stats$p.value,
        'Correlation =',
        stats$estimate
      )
    }
  }
}

check <- function (g) {
  if (!require("igraph", quietly = TRUE)) {
    return("Package 'igraph' required but not installed.")
  }
  
  if (!require("dequer", quietly = TRUE)) {
    return("Package 'dequer' required but not installed.")
  }
  
  if (class(g) != 'igraph') {
    return ("The input object must be an undirected graph of type igraph")
  }
  
  if (is_weighted(g) || !is_simple(g)) {
    return ("The graph must be simple and not weighted")
  }
}

Bonacich_centrality(g)
#..............................................................................#
#ex.2:
tran_g_vs_ed = function(g, r) {
  check(g)
  if (is_directed(g)) {
    return("The graph must be undirected")
  }
  n <- length(V(g))
  m <- length(E(g))
  trans_g <- transitivity_graph(g)
  sum = 0
  for (i in 1:r) {
    ed <- erdos.renyi.game(n,
                           m,
                           type = c("gnm"),
                           directed = FALSE,
                           loops = FALSE)
    trans <- transitivity_graph(ed)
    sum <- sum + trans
  }
  trans_ed <- mean(sum)
  if (trans_g > trans_ed) {
    print(
      "The transitivity of the given graph is bigger than the average of 100 realizations of Erdos-Renyi graphs"
    )
  }
  if (trans_g < trans_ed) {
    print(
      "The transitivity of the given graph is smaller than the average of 100 realizations of Erdos-Renyi graphs"
    )
  }
  if (trans_g == trans_ed) {
    print(
      "The transitivity of the given graph equals to the average of 100 realizations of Erdos-Renyi graphs"
    )
  }
  return(list(trans_g, trans_ed))
}

count_triangles = function (g) {
  sum = 0
  for (i in V(g)) {
    for (j in V(g)) {
      if ((g[i, j] == 1) & (i != j)) {
        for (k in V(g)) {
          if ((g[j, k] == 1) & (g[k, i] == 1)) {
            if ((j != k) & (k != i)) {
              sum = sum + 1
            }
          }
        }
      }
    }
  }
  return(sum / 6)
}

count_two_paths = function (g) {
  sum = 0
  for (i in V(g)) {
    for (j in V(g)) {
      if ((g[i, j] == 1) & (i != j)) {
        for (k in V(g)) {
          if ((g[j, k] == 1) & (j != k) & (k != i)) {
            sum = sum + 1
          }
        }
      }
    }
  }
  return(sum / 2)
}

transitivity_graph <- function(g) {
  res = (3 * count_triangles(g)) / count_two_paths(g)
  return(res)
}

tran_g_vs_ed(g, 100)