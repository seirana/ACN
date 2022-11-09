library('igraph')
library('dequer')
#ex1:
check <- function (g) {
  if (!require("igraph", quietly = TRUE)) {
    return("Package 'igraph' required but not installed.")
  }
  
  if (!require("deque", quietly = TRUE)) {
    return("Package 'deque' required but not installed.")
  }

  if (class(g) != 'igraph') {
    return ("The input object must be an undirected graph of type igraph")
  }
  
  if (is_weighted(g) || !is_simple(g)) {
    return ("The graph must be simple and not weighted")
  }
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
  check(g)
  if (is_directed(g)) {
    return("The graph must be undirected")
  }
  res = (3 * count_triangles(g)) / count_two_paths(g)
  return(res)
}
#..............................................................................#
#ex2:
check2 <- function(g, s, t) {
  if (isFALSE(s %in% c(1:length(V(g))))) {
    return('Please select a valid node.')
  }
  
  if (isFALSE(t %in% c(1:length(V(g))))) {
    return('Please select a valid node.')
  }
}

queue_empty <- function(q) {
  if (length(q) == 0)
    s <- TRUE
  else
    s <- FALSE
  return(s)
}

numberOfShortestPaths <- function(g, s, t) {
  check(g)
  check2(g, s, t)
  q <- queue()
  num_nodes <- length(V(g))
  path <- rep(0, num_nodes)
  dist <- rep(Inf, num_nodes)
  mark <- rep(0, num_nodes)
  mark[s] <- 1
  dist[s] <- 0
  path[s] <- 1
  pushback(q, s)
  
  while (queue_empty(q) == F)
  {
    u <- pop(q)
    adj <- (which(g[u,] != 0))
    for (w in adj) {
      if (dist[w] > dist[u] + 1) {
        dist[w] = dist[u] + 1
        path[w] = path[u]
      } else if (dist[w] == dist[u] + 1) {
        path[w] = path[u] + path[w]
      }
      if (mark[w] != 1) {
        mark[w] = 1
        pushback(q, w)
      }
    }
  }
  return(path[t])
}