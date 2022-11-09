library(igraph)
options(warn = -1)

centerality_measure = function (rep, nv) {
  cnt = array(0, 3)
  ne = 0
  while (ne < rep) {
    vertice_size <- sample(1:nv, 1)
    
    g <- barabasi.game(vertice_size ,
                       power = 2,
                       m = 1,
                       directed = FALSE)
    if (length(E(g)) > 0 &&
        length(E(g)) <  vertice_size * (vertice_size - 1) / 2) {
      c <- complementer(g)
      
      # g_c = betweenness(g)
      # c_c = betweenness(c)
      
      # g_c = closeness(g, mode = "all")
      # c_c = closeness(c, mode = "all")
      
      g_c = degree(g)
      c_c = degree(c)
      
      # g_c = eigen_centrality(g)$vector
      # c_c = eigen_centrality(c)$vector
      
      # g_c = page_rank(g)[[1]]
      #c_c = page_rank(c)[[1]]
      
      p_value <- cor.test(g_c, c_c)[[3]]
      cor_coef <- cor.test(g_c, c_c)[[4]]
      
      
      if (!is.na(p_value) && !is.na(cor_coef)) {
        ne <- ne + 1
        if (p_value < 0.05 && cor_coef < -0.05) {
          cnt[1] <- cnt[1] + 1
        }
        if (p_value < 0.05 && cor_coef <= 0 && cor_coef >= -0.05) {
          cnt[3] <- cnt[3] + 1
        }
        
        if (p_value < 0.05 && cor_coef > 0.05) {
          cnt[2] <- cnt[2] + 1
        }
        if (p_value < 0.05 && cor_coef >= 0 && cor_coef <= 0.05) {
          cnt[3] <- cnt[3] + 1
        }
        
        if (p_value > 0.05) {
          cnt[3] <- cnt[3] + 1
        }
      }
      
    }
  }
  
  cat(cnt[1] / rep * 100 ,
      "% of graphs are negatively correlate. \n")
  cat(cnt[2] / rep * 100 ,
      "% of graphs are positively correlate.\n")
  cat(cnt[3] / rep * 100 ,
      "% of graphs are not correlate.\n")
}

centerality_measure(100, 20)

# CENTRALITY = betweenness
# 39 % of graphs are negatively correlate.
# 0 % of graphs are positively correlate.
# 61% of graphs are not correlate.

# CENTRALITY = closeness
# 100 % of graphs are negatively correlate.
# 0 % of graphs are positively correlate.
# 0 % of graphs are not correlate.

# CENTRALITY = degree
# 100 % of graphs are negatively correlate.
# 0 % of graphs are positively correlate.
# 0 % of graphs are not correlate.

# CENTRALITY = eigen_centrality
# 100 % of graphs are negatively correlate.
# 0 % of graphs are positively correlate.
# 0 % of graphs are not correlate.

# CENTRALITY = page_rank
# 100 % of graphs are negatively correlate.
# 0 % of graphs are positively correlate.
# 0 % of graphs are not correlate.
#..............................................................................#
#Ex.2
#you can extend it for directed graphs also! :)
compare_four_paths_ed_ba = function (rep, nv) {
  vertice_size <- sample(1:nv, 1)
  four_paths_er = array(0, vertice_size)
  four_paths_ba = array(0, vertice_size)
  
  for (i in 1:rep) {
    er = sample_gnp(vertice_size, 0.15, directed = FALSE)
    ba = barabasi.game(vertice_size ,
                       power = 2,
                       m = 1,
                       directed = FALSE)
    if (length(E(ba)) != length(E(er))) {
      while (length(E(ba)) != length(E(er))) {
        er = sample_gnp(vertice_size, 0.15, directed = FALSE)
      }
    }
    four_paths_er[i] = count4Paths(er)
    four_paths_ba[i] = count4Paths(ba)
  }
  
  cat("mean number of 4-paths for Barabasi-Albert model=",
      mean(four_paths_ba) ,
      "\n")
  cat("mean number of 4-paths for Erdos_Renyi model=",
      mean(four_paths_er),
      "\n")
  
  if (mean(four_paths_er) > mean(four_paths_ba)) {
    print("The number of 4-paths on Erdos-Reni graphs is bigger than on Barabasi-Albert graphs")
  }
  if (mean(four_paths_er) < mean(four_paths_ba)) {
    print(
      "The number of 4-paths on Erdos-Renyi graphs is smaller than on Barabasi-Albert graphs"
    )
  }
  if (mean(four_paths_er) == mean(four_paths_ba))  {
    print(
      "The number of 4-paths on Erdos-Renyi graphs equal to the number of 4-paths on Barabasi-Albert graphs"
    )
  }
}

count4Paths <- function(g) {
  counter <- 0
  for (i in V(g)) {
    for (j in V(g)) {
      if ((g[i, j] == 1) & (i != j)) {
        for (k in V(g)) {
          if ((g[j, k] == 1) & (j != k) & (k != i) & g[i, k] == 0) {
            for (l in V(g)) {
              if ((g[k, l] == 1) &
                  (k != l) &
                  (l != i) & g[i, l] == 0 & g[j, l] == 0) {
                counter <- counter + 1
                
              }
            }
          }
        }
      }
    }
  }
  return(counter / 2)
}

check <- function (g) {
  if (class(g) != 'igraph') {
    return ("The input object must be an undirected graph of type igraph")
  }
  
  if (is_weighted(g) || !is_simple(g) || is_directed(g)) {
    return ("The graph must be simple, not weighted and not directed")
  }
}

compare_four_paths_ed_ba(500, 25)
# Barabassi-Albert random graphs tend to have more 4-paths than Erdos-Renyi random graphs with the same amount of edges and nodes.