library('igraph')

#check the input to see if it is positive and integer number or not
ispositiveinteger <- function(k) {
  if (!is.numeric(k))
    #s must be a number
  {
    return(FALSE)
  }
  if (round(k) == k & k > 0) {
    return(TRUE)
  }
  return(FALSE)
}
#
#..............................................................................#
#ex1:
degrees_per_node = function(g)
{
  if (class(g) != 'igraph' | is_directed(g))
  {
    return ("The input object must be an undirected graph of type igraph")
  }
  adj_matrix = as_adjacency_matrix(g2)
  degrees = replicate(nrow(adj_matrix), 0)
  for (i in 1:nrow(adj_matrix))
  {
    for (j in 1:ncol(adj_matrix))
    {
      if (i == j)
      {
        degrees[i] = degrees[i] + 2 * adj_matrix[i, j] #This deals with loops
      }
      else
      {
        degrees[i] = degrees[i] + adj_matrix[i, j]
      }
    }
  }
  return(degrees)
}
#
#..............................................................................#
# ex2:
neighbor_of_node = function(g, v)
{
  if (class(g) != 'igraph' | is_directed(g))
  {
    return ("The input object must be an undirected graph of type igraph")
  }
  if (!ispositiveinteger(v))
  {
    return("k must be a positive integer number")
  }
  if (isFALSE(v %in% c(1:length(V(g))))) {
    return('Please select a valid node.')
  }
  adj_matrix = as_adjacency_matrix(g)
  neighbors = c()
  for (j in 1:ncol(adj_matrix))
  {
    if (v == j && adj_matrix[v, j] != 0)
    {
      x = replicate(2 * adj_matrix[v, j], j) #deals with loops
      neighbors = c(neighbors, x)
    }
    else
    {
      if (adj_matrix[v, j] != 0)
      {
        if (adj_matrix[v, j] == 1)
        {
          neighbors = c(neighbors, j) #appends node j to the list of neighbors
        }
        else if (adj_matrix[v, j] > 1)
          #deals with multi-edges
        {
          x = replicate(adj_matrix[v, j], j)
          neighbors = c(neighbors, x) #appends the number of multi-edges between v and j to the list of neighbors
        }
      }
    }
  }
  if (is.null(neighbors))
  {
    return(c(0))
  }
  else
  {
    return(neighbors)
  }
}
#
#..............................................................................#
# ex3:
second_neighbors <- function(g, v)
{
  if (!is.igraph(g) | is_directed(g))
  {
    return("input must be of an un-directed graph")
  }
  if (!ispositiveinteger(v)) {
    return("k must be a positive integer number")
  }
  if (isFALSE(v %in% c(1:length(V(g))))) {
    return('Please select a valid node.')
  }
  adj <- get.adjacency(g)
  lst <- c()
  for (i in 1:nrow(adj))
  {
    for (j in 1:ncol(adj))
    {
      if (adj[v, i] * adj[i, j] > 0)
      {
        lst = c(lst, j)
      }
    }
  }
  return(unique(lst))
}
#
#..............................................................................#
#ex4:
the_most_similar_pair_of_nodes <- function(g)
{
  if (!is.igraph(g) | is_directed(g))
  {
    return("input must be of an un-directed graph")
  }
  adj <- get.adjacency(g)
  lst = matrix(0, 0, 2)
  max_sim = 0
  for (i in 1:(nrow(adj) - 1))
  {
    ni = neighbors(g, i)
    for (j in (i + 1):nrow(adj))
    {
      nj = neighbors(g, j)
      uni <- union(ni, nj)
      int <- intersect(ni, nj)
      if (length(uni) > 0 & length(int) > 0)
      {
        sim = length(int) / length(uni)
        if (sim == max_sim)
        {
          lst <- rbind(lst, c(i, j))
        }
        if (sim > max_sim)
        {
          lst = c(i, j)
          max_sim = sim
        }
      }
    }
  }
  return(lst)
}