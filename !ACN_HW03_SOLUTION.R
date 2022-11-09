library('dequer')
library('igraph')
#..............................................................................#
#ex.1
bfs2 = function(g, v){
  edges = c()
  Q = queue()
  num_nodes = length(V(g))
  mark = rep(0, num_nodes)
  mark[v] = 1
  pushback(Q, v)
  u = pop(Q)
  while (!is.null(u)){
    for (neighbor in neighbors(g, u)){
      if (mark[neighbor] == 0){
        mark[neighbor] = 1
        edges = c(edges, u, neighbor)
        pushback(Q, neighbor)
      }
    }
    u = pop(Q)
  }
  return(edges)
}

shortestPathLength = function(g, v1, v2) {
  # you can check if g is graph (we have coded before)
  # you can check if v1 & v2 are in the node-list (we have coded before)
  # you can check if the graph is not directed, not weighted, ...
  if (v1 == v2) {
    return(0)
  }
  
  bfsTreeEdges = bfs2(g, v1) # bfs2() returns the edges of the bfs tree
  
  if (length(bfsTreeEdges) == 0){
    # then there is no shortest path. Return infinity.
    return(Inf)
  }
  
  dim(bfsTreeEdges) = c(2, length(bfsTreeEdges)/2)
  bfsTreeEdges = t(bfsTreeEdges)

  if (!(v2 %in% bfsTreeEdges[,2])){
    # then there is no shortest path. Return infinity.
    return(Inf)
  }
  
  steps = 0
  currentNode = v2
  # Walk up the bfs tree. Start at currentNode = v2.
  # Search the one and only edge that ends in currentNode, which is uniquely defined,
  # because the tree is rooted in v1. Then repeat this process from the start of that edge.
  # Walk up the tree until v1
  while (!(currentNode == v1)){
    steps = steps+1
    currentNode = bfsTreeEdges[which(bfsTreeEdges[,2] == currentNode), 1]
  }
  return(steps)
}
#..............................................................................#
#ex.2
visualize_nodes <- function(g, nodes_vector){
  V(g)$color = "#dddddd"
  V(g)[nodes_vector]$color = "orange"
  plot(g)
}

getEccentricity = function(g, v){
  longestShortestPath = 0
  for (w in V(g)) {
    if (!(v == w)) {
      shortestPath = shortestPathLength(g, v, w)
      if (shortestPath > longestShortestPath){
        longestShortestPath = shortestPath
      }
    }
  }
  return(longestShortestPath)
}

getCenterNodes = function(g){
  if (!is.connected(g)){
    print("The graph is not connected!")
  }
  eccentricityVector = rep(1/0, length(V(g)))
  
  for (v in V(g)){
    eccentricityVector[v] = getEccentricity(g, v)
  }
  print(paste("The minimum eccentricity of the graph is ", min(eccentricityVector)))
  
  visualize_nodes(g,which(eccentricityVector == min(eccentricityVector)))
  return(which(eccentricityVector == min(eccentricityVector)))
}