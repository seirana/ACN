library('igraph')
##diameter= max (eccentricity)
my_diameter <- function(A) {
  #A=get.adjacency(graph)
  D = A
  D[D == 0] <- Inf
  diag(D) <- 0
  for (k in 1:nrow(A)) {
    for (i in 1:nrow(A)) {
      for (j in 1:nrow(A)) {
        D[i, j] = min(D[i, j], (D[i, k] + D[k, j]))
      }
    }
  }
  eccentricity = c()
  for (i in 1:nrow(D)) {
    eccentricity = c(eccentricity, max(D[i, ]))
  }
  diameter = max(eccentricity)
  return(diameter)
}

#What distance measure is used in this case? distance wrt the diameter...
#adjacency matrix is used ?! With "binary" distance
my.QT.graphs = function(dist_mt, D) {
  res = list()
  i = 1
  if (length(rownames(dist_mt)) == 0) {
    rownames(dist_mt) = 1:nrow(dist_mt)
  }
  if (length(colnames(dist_mt)) == 0) {
    colnames(dist_mt) = 1:ncol(dist_mt)
  }
  all_names = row.names(dist_mt)
  while (length(dist_mt) > 1) {
    dist_mt = as.matrix(dist_mt)
    cluster_list = list()
    ##loop through every data point in Dist_mt
    for (data_pt in rownames(dist_mt)) {
      #place each data point in its own cluster
      cluster_list[[data_pt]] = data_pt
      #set Flag if diameter whether set D is not reached yet
      D_flag = T
      #as long as set diameter is not reached and we don't place
      #every point in one cluster (would be the endpoint of
      #divisive clustering)
      while (D_flag == T & length(cluster_list[[data_pt]]) != length(rownames(dist_mt))) {
        #add every left point to cluster and check D parameter
        diameter = c()
        for (pt_to_check in setdiff(rownames(dist_mt), cluster_list[[data_pt]])) {
          #create temporary subset
          tmp = c(pt_to_check, cluster_list[[data_pt]])
          diameter = c(diameter, my_diameter(dist_mt[tmp, tmp]))
          names(diameter)[length(diameter)] = pt_to_check
        }
        if (diameter[which.min(diameter)] > D) {
          D_flag = F
        } else{
          cluster_list[[data_pt]] = c(cluster_list[[data_pt]], names(diameter)[which.min(diameter)])
        }
      }
    }
    #remove largest cluster set (will be the first one if there are more than one)
    cluster_to_keep=names(which.max(lapply(cluster_list,length)))
    #result_liste
    res[[i]]=cluster_list[[cluster_to_keep]]
    i=i+1
        
    dist_mt=dist_mt[!(rownames(dist_mt)%in%cluster_list[[cluster_to_keep]]),!(colnames(dist_mt)%in%cluster_list[[cluster_to_keep]])]
    
  }
  
  if (length(unlist(res)) != length(all_names)) {
    res[[i]] = setdiff(all_names, unlist(res))
  }
  
  return(res)
}

mt = matrix(
  c(
    0,
    1,
    1,
    1,
    0,
    0,
    0,
    0,
    1,
    0,
    1,
    0,
    1,
    1,
    0,
    0,
    1,
    1,
    0,
    0,
    0,
    1,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    0,
    1,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    1,
    1,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    1,
    0,
    0,
    1,
    0
  ),
  nrow = 8,
  ncol = 8
)

g = graph_from_adjacency_matrix(mt, mode = "undirected")
plot(g)
my.QT.graphs(mt,1)
my.QT.graphs(mt,2)
my.QT.graphs(mt,3)
my.QT.graphs(mt,4)