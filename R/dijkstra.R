#' @title The dijkstra function
#' @description Computer science stuff (???)
#' @param graph A data.frame with nodes and distances
#' @param init_node A numeric scalar of length 1
#' @return Returns vector with distances from init_node
#' @export dijkstra

dijkstra <- function(graph, init_node){
  #Set of nodes and 
  node_set <- union(graph$v1, graph$v2)
  stopifnot(init_node %in% node_set)
  
  visited_set <- init_node
  unvisited_set <-  setdiff(node_set,init_node)
  dists <- rep(NA_integer_, length(node_set))
  names(dists) <- as.character(node_set)
  dists[as.character(init_node)] <- 0
  
  while(any(is.na(dists))){
    candidates <- graph[graph$v1 %in% visited_set & graph$v2 %in% unvisited_set,]
    cheapest_candidate <- which.min(candidates$w)
    cheapest_way <- candidates[cheapest_candidate,3]
    cheapest_node <- candidates[cheapest_candidate,2]
    
    # Take the step...
    visited_set <- c(visited_set, cheapest_node)
    unvisited_set <- setdiff(unvisited_set,cheapest_node)
    dists[as.character(cheapest_node)] <- cheapest_way
    graph$w[graph$v1 == cheapest_node] <- graph$w[graph$v1 == cheapest_node] + cheapest_way
  }
  return(dists)
}