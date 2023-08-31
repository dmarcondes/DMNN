#' @import POSetR
#' @import igraph
#' @import latex2exp
#' @export
#' @title Plot a Morphological Computational Graph
#'
#' @description Plot the Morphological Computational Graph of a Discrete Morphological Neural Network
#'
#' @param vertices A vector of vertices
#' @param compute A vector with the computation type of vertices
#' @param edges A matrix with the edges
#' @return A plot with the architecture graph
#' @examples
#'plot_architecture(vertices = asf_sg16_sg16$vertices,compute = asf_sg16_sg16$compute,
#'                  edges = asf_sg16_sg16$edges)
plot_architecture <- function(vertices,compute,edges){
  #Create graph
  g <- graph_from_adjacency_matrix(edges,mode = c("directed"))

  #Create poset so plot is prettier
  p <- poset_from_igraph(g)

  #Label each vertice with latex
  lab <- compute
  lab[vertices %in% c("input","output")] <- vertices[vertices %in% c("input","output")]
  lab_exp <- lab
  for(i in 1:length(lab)){
    lab_exp[i] <- ifelse(lab[i] == "inf",TeX('$\\wedge$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "sup",TeX('$\\vee$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "complement",TeX('$\\nu$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "asf",TeX('$\\gamma\\varphi$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "opening",TeX('$\\gamma$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "closing",TeX('$\\varphi$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "erosion",TeX('$\\epsilon$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "dilation",TeX('$\\delta$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "sup_gen",TeX('$\\lambda$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "input",TeX('$X$'),lab_exp[i])
    lab_exp[i] <- ifelse(lab[i] == "output",TeX('$\\psi$'),lab_exp[i])
  }

  return(plot(p,equispaced = T,vertex.label = lab_exp,vertex.size = 5))
}
