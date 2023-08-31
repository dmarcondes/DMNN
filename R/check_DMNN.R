#' @export
#' @title Check a DMNN architecture
#'
#' @description Check if a Discrete Morphological Neural Network architecture is valid
#'
#' @param vertices A vector of vertices
#' @param edges A matrix of edges
#' @param compute A vector with the computation type of the vertices
#' @param param List with the parameters of a realization of the DMNN
#' @return Logical indicating if the DMNN represented by the given Morphological Computational graph is valid
#' @examples
#' check_DMNN(asf_sg16_sg16$vertices,asf_sg16_sg16$edges,asf_sg16_sg16$compute,asf_sg16_sg16$param)
check_DMNN <- function(vertices,edges,compute,param){
  #Set paramaters
  Ie <- colSums(edges) #Number of input of each edge
  Oe <- rowSums(edges) #Number of outputs of each edge
  Ie_psi <- Ie[!(compute %in% c("sup","inf"))] #vertices that compute operators
  Ie_oper <- Ie[compute %in% c("sup","inf")] #vertices that comput operations

  #Test if there is at least three vertices
  if(length(vertices) < 3){
    stop("Must have at least 3 vertices!")
    return(FALSE)
  }

  #Test if input edge is correctly specified
  if(sum(Ie == 0) > 1 | sum(compute[Ie == 0] != "identity") > 0){
    stop("Input edge misspecified!")
    return(FALSE)
  }

  #Test if output edge is correctly specified
  if(sum(Oe == 0) > 1 | sum(compute[Oe == 0] != "identity") > 0){
    stop("Output edge misspecified!")
    return(FALSE)
  }

  #Test if all operator vertices have only one input
  if(max(Ie_psi) > 1){
    stop("Operator edge with more than one input!")
    return(FALSE)
  }

  #Teste if all operations vertices have at least two inputs
  if(min(Ie_oper) < 2){
    stop("Operation edge with less than two inputs!")
    return(FALSE)
  }

  #Test if all vertices, but the output edge, are connected to at least one other vertices
  if(sum(Oe == 0) > 1){
    stop("Edge output is not input of other edge!")
    return(FALSE)
  }

  #Test if parameters are correctly specified
  for(i in 1:length(compute)){
    if(compute[i] == "supgen"){
      if(length(param[[i]]) != 2 | sum(unlist(lapply(param[[i]],function(x) length(dim(x)))) != 2) > 0){
        stop(paste("Sup-generator parameter",i,"misspecified!"))
        return(FALSE)
      }
    }
    if(compute[i] %in% c("erosion","dilation","opening","closing","asf")){
      if(length(param[[i]]) != 1 | sum(unlist(lapply(param[[i]],function(x) length(dim(x)))) != 2) > 0){
        stop("ASF parameter",i,"misspecified!")
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
