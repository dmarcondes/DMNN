#' @import mmand
#' @export
#' @title Apply a DMNN
#'
#' @description Apply a Discrete Morphological Neural Network
#'
#' @param X A single input image
#' @param edges A matrix with the edges
#' @param compute A vector with the computation type of vertices
#' @param param List with the parameters of vertices computation
#' @return Images obtained after applying the given DMNN to X
#' @examples
#' y <- DMNN(input_digits[[1]],asf_sg16_sg16$edges,asf_sg16_sg16$compute,asf_sg16_sg16$param)
#' image_bw(y)
DMNN <- function(X,edges,compute,param){
  #Check if architecture is valid
  #vertices <- colnames(edges)
  #if(!check_DMNN(vertices,edges,compute,param))
  #  return(0)

  #Initialize parameters
  Ie <- colSums(edges)
  Oe <- rowSums(edges)
  input <- which(Ie == 0)
  output <- which(Oe == 0)
  computations <- as.list(1:nrow(edges))

  #Pad input image
  pad <- floor(max(unlist(lapply(param,function(x) unlist(lapply(x,dim)))))/2)
  X <- pad_matrix(X,pad)
  computations[[input]] <- X

  #Compute graph
  edge <- input #Start on input edge
  next_vertices <- c() #Object to store vertices which have received an input
  while(edge != output | length(next_vertices) > 1){ #While has not reached output edge or there are still vertices to compute
    if(compute[edge] %in% c("sup","inf")){ #If the current edge performs an operation, try to realize it
      #Test if all input computations of the operation have been performed
      if(sum(unlist(lapply(computations[which(edges[,edge] == 1)],function(x) length(dim(x)))) == 1) == 0){
        #If prior computations have been performed, perform the sup/inf computation
        if(compute[edge] == "sup"){
          computations[[edge]] <- computations[[which(edges[,edge] == 1)[1]]]
          for(j in 2:length(which(edges[,edge] == 1)))
            computations[[edge]] <- pmax(computations[[edge]],computations[[which(edges[,edge] == 1)[j]]])
        }
        else if(compute[edge] == "inf"){
          computations[[edge]] <- computations[[which(edges[,edge] == 1)[1]]]
          for(j in 2:length(which(edges[,edge] == 1)))
            computations[[edge]] <- pmin(computations[[edge]],computations[[which(edges[,edge] == 1)[j]]])
        }
      }
      else{
        #If the input computations have not yet been performed, add this edge to the end of the line e go to next iteration of while
        next_vertices <- c(next_vertices[next_vertices != edge],edge)
        edge <- next_vertices[1] #Go to first edge of the line
        next
      }
    }
    #If the inf/sup have been performed or the edge computed an operator
    #Input the edge output wherever it is an input of an operator
    for(e in which(edges[edge,] == 1)){
      if(compute[e] == "sup_gen")
        computations[[e]] <- sup_generating(computations[[edge]],param[[e]][[1]],param[[e]][[2]])
      else if(compute[e] == "inf_gen")
        computations[[e]] <- inf_generating(computations[[edge]],param[[e]][[1]],param[[e]][[2]])
      else if(compute[e] == "asf")
        computations[[e]] <- ASF(computations[[edge]],param[[e]])
      else if(compute[e] == "erosion")
        computations[[e]] <- erode(computations[[edge]],param[[e]])
      else if(compute[e] == "dilation")
        computations[[e]] <- dilate(computations[[edge]],param[[e]])
      else if(compute[e] == "opening")
        computations[[e]] <- opening(computations[[edge]],param[[e]])
      else if(compute[e] == "closing")
        computations[[e]] <- closing(computations[[edge]],param[[e]])
      else if(compute[e] == "complement")
        computations[[e]] <- abs(1-computations[[edge]])
    }
    #Add the vertices pointed by the current edge to the line
    next_vertices <- next_vertices[next_vertices != edge]
    next_vertices <- unique(c(next_vertices,which(edges[edge,] == 1)))
    edge <- next_vertices[1] #Go to the first edge of the line
  }
  #add to output the computation of the only node that points to it
  computations[[output]] <- computations[[which(edges[,output] == 1)]]
  #Store the computation of the output to return
  psiX <- computations[[output]]
  #Unpad the result
  psiX <- psiX[(pad + 1):(nrow(psiX) - pad),(pad + 1):(nrow(psiX) - pad)]

  return(psiX)
}
