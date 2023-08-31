#' @export
#' @title Error of a DMNN
#'
#' @description Calculate the error of a Discrete Morphological Neural Network
#'
#' @param X List of input images
#' @param Y List of output images
#' @param edges A matrix with the edges
#' @param compute A vector with the computation type of vertices
#' @param param List with the parameters of vertices computation
#' @param type Error type. Should be "iou" or "mae"
#' @return Error of the DMNN on sample (X,Y)
#' @examples
#' error_DMNN(X = input_digits,Y = output_digits,edges = asf_sg16_sg16$edges,
#'            compute = asf_sg16_sg16$compute,
#'            param = asf_sg16_sg16$param)
error_DMNN <- function(X,Y,edges,compute,param,type = "mae"){
  e <- 0
  for(i in 1:length(X)){
    psiX <- DMNN(X[[i]],edges,compute,param)
    if(type == "iou"){
      tot <- sum(pmax(psiX,Y[[i]]) == 1)
      interc <- sum(psiX + Y[[i]] == 2)
      e <- e + (1 - interc/tot)
    }
    else if(type == "mae"){
      e <- e + mean(abs(psiX - Y[[i]]))
    }
  }
  return(e/length(X))
}
