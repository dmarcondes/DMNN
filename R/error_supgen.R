#' @export
#' @title Error of sup-generating decomposed operator
#'
#' @description Calculate the error of an operator represented by a sup-generating decomposition
#'
#' @param X List of input images
#' @param Y List of output images
#' @param A List of lower limits of intervals
#' @param B List of upper limits of intervals
#' @param type Error type. Should be "iou" or "mae"
#' @return Error of operator with kernel given by the union of the intervals represented by [A,B]
#' @examples
#' error_supgen(X = input_digits[[1]],Y = output_digits[[1]],A = rbind(c(0,1,0),c(1,1,1),c(0,1,0)),
#'              B = rbind(c(1,1,1),c(1,1,1),c(1,1,1)))
error_supgen <- function(X,Y,A,B,type = "mae"){
  e <- 0
  if(!is.list(X))
    X <- list(X)
  if(!is.list(Y))
    Y <- list(Y)
  for(i in 1:length(X)){
    psiX <- woperator(X[[i]],A,B)
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
