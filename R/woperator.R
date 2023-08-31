#' @import mmand
#' @export
#' @title Apply sup-generating decomposed operator
#'
#' @description Apply the operator represented by a sup-generating decomposition
#'
#' @param X A single input image or a list of input images
#' @param A List of lower limits of intervals
#' @param B List of upper limits of intervals
#' @return Images obtained after applying the sup-generated operator with kernel given by the union of the intervals [A,B]
#' @examples
#' A <- list(rbind(c(0,1,0),c(1,1,1),c(0,1,0)),rbind(c(1,0,0),c(0,1,0),c(0,0,1)))
#' B <- list(rbind(c(1,1,1),c(1,1,1),c(1,1,1)),rbind(c(1,1,1),c(1,1,1),c(1,1,1)))
#' y <- woperator(input_digits[[1]],A,B)
#' image_bw(y)
woperator <- function(X,A,B){
  if(is.list(X)){
    psiX <- list()
    for(i in 1:length(X)){
      psiX[[i]] <- sup_generating(X[[i]],A[[1]],B[[1]])
      if(length(A) > 1){
        for(j in 2:length(A))
          psiX[[i]] <- pmax(psiX[[i]],sup_generating(X[[i]],A[[j]],B[[j]]))
      }
    }
  }
  else{
    psiX <- sup_generating(X,A[[1]],B[[1]])
    if(length(A) > 1){
      for(j in 2:length(A))
        psiX <- pmax(psiX,sup_generating(X,A[[j]],B[[j]]))
    }
  }
  return(psiX)
}
