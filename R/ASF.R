#' @import mmand
#' @export
#' @title Apply Alternate-Sequential Filter
#'
#' @description Apply an Alternate-Sequential Filter
#'
#' @param X A single input image or a list of input images
#' @param A List of structuring elements
#' @return Images obtained after applying the ASF with structuring elements A
#' @examples
#' y <- ASF(input_digits[[1]],list(rbind(c(1,0,0),c(0,1,0),c(0,0,1)),
#'          rbind(c(0,1,0),c(1,1,1),c(0,1,0))))
#' image_bw(y)
ASF <- function(X,A){
  if(is.list(X)){
    psiX <- list()
    for(j in 1:length(X)){
      psiX[[j]] <- X[[j]]
      for(i in 1:length(A))
        psiX[[j]] <- closing(opening(psiX[[j]],A[[i]]),A[[i]])
    }
  }
  else{
    psiX <- X
    for(i in 1:length(A))
      psiX <- closing(opening(psiX,A[[i]]),A[[i]])
  }
  return(psiX)
}
