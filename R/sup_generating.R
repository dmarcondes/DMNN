#' @import mmand
#' @export
#' @title Apply a sup-generating operator
#'
#' @description Apply a sup-generating operator to a single image or a list of images
#'
#' @param X A single input image or a list of input images
#' @param A Lower limit of interval
#' @param B Upper limit of interval
#' @return Images obtained after applying the sup-generating operator with interval [A,B].
#' @examples
#' y <- sup_generating(X = input_digits[[1]],A = rbind(c(0,1,0),c(1,1,1),c(0,1,0)),
#'                     B = rbind(c(1,1,1),c(1,1,1),c(1,1,1)))
#' image_bw(y)
sup_generating <- function(X,A,B){
  if(is.list(X)){
    psiX <- list()
    for(i in 1:length(X)){
      t1 <- erode(X[[i]],A)
      t1[is.infinite(t1)] <- 1
      t2 <- abs(1 - dilate(X[[i]],abs(1 - t(B))))
      t2[is.infinite(t2)] <- 1
      psiX[[i]] <- pmin(t1,t2)
    }
  }
  else{
    t1 <- erode(X,A)
    t1[is.infinite(t1)] <- 1
    t2 <- abs(1 - dilate(X,abs(1 - t(B))))
    t2[is.infinite(t2)] <- 1
    psiX <- pmin(t1,t2)
  }
  return(psiX)
}
