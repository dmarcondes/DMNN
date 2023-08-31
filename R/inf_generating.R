#' @import mmand
#' @export
#' @title Apply an inf-generating operator
#'
#' @description Apply an inf-generating operator to a single image or a list of images
#'
#' @param X A single input image or a list of input images
#' @param A Lower limit of interval
#' @param B Upper limit of interval
#' @return Images obtained after applying the inf-generating operator with interval [A,B].
#' @examples
#' y <- inf_generating(X = input_digits[[1]],A = rbind(c(0,1,0),c(1,1,1),c(0,1,0)),
#'                     B = rbind(c(1,1,1),c(1,1,1),c(1,1,1)))
#' image_bw(y)
inf_generating <- function(X,A,B){
  if(is.list(X)){
    psiX <- list()
    for(i in 1:length(X)){
      t1 <- dilate(X[[i]],A)
      t1[is.infinite(t1)] <- 1
      t2 <- abs(1 - erode(X[[i]],t(abs(1 - B))))
      t2[is.infinite(t2)] <- 1
      psiX[[i]] <- pmax(t1,t2)
    }
  }
  else{
    t1 <- dilate(X,A)
    t1[is.infinite(t1)] <- 1
    t2 <- abs(1 - erode(X,t(abs(1 - B))))
    t2[is.infinite(t2)] <- 1
    psiX <- pmax(t1,t2)
  }
  return(psiX)
}
