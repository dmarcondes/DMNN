#' @export
#' @title Error of Alternate-Sequential Filter
#'
#' @description Calculate the error of an Alternate-Sequential Filter
#'
#' @param X List of input images
#' @param Y List of output images
#' @param A List of structuring elements
#' @param type Error type. Should be "iou" or "mae"
#' @return Error of ASF with structuring elements A on sample (X,Y)
#' @examples
#' error_asf(X = input_digits[[1]],Y = output_digits[[1]],
#'           A = list(rbind(c(1,0,0),c(0,1,0),c(0,0,1)),rbind(c(0,1,0),c(1,1,1),c(0,1,0))))
error_asf <- function(X,Y,A,type = "mae"){
  e <- 0
  if(!is.list(X))
    X <- list(X)
  if(!is.list(Y))
    Y <- list(Y)
  for(i in 1:length(X)){
    psiX <- ASF(X[[i]],A)
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
