#' @export
#' @title Pad a matrix with zeros
#'
#' @description Pad a matrix with zeros on the left, right, above and below
#'
#' @param m A matrix
#' @param s Padding size
#' @return Matrix padded with s zeros
#' @examples
#' pad_matrix(matrix(1,3,3),1)
pad_matrix <- function(m,s){
  m <- rbind(matrix(0,s,ncol(m)),m,matrix(0,s,ncol(m)))
  m <- cbind(matrix(0,nrow(m),s),m,matrix(0,nrow(m),s))
  return(m)
}
