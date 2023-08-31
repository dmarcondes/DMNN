#' @import ggplot2
#' @export
#' @title Plot a binary image
#'
#' @description Plot a binary image from a matrix
#'
#' @param X A binary matrix
#' @return A ggplot object with a black and white image
#' @examples
#' image_bw(input_digits[[1]])
image_bw <- function(X){
  d <- as.data.frame(as.table(X))
  p <- ggplot(d,aes(x = Var1,y = Var2,fill = factor(Freq))) + geom_raster() + theme_void() +
    scale_fill_manual(values = c("white","black")) + theme(legend.position = "none")
  return(p)
}
