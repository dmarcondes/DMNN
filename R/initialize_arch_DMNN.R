#' @export
#' @title Initialize DMNN architecture
#'
#' @description Randomly initialize the parameters of a Discrete Morphological Neural Network architecture
#'
#' @param compute A vector with the computation type of vertices
#' @param width Width of each computation
#' @param density Expected density of ones in the random parameters
#' @param random Type of initialization. Should be "random", "random_identity" or "identity"
#' @return List with initial parameters for the DMNN architecture
#' @examples
#' param <- initialize_arch_DMNN(asf_sg16_sg16$compute,rep(3,length(asf_sg16_sg16$compute)))
initialize_arch_DMNN <- function(compute,width,density = 0.3,random = "random"){
  param <- list()

  if(random == "free"){
    #For each edge
    for(i in 1:length(compute)){
      if(compute[i] == "sup_gen"){
        param[[i]] <- list("A" = NULL,"B" = NULL)
        param[[i]]$A <- matrix(sample(c(0,1),width[i]^2,T,c(1 - density,density)),width[i],width[i])
        param[[i]]$B <- pmax(param[[i]]$A,matrix(sample(c(0,1),width[i]^2,T,c(1 - density,density)),width[i],width[i]))
      }
      else if(compute[i] %in% c("erosion","dilation","opening","closing","asf"))
        param[[i]] <- list("A" = matrix(sample(c(0,1),width[i]^2,T,c(1 - density,density)),width[i],width[i]))
      else
        param[[i]] <- NA
    }
  }
  else if(random == "random_identity"){
    #For each edge
    for(i in 1:length(compute)){
      if(compute[i] %in% c("inf_gen","sup_gen")){
        param[[i]] <- list("A" = NULL,"B" = NULL)
        param[[i]]$A <- pmax(matrix(c(rep(0,floor(width[i]^2/2)),1,rep(0,floor(width[i]^2/2))),width[i],width[i]),matrix(sample(c(0,1),width[i]^2,T,c(1 - density,density)),width[i],width[i]))
        param[[i]]$B <- matrix(1,width[i],width[i])
      }
      else if(compute[i] %in% c("erosion","dilation","opening","closing","asf"))
        param[[i]] <- list("A" = pmax(matrix(sample(c(0,1),width[i]^2,T,c(1 - density,density)),width[i],width[i]),matrix(c(rep(0,floor(width[i]^2/2)),1,rep(0,floor(width[i]^2/2))),width[i],width[i])))
      else
        param[[i]] <- NA
    }
  }
  else if(random == "identity"){
    #For each edge
    for(i in 1:length(compute)){
      if(compute[i] %in% c("inf_gen","sup_gen")){
        param[[i]] <- list("A" = NULL,"B" = NULL)
        param[[i]]$A <- matrix(c(rep(0,floor(width[i]^2/2)),1,rep(0,floor(width[i]^2/2))),width[i],width[i])
        param[[i]]$B <- matrix(1,width[i],width[i])
      }
      else if(compute[i] %in% c("erosion","dilation","opening","closing","asf"))
        param[[i]] <- list("A" = matrix(c(rep(0,floor(width[i]^2/2)),1,rep(0,floor(width[i]^2/2))),width[i],width[i]))
      else
        param[[i]] <- NA
    }
  }
  return(param)
}
