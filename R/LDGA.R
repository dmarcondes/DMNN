#' @import mmand
#' @import ggplot2
#' @import tidyverse
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#' @title Lattice Gradient Descent Algorithm for DMNN
#'
#' @description Lattice Gradient Descent Algorithm for training general Discrete Morphological Neural Networks
#'
#' @param X List of input training images
#' @param Y List of output training images
#' @param Xval List of input validation images
#' @param Yval List of output validation images
#' @param b Batch size
#' @param epochs Number of training epochs
#' @param vertices A vector of vertices
#' @param compute A vector with the computation type of vertices
#' @param edges A matrix with the edges
#' @param param List with initial parameters of vertices computation
#' @param type Error type. Should be "iou" or "mae"
#' @param sample Number of neighbors to sample or FALSE if it should not sample
#' @param cores Number of cores for parallel computing.
#' @return param_global Parameters of the DMNN with the least training error obtained during the training
#' @return last_param Parameters of the last DMNN
#' @return trace A data frame with the trace of the algorithm
#' @references D. Marcondes, J. Barrera. Discrete Morphological Neural Networks. 2023.
#' @examples
#' train <- LGDA(X = list(input_digits[[1]]),Y = list(output_digits[[1]]),Xval = input_digits,
#'               Yval = output_digits,
#'               b = 10,epochs = 1,vertices = asf_sg16_sg16$vertices,compute = asf_sg16_sg16$compute,
#'               edges = asf_sg16_sg16$edges,param = asf_sg16_sg16$param,sample = 50)
LGDA <- function(X,Y,Xval = NULL,Yval = NULL,b = 1,epochs = 10,vertices,compute,edges,param,type = "mae",sample = F,cores = 1){
  cat("-----------------------------------------------------------------------\n")
  cat("Lattice Gradient Descent for training Discrete Morphological Neural Network\n")
  cat(paste("Vertices =",length(vertices),"\n"))
  cat(paste("Training epochs = ",epochs,"\n"))
  cat(paste("Sample size = ",length(X),"\n"))
  cat("-----------------------------------------------------------------------\n")

  #Begin time
  t0 <- Sys.time()

  #Register parallel
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  #Test if graph is valid
  if(!check_DMNN(vertices = vertices,edges = edges,compute = compute,param = param))
    return(0)

  #Set parameters and sample structuring elements
  width <- unlist(lapply(param,function(x) ifelse(is.null(dim(x[[1]])[1]),NA,dim(x[[1]])[1])))
  trace <- na.omit(data.frame("Epoch" = NA,"Train_error" = NA,"Val_error" = NA,"Time" = NA))
  e <- error_DMNN(X,Y,edges,compute,param,type)
  e_val <- NA
  if(!is.null(Xval))
    e_val <- error_DMNN(Xval,Yval,edges,compute,param,type)
  e_global <- e
  param_global <- param

  #For each epoch
  for(run in 1:epochs){
    cat(paste("Time:",round(as.numeric(difftime(Sys.time(),t0,units = "mins")),2),"mins | Epoch",run,"/",epochs,"- Minimum training error:",round(e_global,5),
        "- Validation error:",ifelse(!is.null(Xval),round(e_val,5),"XX"),"- Current error:",round(e,5),"\n"))

    #Reorder sample
    epoch_reorder <- sample(1:length(X),length(X),F)
    X <- X[epoch_reorder]
    Y <- Y[epoch_reorder]

    #For each batch
    for(batch in 1:ceiling(length(X)/b)){

      #Batch data
      Xbatch <- list(X[((batch - 1)*b + 1):(min(batch*b,length(X)))])[[1]]
      Ybatch <- list(Y[((batch - 1)*b + 1):(min(batch*b,length(X)))])[[1]]

      #List neighbors
      N_par <- c()
      for(k in 1:length(param)){
        if(compute[k] == "sup_gen"){
          for(i in 1:width[k]){
            for(j in 1:width[k]){
              if(param[[k]][[1]][i,j] == 0)
                N_par <- rbind(N_par,cbind(k,2,i,j))
              if(param[[k]][[2]][i,j] == 1)
                N_par <- rbind(N_par,cbind(k,1,i,j))
              if(param[[k]][[1]][i,j] == 1)
                N_par <- rbind(N_par,cbind(k,1,i,j))
            }
          }
        }
        else if(compute[k] %in% c("erosion","dilation","opening","closing","asf")){
          for(i in 1:width[k])
            for(j in 1:width[k])
              N_par <- rbind(N_par,cbind(k,1,i,j))
        }
      }
      N_par <- unique(N_par)
      if(sample > 0)
        N_par <- N_par[sample(1:nrow(N_par),sample,F),]

      #Error of sample batch
      e <- error_DMNN(Xbatch,Ybatch,edges,compute,param,type)
      cat(paste("Time:",round(as.numeric(difftime(Sys.time(),t0,units = "mins")),2),"mins | Epoch",run,"/",epochs,"Batch",batch,"/",ceiling(length(X)/b),
                "- Batch error:",round(e,5),"\n"))

      #Calculate error of neighbors
      error_N <- foreach(i = 1:nrow(N_par),.combine = 'c',.export = 'error_DMNN') %dopar% {
          paramn <- param
          paramn[[N_par[i,1]]][[N_par[i,2]]][N_par[i,3],N_par[i,4]] <- abs(paramn[[N_par[i,1]]][[N_par[i,2]]][N_par[i,3],N_par[i,4]] - 1)
          error_DMNN(Xbatch,Ybatch,edges,compute,paramn,type)
      }
      min_neigh <- which(error_N == min(error_N))
      if(length(min_neigh) == 1)
        s <- min_neigh
      else
        s <- sample(x = min_neigh,size = 1)
      param[[N_par[s,1]]][[N_par[s,2]]][N_par[s,3],N_par[s,4]] <- abs(param[[N_par[s,1]]][[N_par[s,2]]][N_par[s,3],N_par[s,4]] - 1)
      e <- min(error_N)
    }
    e <- error_DMNN(X,Y,edges,compute,param,type)
    if(e < e_global){
      e_global <- e
      param_global <- param
      if(!is.null(Xval))
        e_val <- error_DMNN(Xval,Yval,edges,compute,param_global,type)
    }
    trace <- rbind.data.frame(trace,data.frame("Epoch" = run,"Train_error" = e,"Val_error" = e_val,"Time" = as.numeric(difftime(Sys.time(),t0,units = "mins"))))

    #Plot
    tmp <- trace %>% gather("Error","value",-Epoch)
    tmp <- tmp %>% filter(Error != "Time")
    p <- ggplot(tmp,aes(x = Epoch,y = value,color = Error,group = Error)) + theme_linedraw() + geom_point() + geom_line() +
      ylab("Error") + theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,
                                                                                           color = "black"),
                            axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                            legend.title = element_text(size = 14), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), panel.border = element_blank(),
                            panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                            legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                            legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
    suppressWarnings(suppressMessages(print(p)))
  }

  #Close parallel
  stopCluster(cl)

  return(list("param_global" = param_global,"last_param" = param,"trace" = trace))
}
