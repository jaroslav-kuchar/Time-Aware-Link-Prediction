library(Matrix)

# initialize tensor with set c() of entities and set of types, dimension of tensor will be entities*entities*types
initializeTensor <- function(entities, types){
  entities <- sort(entities)
  tensor <- list(entities=new.env(), slices=new.env())
  entities<-as.character(entities)
  tensor$entities <- new.env()
  for(e in 1:length(entities)){
    tensor$entities[[entities[e]]] = e;
  }
  for(e in 1:length(types)){
    tensor$slices[[types[e]]] = Matrix(0, ncol=length(entities), nrow=length(entities), sparse=TRUE);
  }
  return(tensor)
}

# copy tensor
copyTensor <- function(tensor){
  t <- list(entities=new.env(), slices=new.env())
  t$entities <- tensor$entities
  types <- ls(tensor$slices)
  for(e in 1:length(types)){
    t$slices[[types[e]]] = Matrix(tensor$slices[[types[e]]], sparse=TRUE);    
  }
  return(t)
}

# randomize all values in tensor values
randomizeTensor <- function(tensor){
  types <- ls(tensor$slices)
  for(e in 1:length(types)){
    tensor$slices[[types[e]]] = Matrix(data=runif(length(tensor$entities)*length(tensor$entities),0,1), ncol=length(tensor$entities), nrow=length(tensor$entities), sparse=TRUE);
  }
  return(tensor)
}

# print all 
printTensor <- function(tensor){
  entities <- ls(tensor$entities)
  for(k in 1:length(entities)){
    print(entities[k])
    print(tensor$entities[[entities[k]]])
  }
  types <- ls(tensor$slices)
  for(k in 1:length(types)){
    print(types[k])
    print(tensor$slices[[types[k]]])
  }
}

# find set of predicted targets
topNtargets <- function(source,type,tensor,factorizationOutput){
  sourceId <- tensor$entities[[source]]
  objectVectorA <- factorizationOutput$A[sourceId,]%*%factorizationOutput$R$slices[[type]]%*%t(factorizationOutput$A)  
  originalVector <- tensor$slices[[type]][sourceId,]
  out <- data.frame(name=ls(tensor$entities), values=as.vector(t(objectVectorA)))
  out <- out[-which(originalVector>0),]
  return(out[order(out$values, decreasing=TRUE),])
}


# add new link to tensor with new dimensions
addTensorValue <- function(tensor, source, type, target, value) {  
  if(is.null(tensor$entities[[source]])){
    tensor$entities[[source]] <- length(ls(tensor$entities))+1
  }
  if(is.null(tensor$entities[[target]])){
    tensor$entities[[target]] <- length(ls(tensor$entities))+1
  }
  if(is.null(tensor$slices[[type]])){
    t <- initializeTensor(ls(tensor$entities),c(ls(tensor$slices),type))
  } else {
    t <- initializeTensor(ls(tensor$entities),ls(tensor$slices))
  }
  t$entities <- tensor$entities
  types <- ls(tensor$slices)
  for(k in 1:length(types)){
    t$slices[[types[[k]]]][(1:nrow(tensor$slices[[types[[k]]]])),(1:ncol(tensor$slices[[types[[k]]]]))] <- tensor$slices[[types[[k]]]]
  }
  setTensorValue(t,source,type,target,value)
  return(t)
}

# set value of tensor
setTensorValue <- function(tensor, source, type, target, value) {
  eval.parent(substitute(tensor$slices[[type]][tensor$entities[[source]],tensor$entities[[target]]]<-value))
}

# get recontructed value for results of tensor
getValue <- function(source, type, target, tensor, factorizationOutput){
  Rk <- (factorizationOutput$R)$slices[[type]]
  objectVectorA <- factorizationOutput$A[tensor$entities[[source]],]%*%Rk%*%t(factorizationOutput$A)
  if(!is.null(tensor$entities[[target]])){
    return(objectVectorA[tensor$entities[[target]]])
  } else {
    return(NULL)
  }
}


# X - input tensor as list of entities and list of slices
# r - number of latent factors
# maxiterations - number of max iterations
# changeCriterion - change of iteration
# lambda - regularization parameter
factorization <- function(X, r, maxiterations = 100, changeCriterion = 0.001, lambda = 0.01){
  X <- copyTensor(X)
  iteration <- 0
  types <- ls(X$slices)
  
  cputime=system.time({
    
    set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
    
    # initialize A matrix (n*r) to random values (0,1)
    A <- matrix(data=runif(length(ls(X$entities))*r,0,1), nrow=length(ls(X$entities)), ncol=r) 
    # initialize R tensor - frontal slices (r*r*m) to random values (0,1)
    R <- initializeTensor(c(1:r),types)
    R <- randomizeTensor(R)
    change <- changeCriterion + 1
    
    while(iteration < maxiterations && change > changeCriterion){ 
      Aold = A
      Rold = copyTensor(R)
      
      iteration = iteration + 1
      
      # update A
      Aleft = 0
      Aright = 0
      d = diag(1,r,r)
      d[1,1] = 0
            
      for(k in 1:length(types)){
        # k-th slice or R
        Rk <- R$slices[[types[k]]]
        # k-th slice of X
        Xk <- X$slices[[types[k]]]
        # Bk & Ck parameter
        Bk <- Rk%*%t(A)%*%A%*%t(Rk)
        Ck <- t(Rk)%*%t(A)%*%A%*%Rk
        Aleft = Aleft + (Xk%*%A%*%t(Rk) + t(Xk)%*%A%*%Rk)
        Aright = Aright + (Bk + Ck + lambda*d)        
      }     
      A <- Aleft%*%solve(Aright)  # multiplication and assigning    
      
      # update R  - QR decomposition
      Qqr <- qr.Q(qr(A)) 
      Aqr <- qr.R(qr(A))   

      Z <- Aqr%x%Aqr        
      d = diag(1,ncol(Z),ncol(Z))
      d[1,1] = 0
      Zm <- (solve((t(Z)%*%Z) +lambda*d))%*%t(Z)            
      for(k in 1:length(types)){
        # k-th slice of X
        Xk <- X$slices[[types[k]]]
        Xkqr <- t(Qqr)%*%Xk%*%Qqr
        Rknew <- Zm%*%c(matrix(Xkqr))
        # matricize and update R slice          
        R$slices[[types[k]]] <- matrix(Rknew, nrow = r, ncol = r)
      }
      # compute change
      change <- sum((A-Aold)^2)
      for(k in 1:length(types)){
        change <- change + sum((R$slices[[types[k]]]-Rold$slices[[types[k]]])^2)
      }      
      # evaluate - sum of differencies
      print(paste("Iteration:", iteration, ",change: ", change))
      
    }   
  })   
  # output
  output = list()
  output$r = r
  output$cputime = cputime
  output$R = R
  output$A = A
  output$iteration = iteration
  return(output)
}