dichotomize <- function(Data,variables, sep="", min=1, length=0, values=NULL,
                        sparse=FALSE, add=TRUE, sort=TRUE) {
    if(is.data.frame(Data)){
      if (min>0 & min<1) min = min*nrow(Data)
      cn <- colnames(Data)
      names(cn) <- cn
      if (length(sep)!=length(variables))
         sep = rep(sep[1],length(variables))
         names(sep) <- variables
         
      for(c in variables){
        if (sep[c]!="")
           L <- lapply(strsplit(as.character(Data[[c]]), sep[c]), paste, sep[c] ,sep="") # Sep at the end of each element
        else {
           L <- Data[[c]]
        }
        if (is.null(values)) Z <- valuesof(L,length,min,sort,sep)
        else                 Z <- paste(values, sep[c],sep="")
      Z <- paste(sep[c],Z,sep="") # To search
      C <- paste(sep[c],Data[[c]],sep[c],sep="")  # Searched    
      Q <- Matrix(0,nrow=length(Z),ncol=nrow(Data),sparse=TRUE) # Result

      for(X in 1:length(Z)) {
        N <- grep(Z[X],C,fixed=TRUE)
        if (length(N) > 0) Q[X,N]<-1
      }
      Z <- substring(Z,nchar(sep[c])+1,nchar(Z)-nchar(sep[c]))
      Z[Z==""]<-".ND."
      if (min>0 & !is.null(values)) {
        VF <- apply(Q,1,sum)
        if (length(VF[VF>=min])>1) {
           Q <- Q[VF>=min,]
           Z <- Z[VF>=min]
        }
        else warning("min > empirical observations")
        if (sort==TRUE) {
          Q<-Q[order(-VF[VF>=min]),]
          Z<-Z[order(-VF[VF>=min])]
        }
        if (length>0) {
          Q<-Q[1:min(length,nrow(Q)),]
          Z<-Z[1:min(length,nrow(Q))]
        }
      }
      if(length(variables)>1)
        Z <- paste(cn[c],Z,sep=".")
      if (sparse==TRUE) {
        Q<-t(Q)
        colnames(Q)<-Z
        Data<-Q
      }
      else {
        W <- as.data.frame(t(as.data.frame(as.matrix(Q), row.names=Z)))
        if (add==TRUE) Data <- cbind(Data,W)
        else Data<-W
      }
    }
    return(Data)
  }else
    warning("You must pass a data frame!")
}

valuesof<-function(x,length=0,min=0,sort=TRUE,sep="") {
  x <- table(unlist(x))
  if (sort) x <- x[order(-x)]
  if (min>0) x<-x[x>=min]
  if (nrow(x)==0) return(NULL)
  if (length>0) x<- x[x>=(x[order(-x)][min(length,nrow(x))])]
  return(names(x))
}
