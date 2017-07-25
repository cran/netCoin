# surCoin is a wrapper to build a netCoin object from an original non-dichotomized data.frame. See below dichotomize()

surCoin<-function(data,variables=names(data),exogenous=NULL, commonlabel=NULL,
                  weight=NULL, subsample=FALSE,
                  minimum=1, maximum=nrow(data), sort=FALSE, decreasing=TRUE,
                  nodes=NULL, frequency=FALSE, percentages=TRUE,
                  name="name", label = NULL, ntext = NULL, 
                  size = "%", color = NULL, shape = NULL, group = NULL, community = NULL, 
                  procedures="Haberman", criteria="Z", Bonferroni=FALSE,
                  support=-Inf, minL=-Inf, maxL=Inf,
                  directed=FALSE, diagonal=FALSE, sortL=NULL, decreasingL=TRUE,
                  lwidth = NULL, lweight = NULL, lcolor = NULL, ltext = NULL,
                  nodeFilter = NULL, linkFilter = NULL,
                  main = NULL, note = NULL, help = NULL,
                  layout = NULL, language = "en", 
                  image = NULL,  dir = NULL, show = TRUE,
                  igraph=FALSE 
                  ){
  incidences<-dichotomize(data, variables, "", min=minimum, length=0, values=NULL, sparse=FALSE, add=FALSE, sort=sort)
  incidences<-na.omit(incidences)
  if (all(incidences==0 | incidences==1)) {
    C<-coin(incidences, minimum, maximum, sort, decreasing, weight=weight, subsample=subsample)
    if (is.null(nodes)) {
      O<-asNodes(C,frequency,percentages,language)
      if (language=="es" & name=="name") name<-"nombre"
      if (!is.null(commonlabel)) { # Preserve the prename (variable) of a node if specified in commonlabel
        ifelse(language=="es",label<-"etiqueta",label<-"label")
        provlabels<-as.character(O[[name]])
        O[[label]]<-ifelse(substr(O[[name]],1,regexpr('\\:',O[[name]])-1) %in% commonlabel,provlabels,substr(O[[name]],regexpr('\\:',O[[name]])+1,1000000L))
      }
    }
    else O<-nodes
    E<-edgeList(C, procedures, criteria, Bonferroni, minL, maxL, support, 
                directed, diagonal, sortL, decreasing)
    if (!is.null(exogenous)) {
      E$chaine<-ifelse((substr(E$source,1,regexpr("\\:",E$source)-1) %in% exogenous) 
                       &   (substr(E$target,1,regexpr("\\:",E$target)-1) %in% exogenous),"No","Yes")
      linkFilter<-paste(ifelse(is.null(linkFilter),"",paste(linkFilter,"&")),"chaine=='Yes'")
    }
    if (!is.null(layout)) {
      if (class(layout)=="matrix") layout<-layout
      else { 
        layout<-layoutControl(layout)
        xNx<-netCoin(O,E,name=name,weight=lweight)
        layout<-coords[[layout]](asIgraph(xNx))
      }
    }
    if (!is.null(community)) {
      commun<-congloControl(community)
      if (!exists("xNx")) xNx<-netCoin(O,E,name=name,weight=lweight)
      O$community<-as.character(membership(conglos[[commun]](asIgraph(xNx))))
      group<-"community"
    }    
    xNx<-netCoin(O,E,
                 name, label, 
                 size, color, shape, group, ntext, 
                 lwidth, lweight, lcolor, ltext,
                 nodeFilter, linkFilter,
                 main, note, help,
                 layout, language, 
                 image,  dir, show)
  }
  else warning("Input is not a dichotomous matrix of incidences")
  if (igraph) return(asIgraph(xNx))
  else return(xNx)
}

dichotomize <- function(data,variables, sep="", min=1, length=0, values=NULL,
                        sparse=FALSE, add=TRUE, sort=TRUE) {
    if(is.data.frame(data)){
      if (min>0 & min<1) min = min*nrow(data)
      cn <- colnames(data)
      names(cn) <- cn
      if (length(sep)!=length(variables))
         sep = rep(sep[1],length(variables))
         names(sep) <- variables
         
      for(c in variables){
        if (sep[c]!="")
           L <- lapply(strsplit(as.character(data[[c]]), sep[c],fixed=TRUE), paste, sep[c] ,sep="") # Sep at the end of each element
        else {
           sep[c]<-"??"
           L <- paste(na.omit(data[[c]]),sep[c],sep="")
        }
        if (is.null(values)) Z <- valuesof(L,length,min,sort,sep)
        else                 Z <- paste(values, sep[c],sep="")
      Z <- paste(sep[c],Z,sep="") # To search
      C <- paste(sep[c],data[[c]],sep[c],sep="")  # Searched    
      Q <- Matrix(0,nrow=length(Z),ncol=nrow(data),sparse=TRUE) # Result

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
        Z <- paste(cn[c],Z,sep=":")
      if (sparse==TRUE) {
        Q<-t(Q)
        colnames(Q)<-Z
        if (!exists("Data")) Data<-Q
        else Data<-cbind(Data,Q)
      }
      else {
        W <- as.data.frame(t(as.data.frame(as.matrix(Q), row.names=Z)))
        if (add==TRUE) data <- cbind(data,W)
        else {
          if (!exists("Data")) Data<-W
          else Data<-cbind(Data,W)
        }
      }
      }
    if (!exists("Data")) Data<-data
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
