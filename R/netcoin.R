## Programs to apply net coin analysis
# Image is a files vector with length and order equal to nrow(nodes). Place as nodes field
# Batch

netCoin<-function (nodes, links = NULL, tree = NULL, name = NULL,
                      label = NULL, size = NULL, color = NULL, shape = NULL, legend = NULL, ntext = NULL, info = NULL,
                      orderA = NULL, orderD = NULL, group = NULL, community = NULL,
                      lwidth = NULL, lweight = NULL, lcolor = NULL, ltext = NULL,
                      nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL, nodeBipolar = FALSE, linkBipolar = FALSE,
                      defaultColor = "#1f77b4", main = NULL, note = NULL, help = NULL, helpOn = FALSE,
                      cex = 1, background = NULL, layout = NULL, controls = c(1,2,3), mode = c("network","heatmap"),
                      showCoordinates = FALSE, showArrows = FALSE, showLegend = TRUE, showAxes = FALSE, showLabels = TRUE, axesLabels = NULL,
                      language = c("en", "es"), image = NULL, imageNames = NULL, dir = NULL, show = TRUE)
{
  if(class(nodes)=="netCoin"){
    links<-nodes$links
    tree<-nodes$tree
    options<-nodes$options
    nodes<-nodes$nodes
    name<-names(nodes)[1]
  }
  else{
    name <- nameByLanguage(name,language,nodes)
    options=list(nodeName=name)
  }
  if(!is.null(links))
    links <- links[links$Source%in%nodes[[name]]&links$Target%in%nodes[[name]],]
  if(!is.null(tree))
    tree <- tree[tree$Source%in%nodes[[name]]&tree$Target%in%nodes[[name]],]
  
  # graph options
  if (!is.null(main)) options[["main"]] <- main
  if (!is.null(note)) options[["note"]] <- note
  if (!is.null(help)) options[["help"]] <- help
  if (!is.null(background)) options[["background"]] <- background
  
  if(nodeBipolar) options[["nodeBipolar"]] <- TRUE
  if(linkBipolar) options[["linkBipolar"]] <- TRUE
  if(helpOn) options[["helpOn"]] <- TRUE
  options[["cex"]] <- as.numeric(cex)
  if (!is.null(defaultColor)) options[["defaultColor"]] <- defaultColor
  if (!is.null(controls)) options[["controls"]] <- as.numeric(controls)
  if (!is.null(mode)) options[["mode"]] <- tolower(substr(as.character(mode),1,1))
  if (!is.null(axesLabels)) options[["axesLabels"]] <- as.character(axesLabels)
  
  if(showCoordinates) options[["showCoordinates"]] <- TRUE
  if(showArrows) options[["showArrows"]] <- TRUE
  if(showLegend) options[["showLegend"]] <- TRUE
  if(showAxes) options[["showAxes"]] <- TRUE
  if(showLabels) options[["showLabels"]] <- TRUE
  
  # node options
  if (!is.null(label)) options[["nodeLabel"]] <- label
  if (!is.null(group)) options[["nodeGroup"]] <- group
  if (!is.null(size)) options[["nodeSize"]] <- size
  if (!is.null(color)) options[["nodeColor"]] <- color
  if (!is.null(shape)) options[["nodeShape"]] <- shape
  if (!is.null(legend)) options[["nodeLegend"]] <- legend
  if (!is.null(ntext)) options[["nodeText"]] <- ntext
  if (!is.null(info)) options[["nodeInfo"]] <- info
  if (!is.null(orderA)) options[["nodeOrderA"]] <- orderA
  if (!is.null(orderD)) options[["nodeOrderD"]] <- orderD
  if(!is.null(imageNames) && !is.null(image)) options[["imageNames"]] <- imageNames
  
  # link options
  if (!is.null(lwidth)) options[["linkWidth"]] <- lwidth
  if (!is.null(lweight)) options[["linkWeight"]] <- lweight
  if (!is.null(lcolor)) options[["linkColor"]] <- lcolor
  if (!is.null(ltext)) options[["linkText"]] <- ltext
  
  # filters
  rownames(nodes) <- nodes[,name]
  nodes$noShow <- FALSE
  if(!is.null(links))
    links$noShow <- FALSE
  if (!is.null(nodeFilter)){
    nodes[,"noShow"] <- !with(nodes,eval(parse(text=nodeFilter)))
    noShowNodes <- as.character(nodes[nodes[,"noShow"],name])
    if(!is.null(links))
      links[(as.character(links[,"Source"]) %in% noShowNodes)|(as.character(links[,"Target"]) %in% noShowNodes),"noShow"] <- TRUE
  }
  
  if (!is.null(links) && !is.null(linkFilter)){
    links[,"noShow"] <- links[,"noShow"] | !with(links,eval(parse(text=linkFilter)))
  }
  
  if (!is.null(degreeFilter)) options[["degreeFilter"]] <- as.numeric(degreeFilter)
  
  net <- structure(list(links=links,nodes=nodes,tree=tree,options=options,call=match.call()),class="netCoin")
  
  #images
  if (!is.null(image)){
    net$nodes[[image]] <- NULL
    net <- netNodeImage(net,nodes[[image]],image)
  }
  
  #layout
  if (!is.null(layout)) {
    if(is.character(layout)){ 
      layoutName <- layoutControl(layout)
      layout <- coords[[layoutName]](toIgraph(net))
      if(layoutName=="su")layout=layout$layout
    }
    if (class(layout)=="matrix")
      net <- netAddLayout(net,layout)
    else warning("layout is not a matrix")
  }
  
  #community
  if (!is.null(community)) {
    commun <- congloControl(community)
    net$nodes$community <- as.character(membership(conglos[[commun]](toIgraph(net))))
    net$options$nodeGroup <- "community"
  }    
  
  if (!is.null(dir)) {
    netCreate(net,language,dir,show)
  }
  return(net)
}

# Program to apply nets to correlations

netCorr<-function(variables, weight=NULL,
                  minimum=-Inf, maximum=Inf, sort=FALSE, decreasing=TRUE,
                  frequency=FALSE, means=TRUE, 
                  method="pearson", criteria="p", Bonferroni=FALSE,
                  minL=0, maxL=Inf,
                  sortL=NULL, decreasingL=TRUE,
                  igraph=FALSE, ...)
{
  arguments <- list(...)
  arguments$name <- nameByLanguage(arguments$name,arguments$language,arguments$nodes)
  if(!("size" %in% names(arguments))) arguments$size <- "mean"
  if(!("lwidth" %in% names(arguments))) arguments$lwidth <- "value"
  if(!("lweight" %in% names(arguments))) arguments$lweight <- "value"
  variables<-na.omit(variables)
  cases<-nrow(variables)
  if (criteria=="p" & maxL==Inf)  maxL<-.5
  if (criteria=="p" & Bonferroni) maxL<-maxL/choose(cases,2)
  if (is.null(arguments$nodes)) {
    arguments$nodes<-data.frame(name=colnames(variables),
                  mean=round(apply(variables,2,mean),2),
                  std=round(sqrt(apply(variables,2,var)),2),
                  min=apply(variables,2,min),
                  max=apply(variables,2,max))
	colnames(arguments$nodes)[1] <- arguments$name
  }
  R<-cor(variables[,arguments$nodes[,2]>=minimum & arguments$nodes[,2]<=maximum],method=method)
  E<-edgeList(R, "shape", min=-1, max=1, directed=FALSE, diagonal=FALSE)
  E$z<-E$value*sqrt(cases)
  E$p<-1-pt(E$z,cases-1)
  E<-E[E[[criteria]]>=minL & E[[criteria]]<=maxL,]
  if (!is.null(sortL)) E<-E[order((-1*decreasingL+!decreasingL)*E[[sortL]]),]
  arguments$links <- E

  xNx <- do.call(netCoin,arguments)
  if (igraph) return(toIgraph(xNx))
  else return(xNx)
}

# Complete netCoin from an incidences matrix

allNet<-function(incidences, weight = NULL, subsample = FALSE,
                 minimum=1, maximum = nrow(incidences), sort = FALSE, decreasing = TRUE,
                 frequency = FALSE, percentages = TRUE, 
                 procedures = "Haberman", criteria = "Z", Bonferroni = FALSE,
                 support = -Inf, minL = -Inf, maxL = Inf,
                 directed = FALSE, diagonal = FALSE, sortL = NULL, decreasingL = TRUE,
                 igraph = FALSE, dir=NULL, ...)
{
  arguments <- list(...)
  arguments$dir<-dir
  if(!("language" %in% names(arguments))) arguments$language <- "en"
  arguments$name <- nameByLanguage(arguments$name,arguments$language,arguments$nodes)
  if(!("size" %in% names(arguments))) arguments$size <- "%"
  incidences<-na.omit(incidences)
  if (all(incidences==0 | incidences==1)) {
    C<-coin(incidences, minimum, maximum, sort, decreasing, weight=weight, subsample=subsample)
    O<-asNodes(C,frequency,percentages,arguments$language)
    if (is.null(arguments$nodes))
      arguments$nodes<-O
    else {
    nodesOrder<-as.character(arguments$nodes[[arguments$name]])
    arguments$nodes<-merge(O,arguments$nodes[,setdiff(names(arguments$nodes),c("frequency","frecuencia","%"))],by.y=arguments$name,all.y=TRUE, sort=FALSE)
    row.names(arguments$nodes)<-arguments$nodes[[arguments$name]]
    arguments$nodes<-arguments$nodes[nodesOrder,]
    }
    procedures<-union(procedures,unlist(arguments[c("lwidth","lweight","lcolor","ltext")]))
    arguments$links<-edgeList(C, procedures, criteria, Bonferroni, minL, maxL, support, 
                              directed, diagonal, sortL, decreasingL)
    for(lattr in c("lwidth","lweight","lcolor","ltext"))
      if(!is.null(arguments[[lattr]])) arguments[[lattr]]<-i.method(c.method(arguments[[lattr]]))
    if(is.character(arguments$layout)){
      if(tolower(substr(arguments$layout,1,2))=="mc")arguments$layout<-layoutMCA(incidences)
      else if(tolower(substr(arguments$layout,1,2))=="pc")arguments$layout<-layoutPCA(C)
    }
    xNx <- do.call(netCoin,arguments)
    if (igraph) return(toIgraph(xNx))
    else return(xNx)
  }
  else warning("Input is not a dichotomous matrix of incidences")
}


# surCoin is a wrapper to build a netCoin object from an original non-dichotomized data.frame. See below dichotomize()

surCoin<-function(data,variables=names(data), commonlabel=NULL,
                  dichotomies=NULL, valueDicho=1, metric=NULL, exogenous=NULL,
                  weight=NULL, subsample=FALSE,
                  minimum=1, maximum=nrow(data), sort=FALSE, decreasing=TRUE,
                  frequency=FALSE, percentages=TRUE,
                  procedures="Haberman", criteria="Z", Bonferroni=FALSE,
                  support=-Inf, minL=-Inf, maxL=Inf,
                  directed=FALSE, diagonal=FALSE, sortL=NULL, decreasingL=TRUE,
                  igraph=FALSE, dir=NULL, ...)
{
  arguments <- list(...)
  
  #Check methods. No necessary because edgeList call these routines.
  #procedures<-i.method(c.method(procedures))
  #criteria<-i.method(c.method(criteria))
  procedures<-union(procedures,unlist(arguments[c("lwidth","lweight","lcolor","ltext")]))
  
  #Metrics 
  if(!is.null(metric)) {
    variables<-setdiff(variables,metric)
    procedures<-intersect(procedures,c("Pearson","Haberman","Z")) #Check in case of NULL result
    criteria<-intersect(criteria,c("Pearson","Haberman","Z")) # Check in case of NULL result
  }
  
  #Names  
  if(!("language" %in% names(arguments))) arguments$language <- "en"
  nodes <- arguments$nodes
  name <- arguments$name <- nameByLanguage(arguments$name,arguments$language,arguments$nodes)
  
  #Data.frame  
  if (all(class(data)==c("tbl_df","tbl","data.frame"))) data<-as.data.frame(data) # convert haven objects
  allvar<-union(union(metric,dichotomies),variables)
  data<-na.omit(data[,union(allvar,weight)])
  if(!is.null(weight)) weight<-data[,weight]
  data<-data[,allvar]
  data[,variables]<-as_factor(data[,variables])
  if(!is.null(nodes)) allvar<-intersect(unlist(nodes[name]),allvar)
  
  
  #Size 
  if(!("size" %in% names(arguments))) arguments$size <- "%"
  
  #Dichotomies    
  if(!is.null(dichotomies)){
    dichos<-dicho(data,dichotomies,valueDicho)
    variables<-setdiff(variables,dichotomies)
  }
  
  #Dichotomize
  if (length(variables)>0){
    incidences<-dichotomize(data, variables, "", min=minimum, length=0, values=NULL, sparse=FALSE, add=FALSE, sort=sort)
    if(!is.null(dichotomies)) incidences<-cbind(dichos,incidences)
  } 
  else if(exists("dichos")) incidences<-dichos
  
  #Nodes filter  
  if (!is.null(nodes)) {
    nonAmong<-setdiff(as.character(nodes[[name]]),names(incidences))
    nodeList<-setdiff(as.character(nodes[[name]]),nonAmong)
    incidences<-incidences[,nodeList]
    nonAmong<-setdiff(nonAmong,metric)
    if(length(nonAmong)>0)
      warning(paste0(toString(nonAmong)," is/are not present among incidences."))
  }
  
  #Nodes elaboration
  if(!exists("incidences")) stop("There are no qualitative variables. Try netCorr.") 
  incidences<-na.omit(incidences)
  if (all(incidences==0 | incidences==1)) {
    C<-coin(incidences, minimum, maximum, sort, decreasing, weight=weight, subsample=subsample)
    O<-asNodes(C,frequency,percentages,arguments$language)
    names(O)[1]<-name
    if(!is.null(nodes)) {
      O<-merge(O,nodes[,setdiff(names(nodes),c("frequency","frecuencia","%"))],by.x=name,all.x=TRUE)
    }else {
      if (!is.null(commonlabel)) { # Preserve the prename (variable) of a node if specified in commonlabel
        ifelse(arguments$language=="es",arguments$label<-"etiqueta",arguments$label<-"label")
        provlabels<-as.character(O[[name]])
        O[[arguments$label]]<-ifelse(substr(O[[name]],1,regexpr('\\:',O[[name]])-1) %in% commonlabel,provlabels,substr(O[[name]],regexpr('\\:',O[[name]])+1,1000000L))
      }
    }
    
    #Links elaboration
    E<-edgeList(C, procedures, criteria, Bonferroni, minL, maxL, support, 
                directed, diagonal, sortL, decreasing)
    for(lattr in c("lwidth","lweight","lcolor","ltext"))
      if(!is.null(arguments[[lattr]])) arguments[[lattr]]<-i.method(c.method(arguments[[lattr]]))
    
    if(!is.null(arguments$layout)) {
      layout2 <- layout <- arguments$layout
      if (class(layout)=="matrix" & is.null(metric)){
        if (!is.null(nodes)){
          if(nrow(layout)==nrow(nodes)){
            Oxy <- matrix(NA,nrow(O),2)
            rownames(Oxy) <- as.character(O[,name])
            rownames(layout) <- as.character(nodes[,name])
            layoutnames <- intersect(rownames(Oxy),rownames(layout))
            Oxy[layoutnames,] <- layout[layoutnames,]
            arguments$layout <- Oxy
          } else warning("layout must have a coordinate per node")
        } else warning("layout must be applied to the nodes variable")
      } else {
        if(is.character(layout)){
          if(tolower(substr(layout,1,2))=="mc")arguments$layout<-layoutMCA(incidences)
          else if(tolower(substr(layout,1,2))=="pc")arguments$layout<-layoutPCA(C)
        }
        else if(!is.null(metric)) arguments$layout<-NULL # There is metric information and not MCA or PCA
      }
    }
    
    arguments$nodes <- O
    arguments$links <- E
    xNx <- do.call(netCoin,arguments)
  }
  else warning("Input is not a dichotomous matrix of incidences")
  
  if(exists("xNx")){
    if(!is.null(metric)) {
      #Nodes filter
      if(!is.null(nodes)){
        metricT<-metric
        nonAmong<-setdiff(as.character(nodes[[name]]),metric)
        metric<-setdiff(as.character(nodes[[name]]),nonAmong)
        nonAmong<-setdiff(metricT,metric)
        if(length(nonAmong)>0)
          warning(paste0(toString(nonAmong)," is/are not present among metrics"))
        O2<-nodes[nodes$name %in% metric,]
      }else{
        if(percentages) xNx$nodes$mean<-xNx$nodes$`%`/100
        xNx$nodes$min<-0
        xNx$nodes$max<-1
        means<-sapply(na.omit(data[,metric]),mean)
        mins<-sapply(na.omit(data[,metric]),min)
        maxs<-sapply(na.omit(data[,metric]),max)
        P<-(means-mins)/(maxs-mins)*100
        O2<-data.frame(name=names(means),mean=means,min=mins,max=maxs,P=P)
        colnames(O2)<-sub("^P$","%",colnames(O2))
      }
      xNx$nodes<-rbind.all.columns(xNx$nodes,O2)
      
      methods<-union(procedures,criteria)
      R<-corr(data[,metric],cbind(incidences,data[,metric]),weight=weight)
      allvar<-union(unlist(nodes[name]),c(names(incidences),metric))
      order1<-intersect(allvar,rownames(R))
      order2<-intersect(allvar,colnames(R))
      R<-R[order1,order2]
      Pearson<-mats2edges(R)
      colnames(Pearson)[3]<-"Pearson"
      D<-as.data.frame(Pearson)
      if("Haberman" %in% methods){
        H<-R*sqrt(nrow(incidences))
        Haberman<-mats2edges(H)
        D<-cbind(D,Haberman[,3]); colnames(D)[length(D)]<-"Haberman"
      }
      if("Z" %in% methods) {
        t<-R/sqrt((1-pmin(1L,R))/(nrow(incidences)-2))
        Z <- mats2edges(1-pt(t,nrow(incidences)-2))
        D<-cbind(D,Z[,3]); colnames(D)[length(D)]<-"Z"
      }
      D<-D[,c("Source","Target",methods)]
      D<-D[D[criteria] > minL & D[criteria] < maxL,]
      colnames(D)<-sub("^Z$","p(Z)",colnames(D))
      if(is.null(xNx$links))xNx$links<-D
      else xNx$links<-rbind.all.columns(arguments$links,D)
      #Layout
      if (class(layout)=="matrix"){
        if (!is.null(nodes)){
          if(nrow(layout2)==nrow(nodes)){
            Oxy <- matrix(NA,nrow(xNx$nodes),2)
            rownames(Oxy) <- as.character(xNx$nodes[,name])
            rownames(layout2) <- as.character(nodes[,name])
            layoutnames <- intersect(rownames(Oxy),rownames(layout2))
            Oxy[layoutnames,] <- layout2[layoutnames,]
            layout2 <- Oxy
          } else warning("layout must have a coordinate per node")
        } else warning("layout must be applied to the nodes variable")
        xNx<-netAddLayout(xNx,layout2)
      }
    }
    if (!is.null(exogenous)) {
      exogenous2<-intersect(exogenous,c(metric,dichotomies))
      xNx$links$chaine<-ifelse(((substr(xNx$links$Source,1,regexpr("\\:",xNx$links$Source)-1) %in% exogenous) |
                                  (xNx$links$Source %in% exogenous2))  &
                                 ((substr(xNx$links$Target,1,regexpr("\\:",xNx$links$Target)-1) %in% exogenous) |
                                    (xNx$links$Target %in% exogenous2)),"No","Yes")
      arguments$linkFilter<-paste(ifelse(is.null(arguments$linkFilter),"",paste(arguments$linkFilter,"&")),"chaine=='Yes'")
      xNx$links[,"noShow"]<-with(xNx$links,!eval(parse(text=arguments$linkFilter)))
    }
    if ("showArrows" %in% names(xNx$options) & exists("nodes")) xNx$links<-orderEdges(xNx$links,nodes[[name]])
    
    if(!is.null(dir)) plot(xNx,dir=dir)
    if (igraph) return(toIgraph(xNx))
    else return(xNx)
  }
}

# Elaborate a netCoin object from a lavaan object.

pathCoin<-function(model, estimates=c("b","se","z","pvalue","beta"), fitMeasures=c("chisq", "cfi", "rmsea"), ...){
  arguments <- list(...)
  if(!("language" %in% names(arguments))) arguments$language <- "en"
  if(!("linkBipolar" %in% names(arguments))) arguments$linkBipolar <- TRUE
  if(!("showArrows" %in% names(arguments))) arguments$showArrows <- TRUE
  arguments$name <- nameByLanguage(arguments$name,arguments$language,arguments$nodes)
  M<-pathParameter(model,estimates=estimates)
  names(M$nodes)[1]<-arguments$name
  
  
  if(!is.null(arguments$note)) arguments$note<-paste0(catFit(model,fitMeasures),arguments$note)
  else if(!is.null(fitMeasures)) arguments$note<-catFit(model,fitMeasures)
  
  if("nodes" %in% names(arguments)) {
    vvnodes<-setdiff(names(arguments$nodes),arguments$name)
    arguments$nodes<-merge(arguments$nodes,M$nodes,by.x=arguments$name,by.y=arguments$name,sort=F)
    arguments$nodes<-arguments$nodes[,c(names(M$nodes),vvnodes)]
  }
  else arguments$nodes<-M$nodes
  arguments$links<-M$links
  
  do.call(netCoin,arguments)
}

# dichotomize: Transform character and factor objects into dichotomies.

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
        if (!exists("R_Data")) R_Data<-Q
        else R_Data<-cbind(R_Data,Q)
      }
      else {
        W <- as.data.frame(t(as.data.frame(as.matrix(Q), row.names=Z)))
        if (add==TRUE) data <- cbind(data,W)
        else {
          if (!exists("R_Data")) R_Data<-W
          else R_Data<-cbind(R_Data,W)
        }
      }
      }
    if (!exists("R_Data")) R_Data<-data
    return(R_Data)
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

# Links. See below funcs="shape"
edgeList <- function(data, procedures="Haberman", criteria="Z", Bonferroni=FALSE, min=-Inf, max=Inf, support=-Inf, 
                     directed=FALSE, diagonal=FALSE, sort=NULL, decreasing=TRUE) {
  if (tolower(substr(criteria,1,2))%in%c("z","hy") & substr(tolower(procedures[1]),1,2)!="sh") {
    if (max==Inf) max<-.50
    if (Bonferroni ) max<-max/choose(nrow(data$f),2) # Changes of Z max criterium (Bonferroni)
  }
  if (substr(tolower(procedures)[1],1,2)!="sh") { # For coin objects
    if (class(data)!="coin") stop("Error: input must be a coin object (see coin function)")
    funcs<-c.method(procedures)
    if(!is.null(sort)) funcs<-union(c.method(sort),funcs)
    criteria<-c.method(criteria)
    todas<-union(funcs,criteria)
    matrices<-sim(data,todas,minimum=min)
    funcs<-i.method(funcs)
    criteria<-i.method(criteria)
    if (length(union(funcs,criteria))==1) {
      M<-new.env()
      M[[funcs]]<-matrices
      matrices<-as.list(M)
    }
    matrices<-matrices[i.method(todas)]
    Mat<-mats2edges(data$f,matrices,criteria=criteria,min=min,max=max,support=support,directed=directed,diagonal=diagonal)
  }
  else {
    if (class(data)!="matrix" & class(data)!="data.frame") 
      stop("Error: input must be a matrix (shape) or a data.frame (tree)")    
    if (class(data)=="matrix"){
      if(min==-Inf)min<-1    
      funcs="value"
      M<-new.env()
      M[[criteria]]<-M[[funcs]]<-data
      matrices<-as.list(M)
      data<-list(f=M[[funcs]],n=NA)
      Mat<-mats2edges(data$f,min=min,max=max,directed=directed,diagonal=diagonal)
    }
    if (class(data)=="data.frame") {
      lines<-sapply(data,as.character)
      lines<-rbind(c(lines[1,1],rep(NA,ncol(lines)-1)),lines) # Add one blank case in order to avoid mXm problem.
      lines<-ifelse(lines=="",NA,lines)
      adjlist<-split(lines,seq(nrow(lines))) # splits the character strings into list with different vector for each line
      adjlist<-sapply(adjlist,na.omit)
      Source=unlist(lapply(adjlist,function(x) rep(x[1],length(x)-1))) # establish first column of edgelist by replicating the 1st element (=ID number) by the length of the line minus 1 (itself)
      Target=unlist(lapply(adjlist,"[",-1)) # the second line I actually don't fully understand this command, but it takes the rest of the ID numbers in the character string and transposes it to list vertically
      return(as.data.frame(cbind(Source,Target),stringsAsFactors = FALSE,row.names=FALSE))
    }
  }  
  
# Last transformations: c.Conditional c.Probable and sort
 
  if(length(Mat)>0) {
    if (!is.null(Mat$c.conditional)) 
      Mat$c.conditional<-factor(Mat$c.conditional,levels=c(0:8),
                                labels=c("Null","Mere","Conditional","Significant","Quite significant","Very significant","Subtotal","Suptotal","Total"))
    if (!is.null(Mat$c.probable)) 
      Mat$c.probable<-factor(Mat$c.probable,levels=c(0:8),
                             labels=c("Null","Mere","Probable","Significant","Quite significant","Very significant","Subtotal","Suptotal","Total"))
    if (!is.null(sort)) {
      if (substr(tolower(procedures)[1],1,2)!="sh") Mat<-Mat[order(Mat[[i.method(c.method(sort))]],decreasing = decreasing),]
      else Mat<-Mat[order(Mat$value,decreasing=decreasing),]
    }
  }
  else return(NULL)
  names(Mat)[names(Mat)=="Z"]<-"p(Z)"
  names(Mat)[names(Mat)=="Fisher"]<-"p(Fisher)"
  return(Mat)
}

c.method<-function(method) {
  if(is.null(method))return(NULL)
  method<-toupper(method)
  if ("ALL"==method[1]) method<-c("M","T","G","S","B","J","D","A","O","K","N","Y","P","V","C","R","E","L","H","Z","f","F","X","I","0","Q","U")
  method<-sub("FIS","W",method)
  method<-sub("HYP","W",method)
  method<-sub("HAM","NNH",method)
  method<-sub("CC" ,"UC" ,method)
  method<-sub("CP" ,"QC" ,method)  
  method<-sub("OD" ,"CD" ,method)
  method<-sub("RO" ,"TA" ,method)
  method<-sub("AND","BER",method)
  method<-sub("TET","VET",method)
  method<-sub("CON","LCO",method)
  method<-sub("II" ,"0"  ,method)
  method<-substr(method,1,1)
  return(method)
}

i.method<-function(method) {
  similarities<-matrix(c("matching","Rogers","Gower","Sneath", "Anderberg",
  "Jaccard","dice", "antiDice","Ochiai","Kulczynski",
  "Hamann", "Yule", "Pearson", "odds", "Russell", "expected", "Haberman", "confidence", "Z",
  "frequencies", "relative", "sConditional","tConditional", "c.conditional","c.probable","tetrachoric","Fisher"), 
   nrow=1, dimnames=list("Similarity",
  c("M","T","G","S","B","J","D","A","O","K","N","Y","P","C","R","E","H","L","Z","F","X","I","0","U","Q","V","W")))
  return(similarities[,method])
}

# Similatities
sim<-function (input,procedures="Jaccard",distance=FALSE, minimum=1, maximum=Inf, sort=FALSE, decreasing=FALSE) {
  method<-c.method(procedures)
  if (is.matrix(input)) {
    if (is.null(colnames(input))) dimnames(input)<-list(NULL,paste("X",1:ncol(input),sep=""))
    input<-as.data.frame(input)
  }
  if (is.data.frame(input)) C<-coin(input,minimum,maximum,sort,decreasing)
  else if (all(objects(input)==c("f","n"))) {
    C<-input
    C$f<-input$f[(diag(input$f)>=minimum & diag(input$f)<=maximum),(diag(input$f)>=minimum &diag(input$f)<=maximum)]
  }
  else return(cat("1st parameter has to be a data frame or a list of coincidences (n and f)"))
  a<-C$f
  N<-C$n
  if (sort==TRUE | decreasing==TRUE) a<-a[order(diag(a),decreasing=decreasing),order(diag(a),decreasing=decreasing)]
  b<--(a-diag(a))
  c<--t((t(a)-diag(a)))
  d=N-a-b-c
  if (any(c("G","B","O","K","Y","P","V") %in% method)) m<-ifelse(a+d==N,1,ifelse(b+c==N,-1,0)) #Special values of distances
  s<-new.env()
  
  if ("M" %in% method) s$matching <- distant((a + d)/(a + b + c + d),distance)
  if ("T" %in% method) s$Rogers <- distant((a + d)/(a + 2 * (b + c) + d), distance)
  if ("G" %in% method) {
    s$Gower <- distant(a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),distance)
    s$Gower <- ifelse(is.na(s$Gower),distant(pmax(m,0),distance),s$Gower)
  }
  if ("S" %in% method) s$Sneath <- distant(2*(a+d)/(2*(a+d)+(b+c)),distance)
  if ("B" %in% method) {
    s$Anderberg <- distant((a/(a+b)+a/(a+c)+d/(c+d)+d/(b+d))/4,distance)
    s$Anderberg <- ifelse(is.na(s$Anderberg),distant(pmax(m,0),distance),s$Anderberg)
  }
  if ("J" %in% method) s$Jaccard <- distant(a/(a + b + c),distance)
  if ("D" %in% method) s$dice <- distant(2 * a/(2 * a + b + c), distance)
  if ("A" %in% method) s$antiDice <- distant(a/(a + 2 * (b + c)),distance)
  if ("O" %in% method) {
    s$Ochiai <- distant(a/sqrt((a + b) * (a + c)),distance)
    s$Ochiai <- ifelse(is.na(s$Ochiai),distant(pmax(m,0),distance),s$Ochiai)    
  }
  if ("K" %in% method) {
    s$Kulczynski <- distant((a/(a+b)+a/(a+c))/2, distance)
    s$Kulczynski <- ifelse(is.na(s$Kulczynski),distant(pmax(m,0),distance),s$Kulczynski)
  }
  if ("N" %in% method) s$Hamann <- distant((a - (b + c) + d)/(a + b + c + d), distance)
  if ("Y" %in% method) {
    s$Yule <- distant((a*d-b*c)/(a*d+b*c))
    s$Yule <- ifelse(is.na(s$Yule),distant(m,distance),s$Yule)
  }
  if ("P" %in% method) {
    s$Pearson <- distant((a * d - b * c)/sqrt((a + b) * (a + c) * (b + d) *  (d + c)),distance)
    s$Pearson <- ifelse(is.na(s$Pearson),distant(m,distance),s$Pearson)
  }
  if ("V" %in% method) {
    s$tetrachoric<-((a*d/(b*c))^(pi/4)-1)/((a*d/(b*c))^(pi/4)+1)
    s$tetrachoric <- ifelse(is.na(s$tetrachoric),distant(m,distance),s$tetrachoric)
  }
  if ("C" %in% method) {
    s$odds <- (pmax(a,.5)*pmax(d,.5))/(pmax(b,.5)*pmax(c,.5))
    if (distance) s$odds<--s$odds
    diag(s$odds)<-ifelse(distance,-Inf,Inf)
  }
  if ("R" %in% method) {
    s$Russell <- distant(a/(a + b + c + d),distance)
    if (!distance) diag(s$Russell) <- 1
  }
  if ("E" %in% method) s$expected <- (a+b)*(a+c)/N
  if ("L" %in% method) {
    signo<-2*(((a+b)*(a+c)/N)<a)-1
    s$confidence <- pmax((a+b)*(a+c)/N+signo*1.64*sqrt(((a+b)*(a+c)/N)*((1-(a+b)/N)*(1-(a+c)/N))),0)
    diag(s$confidence) <- diag(a)
  }
  if ("H" %in% method) {
    s$Haberman <- sqrt(N) * (a * d - b * c)/sqrt((a + b) * (a + c) * (b + d) *  (d + c))
    s$Haberman[is.na(s$Haberman)]<-sqrt(N)
    if (distance) s$Haberman<-(N+s$Haberman)/(2*N)
  }
  if ("Z" %in% method) {
    s$Z <- 1-pt(sqrt(N) * (a * d - b * c)/sqrt((a + b) * (a + c) * (b + d) *  (d + c)),N)
    s$Z[is.na(s$Z)]<-0
  }
  if ("W" %in% method) s$Fisher<-1-phyper(a-1,pmin((a+b),(a+c)),N-pmin((a+b),(a+c)),pmax((a+b),(a+c)))
  if ("x" %in% method) {
    s$Fisher<-matrix(NA,nrow=nrow(a),ncol=ncol(a))
    for (Ro in c(1:nrow(a))) {
      for (Co in c(Ro:ncol(a))) {
        inMatrix=matrix(c(a[Ro,Co],b[Ro,Co],c[Ro,Co],d[Ro,Co]),nrow=2)
        (s$Fisher[Ro,Co]<-fisher.test(inMatrix,alternative="greater")$p.value)
        s$Fisher[Co,Ro] = s$Fisher[Ro,Co]
      }
    }
    # diag(s$Fisher)<-0
    rownames(s$Fisher)<-colnames(s$Fisher)<-rownames(a)
  }
  if ("F" %in% method) s$frequencies <- a
  if ("X" %in% method) s$relative <- a/N*100
  if ("I" %in% method) s$sConditional <-t(a/diag(a)*100)
  if ("0" %in% method) s$tConditional <-a/diag(a)*100
  if ("U" %in% method) {
    Z <- 1-pt(sqrt(N) * (a * d - b * c)/sqrt((a + b) * (a + c) * (b + d) *  (d + c)),N)
    s$c.conditional<-matrix(ifelse(b+c==0, 8,
                             ifelse(c==0,  7,                        
                              ifelse(b==0,  6,
                               ifelse(Z<.001,5,
                                ifelse(Z<.01, 4,
                                 ifelse(Z<.05, 3,
                                  ifelse(Z<.50, 2,       
                                   ifelse(a>0, 1, 0)))))))),nrow=nrow(a),dimnames=dimnames(a))
  }
  if ("Q" %in% method) {
    Z <- 1-pt((a/(a+c)-.50)/(1/(2*sqrt(a+c))),(a+c))
    s$c.probable<-matrix(ifelse(b+c==0, 8,
                                ifelse(c==0,   7,                        
#                                ifelse(b==0,  "Subtotal",
                                  ifelse(Z<.001, 5,
                                   ifelse(Z<.01,  4,
                                    ifelse(Z<.05,  3,
                                     ifelse(Z<.50,  2,      
                                      ifelse(a>0, 1, 0))))))),nrow=nrow(a),dimnames=dimnames(a))
  }  
  if (length(method)==1) return(s[[names(s)[1]]])
  else return(as.list(s,sorted=TRUE))
}

# Convert similatities into dissimilarities
distant<-function(s,t=FALSE) {
  if (t==TRUE) s<-as.dist(1-s)
  return(s)
}
# http://pbil.univ-lyon1.fr/ade4/ade4-html/dist.binary.html


# Print lower matrices

lower<-function(matrix,decimals=3) { # Add an option to hiden diagonal
  m<-as.matrix(matrix)
  form=paste("%1.",decimals,"f",sep="")
  lower<-apply(m,1,function(x) sprintf(form,x))
  lower[upper.tri(lower)]<-""
  lower<-as.data.frame(lower, stringsAsFactors=FALSE)
  if (ncol(m)==1) rownames(lower)<-colnames(lower)<-names(matrix)
  rownames(lower)<-names(lower)
  return(lower)
}

# List of coincidences

coin<-function(incidences,minimum=1, maximum=nrow(incidences), sort=FALSE, decreasing=TRUE,
               total=FALSE, subsample=FALSE, weight=NULL) {
  incidences<-na.omit(incidences)
  if (subsample){
    vector<-apply(incidences,1,sum)
    incidences<-incidences[vector>0,]
  }
  if (total & is.null(weight)) incidences<-data.frame(Total=1,incidences)
  if (all(incidences==0 | incidences==1)) {
    n<-nrow(incidences)
    names(n)<-"n"
    if (is.null(weight)) f<-crossprod(as.matrix(incidences))
    else {
      if (length(weight)!=dim(incidences)[1]) warning("weight has not the appropiate length!")
        f<-crossprod(t(crossprod(as.matrix(incidences),diag(weight,length(weight)))),as.matrix(incidences))
        n<-maximum<-sum(weight)
    }    
    if (is.null(colnames(f))) dimnames(f)<-list(paste("X",1:ncol(f),sep=""),paste("X",1:ncol(f),sep=""))
    d<-diag(f)
    if (sort) d<-sort(d,decreasing=decreasing)
    S<-names(d[(d>=minimum &  d<=maximum)])
    if (total & is.null(weight)) S<-c("Total",S)
    if (total & !is.null(weight)) warning("total cannot be applied in weighted tables")
    if (length(S)>0) structure(list(n=n,f=f[S,S]),class="coin")
    else cat("No variables left")
  }
  else warning("All data in incidence matrix has to be dichotomous.")
}

coocur<-function (ocurrences, minimum = 1, maximum = Inf, sort = FALSE, decreasing=TRUE) 
{
  result <- matrix(nrow = ncol(ocurrences), ncol = ncol(ocurrences), dimnames = list(colnames(ocurrences), 
                                                                       colnames(ocurrences)))
  for (val in c(1:nrow(result))) {
    for (cal in c(val:ncol(result))) {
      result[cal, val] <- sum(pmin(ocurrences[, cal], ocurrences[, val]))
      if (val != cal) 
        result[val, cal] = result[cal, val]
    }
  }
  d<-diag(result)
  if (sort) d<-sort(d,decreasing=decreasing)
  S<-names(d[(d>=minimum &  d<=maximum)])
  if (length(S)>=1) {
  result<-result[S,S]
  if (length(S)==1) names(result)<-S
  n <- sum(ocurrences[,S])
  m <- sum(apply(as.matrix(ocurrences[,S]), 1, max))
  attr(result, "n") <- n
  attr(result, "m") <- m
  structure(result, class = "cooc")
  }
  else cat("No variables left")
}

print.coin<-function(x, ...) {
  cat("n= ",x$n,"\n",sep="")
  print(lower(x$f,0))
}

print.cooc<-function(x, ...) {
  cat("n= ",attr(x,"n"),"; m= ", attr(x,"m"),"\n",sep="")
  print(lower(x,0))
}

print.netCoin<-function(x, ...) {
  printNet(x)
}

print.barCoin<-function(x, ...) {
  printNet(x)
}

print.timeCoin<-function(x, ...) {
  printNet(x)
}

printNet <- function(x){
  if(!is.null(x$options$main))
    cat("Title:",x$options$main,"\n")
    cat("\nNodes(",nrow(x$nodes),"):\n",sep="")
  row.names(x$nodes)<-NULL
  print(as.data.frame(head(x$nodes[,setdiff(names(x$nodes),c("noShow","fixed","chaine","x","y")),drop=FALSE])),row.names=F)
  if (nrow(x$nodes)>6) cat("...\n")
  if(!is.null(x$links)){
    cat("\nLinks(",nrow(x$links),"):\n",sep="")
    row.names(x$links)<-NULL
    print(as.data.frame(head(x$links[,setdiff(names(x$links),c("noShow","fixed","chaine"))])),row.names=F)
    if (nrow(x$links)>6) cat("...\n")
  }
  cat("\n")
  if(!is.null(x$options$note)){
    cat(x$options$note)
    cat("\n")
  }
}

print <- function(x, ...) UseMethod("print")

plot.coin <- function(x, dir=tempdir(), language=c("en","es"), ...){
  N <- asNodes(x)
  E <- edgeList(x,"frequency","expected")
  cbarCoin(N, E, language = language, dir = dir)
}

plot.netCoin <- function(x, dir=tempdir(), language=c("en","es"), ...){
     netCreate(x,language,dir,TRUE)
}

plot.barCoin <- function(x, dir=tempdir(), language=c("en","es"), ...){
     barCreate(x,language,dir,TRUE)
}

plot.timeCoin <- function(x, dir=tempdir(), language=c("en","es"), ...){
     timeCreate(x,language,dir,TRUE)
}

plot <- function(x, ...) UseMethod("plot")

summary.coin <- function(object, ...){
  cat(object$n,"scenarios and", dim(object$f)[1], "events\n")
  diag(object$f)/object$n
}

summary.netCoin <- function(object, ...){
  summaryNet(object)
}

summary.barCoin <- function(object, ...){
  summaryNet(object)
}

summary.timeCoin <- function(object, ...){
  cat(dim(object$nodes)[1], "categories.\n")
  cat(object$options$start, "'s distribution:","\n",sep="")
  print(summary(object$nodes[[object$options$start]]))
  cat(object$options$end, "'s distribution:","\n",sep="")
  print(summary(object$nodes[[object$options$end]]))
}

summaryNet <- function(x){
  cat(dim(x$nodes)[1], "nodes and", dim(x$links)[1], "links.\n")
  if(any(c("frequency","frecuencia","%") %in% names(x$nodes))) {
    F <- "frecuencia"
    if("frequency" %in% names(x$nodes))
	  F <- "frequency"
    if("%" %in% names(x$nodes))
	  F <- "%"
    cat(F," distribution of nodes:","\n", sep="")
    print(summary(x$nodes[[F]]))
  }
  if(!is.null(x$options$linkWidth)){
    cat(x$options$linkWidth, "'s distribution:","\n",sep="")
    print(summary(x$links[[x$options$linkWidth]]))
  }
  else {
    cat(names(x$links)[3], "'s distribution of links:","\n",sep="")
    print(summary(x$links[[3]]))
       }
}

summary <- function(object, ...) UseMethod("summary")

# Transform a coin object into a data frame with name and frequency
asNodes<-function(C,frequency=TRUE,percentages=FALSE,language="en"){
  if (class(C)=="coin") {
    if (!percentages & frequency) nodes<-data.frame(name=as.character(colnames(C$f)),frequency=diag(C$f))
    else if (!frequency & percentages) nodes<-data.frame(name=as.character(colnames(C$f)),"%"=diag(C$f)/C$n*100,check.names=FALSE)
    else if (percentages & frequency)nodes<-data.frame(name=as.character(colnames(C$f)),frequency=diag(C$f), "%"=diag(C$f)/C$n*100,check.names=FALSE)
    else nodes<-data.frame(name=as.character(colnames(C$f)),check.names=FALSE)
    if (language=="es") {
       colnames(nodes)[colnames(nodes)=="frequency"]<-"frecuencia"
       colnames(nodes)[colnames(nodes)=="name"]<-"nombre"
    }       
  }
  else if (min(c("Source", "Target") %in% names(C))) nodes<-data.frame(name=sort(union(C$Source,C$Target)))
  else warning("Is neither a coin object or an edge data frame")
  return(nodes)
}

rescale <- function(x) {
  to <- c(0, 1)
  from <- range(x, na.rm = TRUE, finite = TRUE)
  return((x - from[1]) / diff(from) * diff(to) + to[1])
}

toColorScale <- function(items){
  if(is.numeric(items)){
    return(hsv(1,1,rescale(items)))
  }else{
	colors <- c(
  "#1f77b4", # blue
  "#2ca02c", # green
  "#d62728", # red
  "#9467bd", # purple
  "#ff7f0e", # orange
  "#8c564b", # brown
  "#e377c2", # pink
  "#7f7f7f", # grey
  "#bcbd22", # lime
  "#17becf", # cyan
  "#aec7e8", # light blue
  "#98df8a", # light green
  "#ff9896", # light red
  "#c5b0d5", # light purple
  "#ffbb78", # light orange
  "#c49c94", # light brown
  "#f7b6d2", # light pink
  "#c7c7c7", # light grey
  "#dbdb8d", # light lime
  "#9edae5" # light cyan
     )
	items <- as.numeric(as.factor(items))
	if(max(items)>length(colors))
	  return("black")
	else
	  return(colors[items])
  }
}

toIgraph <- function(net){
  if (class(net)=="netCoin"){
    nodes <- net$nodes
    links <- net$links
    options <- net$options
    
    if(exists("showArrows",net$options)) directed<-net$show$Arrows else directed=FALSE
    g <- graph.empty(0, directed)
    
    if(!is.null(options[["nodeColor"]])){
      nodes[,options[["nodeColor"]]] <- toColorScale(nodes[,options[["nodeColor"]]])
    }
    if(!is.null(options[["linkColor"]])){
      links[,options[["linkColor"]]] <- toColorScale(links[,options[["linkColor"]]])
    }
    
    for(i in seq_len(nrow(nodes))){
      attr <- list()
      if(!is.null(options[["nodeLabel"]]))
        attr[['label']] <- as.character(nodes[i,options[["nodeLabel"]]])
      else
        attr[['label']] <- as.character(nodes[i,options[["nodeName"]]])
      if(!is.null(options[["nodeColor"]]))
        attr[['color']] <- as.character(nodes[i,options[["nodeColor"]]])
      if(!is.null(options[["nodeSize"]]))
        attr[['size']] <- nodes[i,options[["nodeSize"]]]
      if(!is.null(options[["nodeShape"]]))
        attr[['shape']] <- as.character(nodes[i,options[["nodeShape"]]])
      
      others<-setdiff(names(nodes),union(options[["nodeName"]],c(
        options[["nodeColor"]],
        options[["nodeSize"]],
        options[["nodeShape"]],
        "x","y")))
      for(j in others){
        attr[[j]]<-nodes[[i,j]]
      }
      if(exists("x",nodes) & exists("y",nodes)){
        attr[['x']]<-nodes[i,"x"]
        attr[['y']]<-nodes[i,"y"]
      }
      g <- add.vertices(g, 1, attr = attr)
    }
    
    for(i in seq_len(nrow(links))){
      edges <- c(which(as.character(links[i,'Source'])==as.character(nodes[,net$options$nodeName])),which(as.character(links[i,'Target'])==as.character(nodes[,net$options$nodeName])))
      attr <- list()
      if(!is.null(options[["linkWeight"]]))
        attr[['weight']] <- links[i,options[["linkWeight"]]]
      if(!is.null(options[["linkWidth"]]))
        attr[['width']] <- links[i,options[["linkWidth"]]]
      if(!is.null(options[["linkColor"]]))
        attr[['color']] <- as.character(links[i,options[["linkColor"]]])
      if(!is.null(options[["linkText"]]))
        attr[['label']] <- as.character(links[i,options[["linkText"]]])
      others<-setdiff(names(links),c(
        "Source","Target",
        options[["linkWeight"]],
        options[["linkWidth"]],
        options[["linkText"]]))
      for(j in others){
        attr[[j]]<-links[[i,j]]
      }
      
      g <- add.edges(g, edges, attr = attr)
    }
    
    return(g)
  }
  else warning("Is not a netCoin object")
}

savePajek<-function(net, file="file.net", arcs=NULL, edges=NULL, partitions= NULL, vectors=NULL){
  if(length(setdiff(partitions,names(net[["nodes"]])))>0) stop("At least one partition is not amongst ",paste(names(net$nodes),collapse=", "),".")
  if(length(setdiff(vectors,names(net[["nodes"]])))>0) stop("At least one vector is not amongst ",paste(names(net$nodes),collapse=", "),".")
  if(length(setdiff(arcs,names(net[["links"]])))>0) stop("At least one arc is not amongst ",paste(names(net$links),collapse=", "),".")
  if(length(setdiff(edges,names(net[["links"]])))>0) stop("At least one edge is not amongst ",paste(names(net$links),collapse=", "),".")
  
  if(!grepl("\\.",file))file<-paste0(file,".net")
  if(!is.null(vectors) | !is.null(partitions)) file<-gsub(".net",".paj",file)
  connec<-file(file,"w")
  writeLines(paste0("*Network ",net[["options"]]$main),con=connec)
  close(connec)
  connec<-file(file,"a")
  writeLines(paste0("*Vertices ",as.character(nrow(net[["nodes"]]))),con=connec)
  writeLines(paste0(seq(1:nrow(net[["nodes"]])),' "',net[["nodes"]]$name,'" '), con=connec)
  N<-cbind(n=seq(1:nrow(net[["nodes"]])),net[["nodes"]][1])
  L<-cbind(N[unlist(net[["links"]]$Source),1],N[unlist(net[["links"]]$Target),1])
  
  if(!is.null(arcs)) {
    cont=1
    for(weights in arcs) {
      writeLines(paste0("*Arcs : ",cont,' "',weights,'"'), con=connec)
      writeLines(paste(L[,1],L[,2],net[["links"]][[weights]]), con=connec)
      cont=cont+1
    }
  }
  if(!is.null(edges)) {
    ifelse(exists("cont"),cont<-cont,cont<-1)
    for(weights in edges) {
      writeLines(paste0("*Edges : ",cont,' "',weights,'"'), con=connec)
      writeLines(paste(L[,1],L[,2],net[["links"]][[weights]]), con=connec)
      cont=cont+1
    }
  }
  if(!is.null(partitions)){
    for(partition in partitions) {
      writeLines(paste0("*Partition ", partition), con=connec)
      writeLines(paste0("*Vertices ", nrow(net$nodes)), con=connec)
      writeLines(as.character(as.numeric(as.factor(net[["nodes"]][[partition]]))),con=connec)
    }
  }
  if(!is.null(vectors)){
    for(vector in vectors) {
      writeLines(paste0("*Vector ", vector), con=connec)
      writeLines(paste0("*Vertices ", nrow(net$nodes)), con=connec)
      Line<-as.character(net[["nodes"]][[vector]])
      Line[is.na(Line)]<-"0"
      writeLines(Line, con=connec)
    }   
  }
  close(connec)
}

expectedList<- function(data, names=NULL, min=1, confidence=FALSE) {
  if (class(data)!="coin") stop("Error: input must be a coin object")
  if (!is.null(names)) colnames(data$f)<-rownames(data$f)<-names
  a<-data$f
  b<--(a-diag(a))
  c<--t((t(a)-diag(a)))
  d=data$n-a-b-c
  data$e<-(a+b)*(a+c)/(a+b+c+d)
  E<-edgeList(data$e,"shape",min=0,max=Inf)
  F<-edgeList(data$f,"shape",min=0,max=Inf)
  if (!confidence) {
    dataL<-cbind(F,E[,3])[F[,3]>=min,]
    colnames(dataL)[3:4]<-c("coincidences","expected")
  }
  else {
    N<-a+b+c+d
    signo<-2*(((a+b)*(a+c)/N)<a)-1
    data$l <- pmax((a+b)*(a+c)/N+signo*1.64*sqrt(((a+b)*(a+c)/N)*((1-(a+b)/N)*(1-(a+c)/N))),0)
    diag(data$l) <- diag(a)
    L<-edgeList(data$l,"shape",min=-Inf,max=Inf)
    dataL<-cbind(F,E[,3],L[,3])[F[,3]>=min,]
    colnames(dataL)[3:5]<-c("coincidences","expected","confidence")
  }
  return(dataL)
}

mats2edges<-function(data,list=NULL,criteria=1,min=-Inf,max=Inf,support=-Inf,directed=FALSE,diagonal=FALSE){
  
  # Input control
  if (!is.null(list)) {
    if (!identical(dim(data),dim(list[[1]]))) {
      warning("data & list must have the same dimensions")
      return()
    }
    if (criteria!=1 & !(criteria %in% names(list))) warning("criteria are not in the matrices list")
  }
  if (is.null(rownames(data))) rownames(data)<-as.character(1:nrow(data))
  if (is.null(colnames(data))) colnames(data)<-as.character(1:ncol(data))
  
  #  type of edgelist
  if (nrow(data)<ncol(data)) { # For asymmetric matrices. Improve later for directed cases.
    l<-data>-Inf
    l[,rownames(l)]<-lower.tri(l[,rownames(l)])
  }
  else { # For symmetric matrices
    if (directed) l <- as.vector(lower.tri(data,diag=diagonal) | upper.tri(data))
    else l <- as.vector(lower.tri(data,diag=diagonal))
  }
  # data.frame building
  sources<-rep(colnames(data),each=dim(data)[1])
  targets<-rep(rownames(data),dim(data)[2])
  Mat <- data.frame(Source=sources,Target=targets)
  value <- as.vector(data)
  
  # data.frame for matrices list
  if (!is.null(list)) {
    c <- as.vector(list[[criteria]])
    a<-as.data.frame(lapply(list,as.vector))
    Mat<-cbind(Mat,a)[l==TRUE & c<=max & c>=min & value>=support,]
  }
  # data.frame for alone matrix
  else Mat<-{
    if (criteria!=1) warning("Criteria don't apply for only one matrix. Use min and max")
    cbind(Mat,value)[l==TRUE & value<=max & value>=min,]
  }
  # return
  if (nrow(Mat)>0) {
    row.names(Mat)<-NULL
    return(Mat)
  }
  else return(NULL)
}

orderEdges<-function(links,nodes){ #Used in surCoin to order arrows
  A<-unlist(sapply(paste0("^",links[,"Source"],"$"),grep,x=nodes))
  B<-unlist(sapply(paste0("^",links[,"Target"],"$"),grep,x=nodes))
  links[A>B,c("Source","Target")]<-links[A>B,c("Target","Source")]
  links<-links[!is.na(links$Source) & !is.na(links$Target),]
  return(links)
}

coords<-list(
  da=function(...)layout.davidson.harel(...),
  dr=function(...)layout.drl(...),
  ci=function(...)layout.circle(...),
  fr=function(...)layout.fruchterman.reingold(...),
  ge=function(...)layout.gem(...),
  gr=function(...)layout.grid(...),
  ka=function(...)layout.kamada.kawai(...),
  lg=function(...)layout.lgl(...),
  md=function(...)layout.mds(...),
  ra=function(...)layout.random(...),
  re=function(...)layout.reingold.tilford(...),
  st=function(...)layout.star(...),
  su=function(...)layout.sugiyama(...)
)

conglos<-list(
  ed=function(...)cluster_edge_betweenness(...),
  fa=function(...)cluster_fast_greedy(...),
  la=function(...)cluster_label_prop(...),
  le=function(...)cluster_leading_eigen(...),
  lo=function(...)cluster_louvain(...),
  op=function(...)cluster_optimal(...),
  sp=function(...)cluster_spinglass(...),
  wa=function(...)cluster_walktrap(...)
)

layoutControl<-function(layout){
  if (!is.null(layout)){
    layouts<-c("da","dr","ci","fr","ge","gr","ka","lg","md","ra","re","st","su")
    layout<-(tolower(substr(layout,1,2)))
    if (layout %in% layouts) return (layout)
    else {
      text<-paste(layout, "is not a valid layout")
      warning(text)
      return(NULL)
    }
  }
}

congloControl<-function(conglo){
  if (!is.null(conglo)){
    conglos<-c("ed","fa","la","le","lo","op","sp","wa")
    conglo<-(tolower(substr(conglo,1,2)))
    if (conglo %in% conglos) return (conglo)
    else {
      text<-paste(conglo, "is not a valid layout")
      warning(text)
      return(NULL)
    }
  }
}

dicho<-function(input,variables,value) {
  datum<-as.data.frame(ifelse(input[,variables]==value,1,0))
  if (all(class(input)==c("tbl_df","tbl","data.frame"))) 
    names(datum)<-sapply(input[,variables],attr,"label")
  return(datum)
}

nameByLanguage <- function(name,language,nodes){
  if(is.null(name)){
    if(!is.null(language) && language[1]=="es")
	  name <- "nombre"
	else
	  name <- "name"
  }
  if(!is.null(nodes) & !(name %in% colnames(nodes))) warning(paste0("name: '",name,"' column missing in nodes data frame"))
  return(name)
}

layoutCircle <- function(N,nodes,deg=0,name=NULL){
  N[,"x"] <- NA
  N[,"y"] <- NA

  if(!is.null(name)) rownames(N) <- N[[name]]
  if(is.numeric(nodes)||is.character(nodes)){
  if(is.numeric(nodes)) nodes <- intersect(nodes,seq_len(nrow(N)))
  if(is.character(nodes)) nodes <- intersect(nodes,rownames(N))
  if(length(nodes)<1){
    warning("nodes: length of items present in 'N' must be greater than zero")
  }else if(length(nodes)==1){
    N[nodes,"x"] <- 0
    N[nodes,"y"] <- 0
  }else{
    angle <- (seq_along(nodes) / (length(nodes)/2)) * pi
	angle <- angle - angle[1] + (deg * pi / 180)
    N[nodes,"x"] <- round(cos(angle),3)
    N[nodes,"y"] <- round(sin(angle),3)
  }
  }else
    warning("nodes: must be numeric or character")

  return(as.matrix(N[,c("x","y")]))
}

layoutGrid <- function(N,string,name=NULL){
  N[,"x"] <- NA
  N[,"y"] <- NA

  if(!is.null(name)) rownames(N) <- N[[name]]
  if(is.character(string)){
    Levels <- as.list(unlist(strsplit(string,"\\.")))
	xlen <- length(Levels)
	ylen <- 0
	for(i in seq_len(xlen)){
	  a <- gsub("\\*","",Levels[[i]])
	  a <- gsub(";",",,",a)
      addToEnd <- substr(a,nchar(a),nchar(a))==","
	  a <- as.list(unlist(strsplit(a,"\\,")))
      if(addToEnd) a[[length(a)+1]] <- ""
	  len <- length(a)
	  if(len>ylen)
	    ylen <- len
	  for(j in seq_len(len)){
	    index <- a[[j]]
	    if(index!=""){
	      N[index,"x"] <- i - 1
		  N[index,"y"] <- j - 1
		}
	  }
      Levels[[i]] <- a
	}
    for(i in seq_len(xlen)){
	  len <- length(Levels[[i]])
	  indices <- unlist(Levels[[i]])
	  indices <- indices[indices!=""]
	  if(len!=ylen){
        N[indices,"y"] <- N[indices,"y"] + ((ylen - len)/2)
	  }
    }
    N[,"x"] <- N[,"x"]/(xlen-1)
	N[,"y"] <- 1 - (N[,"y"]/(ylen-1))
  }else
    warning("string: must be character")

  mat <- as.matrix(N[,c("x","y")])
  mat[is.nan(mat)] <- 0
  return(mat)
}

rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

pathParameter<-function(model,estimates=c("b","se","z","pvalue","beta")){
  if(class(model)=="lavaan"){
    links<-lavaan::parameterEstimates(model,standardized = T)
    names(links)<-gsub("^est$","b",names(links))
    names(links)<-gsub("^std.all$","beta",names(links))
    links<-links[links$op=="~",c("rhs","lhs",estimates)]
    names(links)[1:2]<-c("Source","Target")
    if(length(intersect(unique(union(links$Source,links$Target)),model@Data@ov$name))>0) {
      nodes<-as.data.frame(model@Data@ov,stringsAsFactors=F)[,intersect(names(model@Data@ov),c("name","mean","var"))]
      nodes$stdev<-sqrt(nodes$var)
      row.names(nodes)<-nodes$name
    }
    else {
      nodes<-data.frame(name=unique(union(links$Source,links$Target)))
    }
    # nodes<-nodes[,c("name","mean","stdev")]
    nodes$name<-iconv(nodes$name,"","UTF-8")
    links<-links[,c("Source","Target",estimates)]
    links$Source<-iconv(links$Source,"","UTF-8")
    links$Target<-iconv(links$Target,"","UTF-8")  
    structure(list(links = links, nodes = nodes))
  }
  else stop("Model has to be a lavaan object")
}

catFit<-function(model,fitMeasures){
  if("chisq" %in% fitMeasures) fitMeasures<-union(fitMeasures,c("df","pvalue"))
  fit<-lavaan::fitMeasures(model,fitMeasures)
  text<-NULL
  if("chisq" %in% names(fit)) text<-paste(text, paste0("Chi2=",format(fit["chisq"],digits=4)," (",fit["df"]," df)",", pvalue=", format(fit["pvalue"],digits=2)), sep=". ")
  if("cfi" %in% names(fit)) text<-paste(text, paste0("CFI= ",format(fit["cfi"],digits=3)), sep=". ") 
  if("rmsea" %in% names(fit)) text<-paste(text, paste0("RMSEA= ",format(fit["rmsea"],digits=3)), sep=". ")
  paste0(gsub("^. ","<p>",text),".</p>")
}

corr <- function (a, b = a, weight = NULL )
{
  if (is.null(weight)) weight= rep(1/nrow(a), nrow(a))
  s<-complete.cases(cbind(a,b,weight))
  a<-as.matrix(a[s,]);b<-as.matrix(b[s,])
  # normalize weights
  weight <- weight[s] / sum(weight[s])
  
  # center matrices
  a <- sweep(a, 2, apply((a * weight),2,sum))
  b <- sweep(b, 2, apply((b * weight),2,sum))
  
  # compute weighted correlatio
  t(a*weight) %*% b / sqrt(apply((a**2 *weight),2,sum) %*% t(apply((b**2 *weight),2,sum))) 
  
}

layoutMCA<-function(matrix) { # Correspondencias simples clasicas aplicadas a dicotomicas.
  matrix<-cbind(matrix,1-matrix)
  n<-sum(matrix)
  P=matrix/n
  column.masses<-colSums(P)
  row.masses=rowSums(P)
  E=row.masses %o% column.masses
  R=P-E
  I=R/E
  Z=R/sqrt(E) # Corrected
  SVD=svd(Z)
  rownames(SVD$v)=colnames(P)
  standard.coordinates.columns = sweep(SVD$v[1:(ncol(Z)/2),1:2], 1, sqrt(column.masses[1:(ncol(Z)/2)]), "/")
  principal.coordinates.columns = sweep(standard.coordinates.columns, 2, SVD$d[1:2], "*")
  colnames(principal.coordinates.columns)<-c("F1","F2")
  return(principal.coordinates.columns)
}

layoutPCA<-function(coin) { # Coordenadas a partir de Pearson: Haberman/raiz(n)
  A<-eigen(sim(coin,"P"))
  C<-sweep(A$vectors[,1:2],2,sqrt(A$values[1:2]),"*")
  rownames(C)<-rownames(coin$f)
  colnames(C)<-c("F1","F2")
  return(C)
}

mobileEdges<-function(data, name=1, number=2, difference=0) {
  if(!is.numeric(data[[number]])) data[[number]]<-as.numeric(paste(data[[number]]))
  DC<-matrix(NA,nrow=nrow(data),ncol=nrow(data))
  colnames(DC)<-rownames(DC)<-data$name
  for(i in 1:nrow(data))DC[i,]=ifelse(abs(data[[number]][i]-t(data[[number]]))<=difference,(1+difference-abs(data[[number]][i]-t(data[[number]]))),0)
  diag(DC)<-0
  DCLinks<-edgeList(DC,"shape",min=1)
  colnames(DCLinks)[3]<-"sim."
  DCLinks$dist.<-(1+difference-DCLinks$sim.)
  return(DCLinks)
}

incTime<-function(data, name="name", beginning="birth", end="death") {
  if(!is.integer(data[[beginning]])) data[[beginning]]<-as.integer(paste(data[[beginning]]))
  if(!is.integer(data[[end]])) data[[end]]<-as.integer(paste(data[[end]]))
  anos<-min(na.omit(data[[beginning]])):max(na.omit(data[[end]]))
  E<-matrix(NA,nrow=nrow(data),ncol=length(anos))
  colnames(E)<-anos
  for(i in 1:nrow(data)) E[i,]<-ifelse(anos>=data[[beginning]][i] & (anos<=data[[end]][i] | is.na(data[[end]][i])),1,0)
  Datos<-as.data.frame(t(E))
  colnames(Datos)<-data[[name]]
  return(Datos)
}
