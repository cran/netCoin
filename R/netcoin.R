## Programs to apply net coin analysis
# Image is a files vector with length and order equal to nrow(nodes). Place as nodes field
# Batch
netCoin<-function (nodes, links, name="name",
                   label = NULL, size = NULL, color = NULL, shape = NULL, group = NULL, ntext = NULL, 
                   width = NULL, weight = NULL, lcolor = NULL, ltext = NULL,
                   nodeFilter = NULL, linkFilter = NULL,
                   main = NULL, minor = NULL, help = NULL,
                   layout = NULL, language = c("en", "es"), 
                   image = NULL,  dir = NULL)
{
  links<-links[links$source%in%nodes[[name]]&links$target%in%nodes[[name]],]
  options=list(nodeName=name)

  # graph options
  if (!is.null(main)) options[["main"]] <- main
  if (!is.null(minor)) options[["minor"]] <- minor
  if (!is.null(help)) options[["help"]] <- help

  # node options
  if (!is.null(label)) options[["nodeLabel"]] <- label
  if (!is.null(group)) options[["nodeGroup"]] <- group
  if (!is.null(size)) options[["nodeSize"]] <- size
  if (!is.null(color)) options[["nodeColor"]] <- color
  if (!is.null(shape)) options[["nodeShape"]] <- shape
  if (!is.null(ntext)) options[["nodeText"]] <- ntext

  # link options
  if (!is.null(width)) options[["linkWidth"]] <- width
  if (!is.null(weight)) options[["linkWeight"]] <- weight
  if (!is.null(lcolor)) options[["linkColor"]] <- lcolor
  if (!is.null(ltext)) options[["linkText"]] <- ltext

  # filters
  if (!is.null(nodeFilter)) 
    nodes$noShow <- !with(nodes,eval(parse(text=nodeFilter)))

  if (!is.null(linkFilter))
    links$noShow <- !with(links,eval(parse(text=linkFilter)))

  net <- structure(list(links=links,nodes=nodes,options=options,call=match.call()),class="network")

  #images
  if (!is.null(image)) 
    net <- network.nodeImage(net,nodes[[image]],image)

  #layout
  if(!is.null(layout))
    net <- network.addLayout(net,layout)

  if (!is.null(dir)) network.create(net,dir=dir,language=language)
  return(net)
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
    matrices<-sim(data,todas)
    funcs<-i.method(funcs)
    criteria<-i.method(criteria)
    if (length(union(funcs,criteria))==1) {
      M<-new.env()
      M[[funcs]]<-matrices
      matrices<-as.list(M)
    }
    Mat<-mats2edges(data$f,matrices,criteria=criteria,min=min,max=max,support=support,directed=directed,diagonal=diagonal)
  }
  # This is for the case of having  only one matrix to be converted in edge form [funcs="shape"]
  else {
    if (class(data)!="matrix") stop("Error: input must be a matrix")
    if(min==-Inf)min<-1    
    funcs="value"
    M<-new.env()
    M[[criteria]]<-M[[funcs]]<-data
    matrices<-as.list(M)
    data<-list(f=M[[funcs]],n=NA)
    Mat<-mats2edges(data$f,min=min,max=max,directed=directed,diagonal=diagonal)
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
  return(Mat)
}

c.method<-function(method) {
  method<-toupper(method)
  if ("ALL"==method[1]) method<-c("J","M","A","T","D","N","O","G","P","R","B","S","K","Y","C","E","H","L","Z","W","F","I","X","Q","U","V")
  method<-sub("CC","UC"  ,method)
  method<-sub("CP","QC"  ,method)  
  method<-sub("OD","CD"  ,method)
  method<-sub("RO","TA"  ,method)
  method<-sub("AND","BER",method)
  method<-sub("HAM","NNH",method)
  method<-sub("TET","VET",method)
  method<-sub("HY" ,"W"  ,method)
  method<-sub("CON","LCO",method)
  method<-toupper(substr(method,1,1))
  return(method)
}

i.method<-function(method) {
  similarities<-matrix(c("matching","Rogers","Gower","Sneath", "Anderberg",
                         "Jaccard","dice", "antiDice","Ochiai","Kulczynski",
                         "Hamann", "Yule", "Pearson", "odds", "Russell", "expected", "Haberman", "confidence", "Z",
                         "coincidences", "relative", "conditional","c.conditional","c.probable","tetrachoric","hyperGeometric"), 
                       nrow=1, dimnames=list("Similarity",
                                             c("M","T","G","S","B","J","D","A","O","K","N","Y","P","C","R","E","H","L","Z","F","X","I","U","Q","V","W")))
  return(similarities[,method])
}

# Similatities
sim<-function (input,procedures="Jaccard",distance=FALSE, minimum=1, maximum=Inf, sort=FALSE, decreasing=FALSE) {
  method<-c.method(toupper(procedures))
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
  s<-new.env()
  
  if ("M" %in% method) s$matching <- distant((a + d)/(a + b + c + d),distance)
  if ("T" %in% method) s$Rogers <- distant((a + d)/(a + 2 * (b + c) + d), distance)
  if ("G" %in% method) s$Gower <- distant(a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),distance)
  if ("S" %in% method) s$Sneath <- distant(2*(a+d)/(2*(a+d)+(b+c)),distance)
  if ("B" %in% method) s$Anderberg <-distant((a/(a+b)+a/(a+c)+d/(c+d)+d/(b+d))/4,distance)
  if ("J" %in% method) s$Jaccard <- distant(a/(a + b + c),distance)
  if ("D" %in% method) s$dice <- distant(2 * a/(2 * a + b + c), distance)
  if ("A" %in% method) s$antiDice <- distant(a/(a + 2 * (b + c)),distance)
  if ("O" %in% method) s$Ochiai <- distant(a/sqrt((a + b) * (a + c)),distance)
  if ("K" %in% method) s$Kulczynski <- distant((a/(a+b)+a/(a+c))/2, distance)
  if ("N" %in% method) s$Hamann <- distant((a - (b + c) + d)/(a + b + c + d), distance)
  if ("Y" %in% method) s$Yule <- distant((a*d-b*c)/(a*d+b*c))
  if ("P" %in% method) s$Pearson <- distant((a * d - b * c)/sqrt((a + b) * (a + c) * (b + d) *  (d + c)),distance)
  if ("V" %in% method) {
    s$tetrachoric<-((a*d/(b*c))^(pi/4)-1)/((a*d/(b*c))^(pi/4)+1)
    diag(s$tetrachoric)<-1
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
  if ("W" %in% method) s$hyperGeometric<-1-phyper(a-1,pmin((a+b),(a+c)),N-pmin((a+b),(a+c)),pmax((a+b),(a+c)))
  if ("F" %in% method) s$coincidences <- a
  if ("X" %in% method) s$relative <- a/N*100
  if ("I" %in% method) s$conditional <-t(a/diag(a)*100)
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
  if (t==TRUE) s<-as.dist(sqrt(1-s))
  return(s)
}
# http://pbil.univ-lyon1.fr/ade4/ade4-html/dist.binary.html


# Print lower matrices

lower<-function(matrix,decimals=3) { # Add an option to hiden diagonal
  m<-as.matrix(matrix)
  form=paste("%1.",decimals,"f",sep="")
  lower<-apply(matrix,1,function(x) sprintf(form,x))
  lower[upper.tri(lower)]<-""
  lower<-as.data.frame(lower, stringsAsFactors=FALSE)
  rownames(lower)<-colnames(lower)
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
print.coin<-function(x, ...) {
  cat("n= ",x$n,"\n",sep="")
  print(lower(x$f,0))
}
print <- function(x, ...) UseMethod("print")
# Transform a coin object into a data frame with name and frequency
as.nodes<-function(C,frequency=TRUE,percentages=FALSE,language="en"){
  if (class(C)=="coin") {
    if (!percentages & frequency) nodes<-data.frame(name=as.character(colnames(C$f)),frequency=diag(C$f))
    else if (!frequency) nodes<-data.frame(name=as.character(colnames(C$f)),"%"=diag(C$f)/C$n*100,check.names=FALSE)
    else nodes<-data.frame(name=as.character(colnames(C$f)),frequency=diag(C$f), "%"=diag(C$f)/C$n*100,check.names=FALSE)
    if (language=="es") {
       colnames(nodes)[colnames(nodes)=="frequency"]<-"frecuencia"
       colnames(nodes)[colnames(nodes)=="name"]<-"nombre"
    }       
  }
  else if (min(c("source", "target") %in% names(C))) nodes<-data.frame(name=sort(union(C$source,C$target)))
  else warning("Is neither a coin object or an edge data frame")
  return(nodes)
}

asIgraph <- function(net){
  nodes <- net$nodes
  links <- net$links
  options <- net$options
  
  g <- graph.empty(0, directed=FALSE)
  
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
    g <- add.vertices(g, 1, attr = attr)
  }
  
  for(i in seq_len(nrow(links))){
    edges <- c(which(links[i,'source']==nodes[,net$options$nodeName]),which(links[i,'target']==nodes[,net$options$nodeName]))
    attr <- list()
    if(!is.null(options[["linkWidth"]]))
      attr[['width']] <- links[i,options[["linkWidth"]]]
    if(!is.null(options[["linkColor"]]))
      attr[['color']] <- as.character(links[i,options[["linkColor"]]])
    g <- add.edges(g, edges, attr = attr)
  }
  
  return(g)
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
  if (directed) l <- as.vector(lower.tri(data,diag=diagonal) | upper.tri(data))
  else l <- as.vector(lower.tri(data,diag=diagonal))
  
  # data.frame building
  targets<-rep(rownames(data),dim(data)[1])
  sources<-rep(colnames(data),rep(dim(data)[2],dim(data)[1]))
  Mat <- data.frame(source=sources,target=targets)
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
  if (nrow(Mat)>0) return(Mat)
  else return(NULL)
}

