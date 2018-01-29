## Programs to apply net coin analysis
# Image is a files vector with length and order equal to nrow(nodes). Place as nodes field
# Batch

allNet<-function(incidences, weight = NULL, subsample = FALSE,
                 minimum=1, maximum = nrow(incidences), sort = FALSE, decreasing = TRUE,
                 nodes = NULL, frequency = FALSE, percentages = TRUE,
                 name = NULL, label = NULL, ntext = NULL, 
                 size = "%", color = NULL, shape = NULL, group = NULL, community = NULL, 
                 procedures = "Haberman", criteria = "Z", Bonferroni = FALSE,
                 support = -Inf, minL = -Inf, maxL = Inf,
                 directed = FALSE, diagonal = FALSE, sortL = NULL, decreasingL = TRUE,
                 lwidth = NULL, lweight = NULL, lcolor = NULL, ltext = NULL,
                 nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL,
				 nodeBipolar = FALSE, linkBipolar = FALSE, defaultColor = "#1f77b4",
                 main = NULL, note = NULL, help = NULL, background = NULL,
				 layout = NULL, cex=1, controls = c(1,2,3), mode = c("network","heatmap"),
				 showCoordinates = FALSE, showArrows = FALSE, showLegend = TRUE, showAxes = FALSE, showLabels = TRUE,
				 axesLabels = NULL, language = "en", image = NULL,  dir = NULL, show = TRUE, igraph = FALSE)
{
  name <- nameByLanguage(name,language,nodes)
  incidences<-na.omit(incidences)
  if (all(incidences==0 | incidences==1)) {
    C<-coin(incidences, minimum, maximum, sort, decreasing, weight=weight, subsample)
    if (is.null(nodes)) {
        O<-asNodes(C,frequency,percentages,language)
    }
    else O<-nodes
    if (!is.null(lwidth)) lwidth<-i.method(c.method(lwidth))
    if (!is.null(lweight))lweight<-i.method(c.method(lweight))
    if (!is.null(lcolor)) lcolor<-i.method(c.method(lcolor))
    if (!is.null(ltext))  ltext<-i.method(c.method(ltext))
    eles<-unique(c(lwidth,lweight,lcolor,ltext))
    E<-edgeList(C, union(procedures,eles), criteria, Bonferroni, minL, maxL, support, 
                directed, diagonal, sortL, decreasing)
    xNx<-netCoin(O,E,
                 name, label, 
                 size, color, shape, ntext, group, community,
                 lwidth, lweight, lcolor, ltext,
                 nodeFilter, linkFilter, degreeFilter, nodeBipolar, linkBipolar, defaultColor,
                 main, note, help, background,
                 layout, cex, controls, mode, showCoordinates, showArrows, showLegend,
                 showAxes, showLabels, axesLabels, language,
                 image,  dir, show)
  }
  else warning("Input is not a dichotomous matrix of incidences")
  if (igraph) return(toIgraph(xNx))
  else return(xNx)
}

netCoin<-function (nodes, links, name = NULL,
                   label = NULL, size = NULL, color = NULL, shape = NULL, ntext = NULL, group = NULL, community = NULL,
                   width = NULL, weight = NULL, lcolor = NULL, ltext = NULL,
                   nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL, nodeBipolar = FALSE, linkBipolar = FALSE, defaultColor = "#1f77b4",
                   main = NULL, note = NULL, help = NULL, background = NULL, layout = NULL, cex = 1, controls = c(1,2,3), mode = c("network","heatmap"),
                   showCoordinates = FALSE, showArrows = FALSE, showLegend = TRUE, showAxes = FALSE, showLabels = TRUE, axesLabels = NULL,
                   language = c("en", "es"), image = NULL,  dir = NULL, show = TRUE)
{
  name <- nameByLanguage(name,language,nodes)
  links<-links[links$Source%in%nodes[[name]]&links$Target%in%nodes[[name]],]
  options=list(nodeName=name)

  # graph options
  if (!is.null(main)) options[["main"]] <- main
  if (!is.null(note)) options[["note"]] <- note
  if (!is.null(help)) options[["help"]] <- help
  if (!is.null(background)) options[["background"]] <- background

  if(nodeBipolar) options[["nodeBipolar"]] <- TRUE
  if(linkBipolar) options[["linkBipolar"]] <- TRUE
  options[["cex"]] <- as.numeric(cex)
  if (!is.null(defaultColor)) options[["defaultColor"]] <- defaultColor
  if (!is.null(controls)) options[["controls"]] <- as.numeric(controls)
  if (!is.null(mode)) options[["mode"]] <- as.character(mode)
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
  if (!is.null(ntext)) options[["nodeText"]] <- ntext

  # link options
  if (!is.null(width)) options[["linkWidth"]] <- width
  if (!is.null(weight)) options[["linkWeight"]] <- weight
  if (!is.null(lcolor)) options[["linkColor"]] <- lcolor
  if (!is.null(ltext)) options[["linkText"]] <- ltext

  # filters
  rownames(nodes) <- nodes[,name]
  nodes$noShow <- FALSE
  links$noShow <- FALSE
  if (!is.null(nodeFilter)){
    nodes[,"noShow"] <- !with(nodes,eval(parse(text=nodeFilter)))
	noShowNodes <- as.character(nodes[nodes[,"noShow"],name])
	links[(as.character(links[,"Source"]) %in% noShowNodes)|(as.character(links[,"Target"]) %in% noShowNodes),"noShow"] <- TRUE
  }

  if (!is.null(linkFilter)){
    links[,"noShow"] <- links[,"noShow"] | !with(links,eval(parse(text=linkFilter)))
  }
  
  if (!is.null(degreeFilter)) options[["degreeFilter"]] <- as.numeric(degreeFilter)

  net <- structure(list(links=links,nodes=nodes,options=options,call=match.call()),class="netCoin")

  #images
  if (!is.null(image)){
    net$nodes[[image]] <- NULL
    net <- netNodeImage(net,nodes[[image]],image)
  }

  #layout
  if (!is.null(layout)) {
    if(is.character(layout)){ 
      layout <- layoutControl(layout)
      layout <- coords[[layout]](toIgraph(net))
    }
    if (class(layout)=="matrix")
	  net <- netAddLayout(net,layout)
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
                  nodes=NULL, frequency=FALSE, means=TRUE,
                  name=NULL, label = NULL, ntext = NULL, 
                  size = "mean", color = NULL, shape = NULL, group = NULL, community = NULL, 
                  method="pearson", criteria="p", Bonferroni=FALSE,
                  minL=0, maxL=Inf,
                  sortL=NULL, decreasingL=TRUE,
                  lwidth = "value", lweight = "value", lcolor = NULL, ltext = NULL,
                  nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL,
				  nodeBipolar = FALSE, linkBipolar = FALSE, defaultColor = "#1f77b4",
                  main = NULL, note = NULL, help = NULL, background = NULL,
                  layout = NULL, cex=1, controls = c(1,2,3), mode = c("network","heatmap"),
                  showCoordinates = FALSE, showArrows = FALSE, showLegend = TRUE, showAxes = FALSE, showLabels = TRUE,
                  axesLabels = NULL, language = "en", 
                  image = NULL,  dir = NULL, show = TRUE,
                  igraph=FALSE 
){
  name <- nameByLanguage(name,language,nodes)
  variables<-na.omit(variables)
  cases<-nrow(variables)
  if (criteria=="p" & maxL==Inf)  maxL<-.5
  if (criteria=="p" & Bonferroni) maxL<-maxL/choose(cases,2)
  if (is.null(nodes)) {
    O<-data.frame(name=colnames(variables),
                  mean=round(apply(variables,2,mean),2),
                  std=round(sqrt(apply(variables,2,var)),2),
                  min=apply(variables,2,min),
                  max=apply(variables,2,max))
  }
  else O<-nodes
  R<-cor(variables[,O[,2]>=minimum & O[,2]<=maximum],method=method)
  E<-edgeList(R, "shape", min=-1, max=1, directed=FALSE, diagonal=FALSE)
  E$z<-E$value*sqrt(cases)
  E$p<-1-pt(E$z,cases-1)
  E<-E[E[[criteria]]>=minL & E[[criteria]]<=maxL,]
  if (!is.null(sortL)) E<-E[order((-1*decreasingL+!decreasingL)*E[[sortL]]),]
   
  xNx<-netCoin(O,E,
               name, label, 
               size, color, shape, ntext, group, community, 
               lwidth, lweight, lcolor, ltext,
               nodeFilter, linkFilter, degreeFilter, nodeBipolar, linkBipolar, defaultColor,
               main, note, help, background,
               layout, cex, controls, mode, showCoordinates,
               showArrows, showLegend, showAxes, showLabels,
			   axesLabels, language, image,  dir, show)
  if (igraph) return(toIgraph(xNx))
  else return(xNx)
}

# surCoin is a wrapper to build a netCoin object from an original non-dichotomized data.frame. See below dichotomize()

surCoin<-function(data,variables=names(data),exogenous=NULL, commonlabel=NULL,
                  weight=NULL, subsample=FALSE,
                  minimum=1, maximum=nrow(data), sort=FALSE, decreasing=TRUE,
                  nodes=NULL, frequency=FALSE, percentages=TRUE,
                  name=NULL, label = NULL, ntext = NULL, 
                  size = "%", color = NULL, shape = NULL, group = NULL, community = NULL, 
                  procedures="Haberman", criteria="Z", Bonferroni=FALSE,
                  support=-Inf, minL=-Inf, maxL=Inf,
                  directed=FALSE, diagonal=FALSE, sortL=NULL, decreasingL=TRUE,
                  lwidth = NULL, lweight = NULL, lcolor = NULL, ltext = NULL,
                  nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL,
				  nodeBipolar = FALSE, linkBipolar = FALSE, defaultColor = "#1f77b4",
                  main = NULL, note = NULL, help = NULL, background = NULL,
                  layout = NULL, cex=1, controls = c(1,2,3), mode = c("network","heatmap"),
                  showCoordinates = FALSE, showArrows = FALSE, showLegend = TRUE, showAxes = FALSE, showLabels = TRUE,
                  axesLabels = NULL, language = "en", 
                  image = NULL,  dir = NULL, show = TRUE,
                  igraph=FALSE 
                  ){

  if (all(class(data)==c("tbl_df","tbl","data.frame"))) data<-as.data.frame(as_factor(data)) # convert haven objects
  incidences<-dichotomize(data, variables, "", min=minimum, length=0, values=NULL, sparse=FALSE, add=FALSE, sort=sort)
  name <- nameByLanguage(name,language,nodes)
  if (!is.null(nodes)) {
    nonAmong<-setdiff(as.character(nodes[[name]]),names(incidences))
    if(length(nonAmong)>0)
      warning(paste0(toString(nonAmong)," is/are not present among incidences."))
    nodeList<-setdiff(as.character(nodes[[name]]),nonAmong)
    incidences<-incidences[,nodeList]
  }
  incidences<-na.omit(incidences)
  if (all(incidences==0 | incidences==1)) {
    C<-coin(incidences, minimum, maximum, sort, decreasing, weight=weight, subsample=subsample)
    O<-asNodes(C,frequency,percentages,language)
    names(O)[1]<-name
    if(!is.null(nodes)) {
      O<-merge(O,nodes[,setdiff(names(nodes),c("frequency","frecuencia","%"))],by.x=name,all.x=TRUE)
    }else {
      if (!is.null(commonlabel)) { # Preserve the prename (variable) of a node if specified in commonlabel
        ifelse(language=="es",label<-"etiqueta",label<-"label")
        provlabels<-as.character(O[[name]])
        O[[label]]<-ifelse(substr(O[[name]],1,regexpr('\\:',O[[name]])-1) %in% commonlabel,provlabels,substr(O[[name]],regexpr('\\:',O[[name]])+1,1000000L))
      }
    }
    if (!is.null(lwidth)) lwidth<-i.method(c.method(lwidth))
    if (!is.null(lweight))lweight<-i.method(c.method(lweight))
    if (!is.null(lcolor)) lcolor<-i.method(c.method(lcolor))
    if (!is.null(ltext))  ltext<-i.method(c.method(ltext))
    eles<-unique(c(lwidth,lweight,lcolor,ltext))
    E<-edgeList(C, union(procedures,eles), criteria, Bonferroni, minL, maxL, support, 
                directed, diagonal, sortL, decreasing)
    if (!is.null(exogenous)) {
      E$chaine<-ifelse((substr(E$Source,1,regexpr("\\:",E$Source)-1) %in% exogenous) 
                       &   (substr(E$Target,1,regexpr("\\:",E$Target)-1) %in% exogenous),"No","Yes")
      linkFilter<-paste(ifelse(is.null(linkFilter),"",paste(linkFilter,"&")),"chaine=='Yes'")
    }
    if (!is.null(layout)) {
      if (class(layout)=="matrix"){
	    if (!is.null(nodes)){
		  if(nrow(layout)==nrow(nodes)){
		    Oxy <- matrix(NA,nrow(O),2)
		    rownames(Oxy) <- as.character(O[,name])
			rownames(layout) <- as.character(nodes[,name])
			layoutnames <- intersect(rownames(Oxy),rownames(layout))
		    Oxy[layoutnames,] <- layout[layoutnames,]
            layout <- Oxy
		  } else warning("layout must have a coordinate per node")
		} else warning("layout must be applied to the nodes variable")
      }
    }   
    xNx<-netCoin(O,E,
                 name, label, 
                 size, color, shape, ntext, group, community, 
                 lwidth, lweight, lcolor, ltext,
                 nodeFilter, linkFilter, degreeFilter, nodeBipolar, linkBipolar, defaultColor,
                 main, note, help, background,
                 layout, cex, controls, mode, showCoordinates,
                 showArrows, showLegend, showAxes, showLabels,
                 axesLabels, language, image,  dir, show)
  }
  else warning("Input is not a dichotomous matrix of incidences")
  if (igraph) return(toIgraph(xNx))
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
    matrices<-matrices[i.method(todas)]
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
  print(as.data.frame(head(x$nodes[,setdiff(names(x$nodes),c("noShow","fixed","chaine","x","y"))])),row.names=F)
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

    g <- graph.empty(0, directed=FALSE)

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
      g <- add.vertices(g, 1, attr = attr)
    }
  
    for(i in seq_len(nrow(links))){
      edges <- c(which(as.character(links[i,'Source'])==as.character(nodes[,net$options$nodeName])),which(as.character(links[i,'Target'])==as.character(nodes[,net$options$nodeName])))
      attr <- list()
      if(!is.null(options[["linkWidth"]]))
        attr[['width']] <- links[i,options[["linkWidth"]]]
      if(!is.null(options[["linkColor"]]))
        attr[['color']] <- as.character(links[i,options[["linkColor"]]])
      g <- add.edges(g, edges, attr = attr)
    }
  
    return(g)
  }
  else warning("Is not a netCoin object")
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
  if (nrow(Mat)>0) return(Mat)
  else return(NULL)
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

layoutPolygon <- function(N,nodes,deg=0,name=NULL){
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
    N[nodes,"x"] <- cos(angle)
    N[nodes,"y"] <- sin(angle)
  }
  }else
    warning("nodes: must be numeric or character")

  return(as.matrix(N[,c("x","y")]))
}

nameByLanguage <- function(name,language,nodes){
  if(is.null(name)){
    if(language[1]=="es")
	  name <- "nombre"
	else
	  name <- "name"
  }
  if(!is.null(nodes) & !(name %in% colnames(nodes))) warning(paste0("name: '",name,"' column missing in nodes data frame"))
  return(name)
}