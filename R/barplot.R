barplotJSON <- function(bar){
  return(toJSON(list(nodes=bar$nodes,links=bar$links,options=bar$options)))
}

barStart <- function(nodes, links, options){
  lNames <- union(links$Source,links$Target)
  notListed <- length(setdiff(lNames,nodes[[options$name]]))
  if(notListed!=0)
    stop(paste(notListed," node link(s) not defined in nodes data frame."))
  structure(list(nodes=nodes, links=links, options=options), class="barCoin")
}

barCreate <- function(bar, dir = "barCoin"){
  language <- getLanguageScript(bar)
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","functions.js",language,"colorScales.js","barplot.js"), barplotJSON(bar))
}

barCoin<-function(data, variables = colnames(data), commonlabel = NULL,
        dichotomies = c("_all","_none"), valueDicho = 1, weight = NULL,
        subsample = FALSE, sort = NULL, decreasing = TRUE, nodes = NULL,
        name = NULL, select = NULL, scalebar = FALSE, note = NULL, label = NULL,
        text = NULL, color = NULL, defaultColor = "#1f77b4", expected = FALSE,
        confidence = FALSE, level = .95, significance = FALSE, minimum = 1 ,
        maximum = nrow(data), percentages = FALSE, criteria = c("Z","hyp"),
        Bonferroni = FALSE, support = 1, minL = -Inf, maxL = 1,
        language = c("en","es","ca"), cex = 1.0, dir = NULL)
{

  name <- nameByLanguage(name = name, language =language, nodes = nodes)
  dicho<-function(input,variables,value) {
    datum<-as.data.frame(ifelse(input[,variables]==value,1,0))
    if (all(class(input)==c("tbl_df","tbl","data.frame"))) {
      # L<-sapply(datum[,variables],attr,"label")
      M<-sapply(sapply(datum,attr,"label"),function(X) ifelse(is.null(X),NA,X))
      L<-ifelse(is.na(M),variables,M)
      names(datum)<-L
    }
    return(datum)
  }

# model of bar
  if (confidence) procedures <- c("Frequencies","Expected","Confidence")
  else procedures <- c("Frequencies","Expected")
  criteria <- criteria[1]
  if(criteria!="hyp") criteria <- "Z"
  procedures<-c(procedures,criteria)
  criteriacolname <- c(Z="p(Z)",hyp="p(Fisher)")[criteria]
  

# names  
  if ("tbl_df" %in% class(nodes)) nodes<-as.data.frame(nodes)

  

# classification of variables
  if("_all"  %in% dichotomies) dichotomies<-variables
  if("_none" %in% dichotomies) {
    allvar<-variables  
    dichotomies <- NULL
  }
    else allvar <- union(dichotomies,variables)
#  if(!is.null(nodes)) allvar<-intersect(unlist(nodes[name]),allvar)
  
# treatment of weight in the data frame. Omission of na data.
    if(!is.null(weight)) {
    if(class(weight)=="character"){
      data<-na.omit(data[,allvar])
      allvar<-setdiff(allvar,weight)
      variables<-setdiff(variables,weight)
      weight<-data[,weight]
    }
    else{
      if(length(weight)!=dim(data)[1]) stop("Weights have not the correct dimensions")
      data<-na.omit(cbind(data[,allvar],weight))[,1:length(data[,allvar])]
    }
  }
  else data<-na.omit(data[,allvar])

# set dichotomies    
  if(length(dichotomies)>0) {
    dichos<-dicho(data,dichotomies,valueDicho)
    variables<-setdiff(variables,dichotomies)
  }

# data.frame setting
  data[,variables]<-as_factor(data[,variables])
  if (all(class(data)==c("tbl_df","tbl","data.frame"))) data<-as.data.frame(data) # convert haven objects
  
# dichotomizing factor variables
  if (length(variables)>0){
    incidences<-dichotomize(data, variables, "", min=minimum, length=0, values=NULL, sparse=FALSE, add=FALSE, sort=TRUE)
    if(exists("dichos")) incidences<-cbind(dichos,incidences)
  } 
  else if(exists("dichos")) incidences<-dichos
  
  
# nodes filter  
  if (!is.null(nodes)) {
    nonAmong<-setdiff(as.character(nodes[[name]]),names(incidences))
    nodeList<-setdiff(as.character(nodes[[name]]),nonAmong)
    incidences<-incidences[,nodeList]
    if(length(nonAmong)>0)
      warning(paste0(toString(nonAmong)," is/are not present among incidences."))
  }
  
# coincidences elaboration
  if(!exists("incidences")) stop("There are no qualitative variables. Try netCorr.") 
  incidences<-na.omit(incidences)
  if(!is.null(nodes)) incidences <- incidences[,intersect(unlist(nodes[name]),colnames(incidences))]
  if (all(incidences==0 | incidences==1)) {
    C<-coin(incidences, minimum, maximum, sort=TRUE, decreasing=TRUE, weight=weight, subsample=subsample)

# nodes data.frame elaboration
    O<-asNodes(C, !percentages, percentages, language = language) # Attention to !percentages
    if(name!="name") names(O)[1]<-name
    names(O)[2] <- "incidences"
    if(!is.null(nodes)) {
      O<-merge(O,nodes[,setdiff(names(nodes),frequencyList),drop=FALSE],by.x=name,by.y=name,all.x=TRUE)
    }else {
      if (!is.null(commonlabel)) { # Preserve the prename (variable) of a node if specified in commonlabel
        label<-getByLanguage(labelList,language)
        provlabels<-as.character(O[[name]])
        O[[label]]<-ifelse(substr(O[[name]],1,regexpr('\\:',O[[name]])-1) %in% commonlabel,provlabels,substr(O[[name]],regexpr('\\:',O[[name]])+1,1000000L))
      }
    }

# making edgeList
    level <- checkLevel(level)
    E<-edgeList(C, procedures, criteria, level, Bonferroni, minL, maxL, support, 
                directed=FALSE, diagonal= FALSE, sort= NULL)

# definition of options
    options <-list(name = name, incidences = "incidences", coincidences = "coincidences", level = level, defaultColor = defaultColor)
    if (expected || confidence) options[["expected"]] <- "expected"
    if (confidence){
      if(significance){
        options[["confidence"]] <- "confidence"
        E[,c("conf.L","conf.U")] <- NULL
      }else{
        options[["confidence"]] <- c("conf.L","conf.U")
        E[,"confidence"] <- NULL
      }
    }
    if (significance){
      options[["significance"]] <- criteriacolname
    }else{
      E[,criteriacolname] <- NULL
    }
    options[["cex"]] <- as.numeric(cex)
    options[["language"]] <- language[1]
    if(!is.null(note))
      options[["note"]] <- note
    if(!is.null(label))
      options[["label"]] <- label
    if(!is.null(text))
      options[["text"]] <- text
    if(!is.null(color))
      options[["color"]] <- color
    if(!is.null(select)){
      if(select %in% O[,name])
        options[["select"]] <- select
      else
        warning("select: must be in 'nodes' name column")
    }
    options[["rev"]] <- as.integer(!decreasing)
    if(!is.null(sort)){
      if(sort %in% colnames(O)){
        options[["order"]] <- sort
        options[["rev"]] <- bitwXor(as.integer(is.numeric(O[,sort])),as.integer(decreasing))
      }else
        warning("sort: must be a 'nodes' column")
    }
    if(scalebar)
      options[["scalebar"]] <- TRUE

# convertion to percentages
    if (percentages) {
      E[,intersect(names(E),c("coincidences","expected","confidence","conf.L","conf.U"))]<-
      E[,intersect(names(E),c("coincidences","expected","confidence","conf.L","conf.U"))]/attr(C,"n")*100
    }
        
# preparing bar graph
    bar <- barStart(O, E, options)
    if (!is.null(dir)) barCreate(bar, dir)
    return(bar)
  }
  else warning("Input is not a dichotomous matrix of incidences")
}
