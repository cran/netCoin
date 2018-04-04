# create json for multigraph
multigraphJSON <- function(multi,multinames,dir){
json <- character(length(multi))
for(i in seq_along(multi)){
  graph <- multi[[i]]
  gClass <- class(graph)
  jsongraph <- "{}"
  if(gClass == "netCoin"){
    if("images" %in% names(graph))
      graph$nodes[["image"]] <- images2net(graph$images,dir)
    jsongraph <- networkJSON(graph)
  }
  if(gClass == "timeCoin")
    jsongraph <- timelineJSON(graph)
  if(gClass == "barCoin")
    jsongraph <- barplotJSON(graph)
  if(gClass == "character" && file.exists(paste0(graph,'/index.html'))){
    gClass <- 'iFrame'
    graphName <- sub("^.*/","",graph)
    dir.create(paste0(dir,'/data'), showWarnings = FALSE)
    file.copy(graph, paste0(dir,'/data'), recursive = TRUE)
    jsongraph <- toJSON(paste0('data/',graphName))
  }
  json[i] <- paste0('"',multinames[i],'":["',gClass,'",',jsongraph,']')
}
json <- paste0("{",paste0(json,collapse=","),"}")
return(json)
}

multiGraph <- function(multi,multinames,language,dir){
 if(language[1]=="es")
    language <- "es.js"
  else
    language <- "en.js"
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","jszip.min.js","functions.js",language,"colorScales.js","multigraph.js","network.js","barplot.js","timeline.js"), function(){ return(multigraphJSON(multi,multinames,dir)) }, FALSE)
}

polyGraph <- function(multi,multinames,language,dir){
  createHTML(dir, NULL, c("d3.min.js","polygraph.js"), toJSON(multinames), FALSE)
  multiGraph(multi,multinames,language,paste0(dir,"/multiGraph"))
}

#create html wrapper for multigraph
multigraphCreate <- function(...,  parallel = FALSE, language = c("en","es"), dir = "MultiGraph", show = TRUE){
multi <- list(...)
multinames <- names(multi)
if(is.null(multinames))
  multinames <- paste0("graph",seq_along(multi))
if(parallel){
  polyGraph(multi,multinames,language,dir)
}else{
  multiGraph(multi,multinames,language,dir)
}
if(identical(show,TRUE))
  browseURL(normalizePath(paste(dir, "index.html", sep = "/")))
}
