# create json for multigraph
multigraphJSON <- function(multi){
json <- character(length(multi))
multinames <- names(multi)
if(is.null(multinames))
  multinames <- paste0("graph",seq_along(multi))
for(i in seq_along(multi)){
  graph <- multi[[i]]
  gClass <- class(graph)
  jsongraph <- "{}"
  if(gClass == "network")
    jsongraph <- networkJSON(graph)
  if(gClass == "timeline")
    jsongraph <- timelineJSON(graph)
  if(gClass == "barplot")
    jsongraph <- barplotJSON(graph)
  json[i] <- paste0('"',multinames[i],'":["',gClass,'",',jsongraph,']')
}
json <- paste0("{",paste0(json,collapse=","),"}")
return(json)
}

#create html wrapper for multigraph
multigraph.create <- multinet.create <- function(..., language = c("en","es"), dir = "MultiGraph"){
multi <- list(...)
if(language[1]=="es")
  language <- "es.js"
else
  language <- "en.js"
createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","jszip.min.js","functions.js",language,"colorScales.js","multigraph.js","network.js","barplot.js","timeline.js"),function(){
  for(i in seq_along(multi))
    if("images" %in% names(multi[[i]]))
      multi[[i]]$nodes[["image"]] <- images2net(multi[[i]]$images,dir)
  return(multigraphJSON(multi))
})
}
