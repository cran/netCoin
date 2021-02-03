# create json for multigraph
multigraphJSON <- function(multi,dir){
json <- character(0)
types <- character(0)
items <- character(0)
for(item in names(multi)){
  graph <- multi[[item]]
  gClass <- ""
  jsongraph <- "{}"
  if(inherits(graph,"netCoin")){
    gClass <- "netCoin"
    jsongraph <- imgWrapper(graph,networkJSON,dir)
  }else if(inherits(graph,"timeCoin")){
    gClass <- "timeCoin"
    jsongraph <- timelineJSON(graph)
  }else if(inherits(graph,"barCoin")){
    gClass <- "barCoin"
    jsongraph <- barplotJSON(graph)
  }else if(is.character(graph) && file.exists(paste0(graph,'/index.html'))){
    gClass <- "iFrame"
    graphName <- sub("^.*/","",graph)
    dir.create(paste0(dir,'/data'), showWarnings = FALSE)
    file.copy(graph, paste0(dir,'/data'), recursive = TRUE)
    jsongraph <- toJSON(paste0('data/',graphName))
  }else{
    warning(paste0('Not supported object "',item,'".'))
    next
  }
  json <- c(json,jsongraph)
  types <- c(types,toJSON(gClass))
  items <- c(items,toJSON(item))
}
json <- paste0(json,collapse=',')
types <- paste0(types,collapse=',')
items <- paste0(items,collapse=',')
return(paste0('{"items":[',items,'],"types":[',types,'],"data":[',json,']}'))
}

multiGraph <- function(multi,dir){
  language <- unique(unlist(lapply(multi,getLanguageScript)))
  if(length(language)!=1)
    language <- "en.js"
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","jszip.min.js","iro.min.js","functions.js",language,"colorScales.js","multigraph.js","network.js","barplot.js","timeline.js"), function(){ return(multigraphJSON(multi,dir)) })
}

polyGraph <- function(multi,dir){
  createHTML(dir, NULL, "polygraph.js", NULL)
  multiGraph(multi,paste0(dir,"/multiGraph"))
}

frameGraph <- function(multi,frame,speed,dir){

  if(!all(vapply(multi, inherits, TRUE, what = "netCoin")))
    stop("All graphs must be 'netCoin' objects")
  name <- unique(vapply(multi,function(x){ return(x$options$nodeName) },character(1)))
  if(length(name)!=1)
    stop("name: all graphs must have the same name")
  nodenames <- colnames(multi[[1]]$nodes)
  linknames <- colnames(multi[[1]]$links)
  for(m in multi){
    if(!identical(nodenames,colnames(m$nodes)))
      stop("nodes: all graphs must have the same node columns")
    if(!identical(linknames,colnames(m$links)))
      stop("links: all graphs must have the same link columns")
  }

  frames <- names(multi)

  links <- lapply(multi,function(x){ return(x$links) })
  for(i in seq_along(frames)){
    if(!is.null(links[[i]])){
      links[[i]][["_frame_"]] <- i-1
    }
  }
  links <- do.call(rbind,links)
  rownames(links) <- NULL

  nodes <- data.frame(name=unique(unlist(lapply(multi,function(x){ return(x$nodes[[name]]) }))))
  nodes$name <- as.character(nodes$name)
  names(nodes) <- name
  for(n in nodenames){
    if(n!=name){
      nodes[[n]] <- sapply(nodes[[name]],function(x){
        values <- sapply(frames,function(f){
          return(multi[[f]]$nodes[x,n])
        })
        aux <- unique(values[!is.na(values)])
        if(length(aux)==1)
          return(aux)
        if(length(aux)<1)
          return(NA)
        values <- as.character(values)
        values[is.na(values)] <- ""
        return(paste0(values,collapse="|"))
      })
    }
  }

  options <- multi[[1]]$options
  getAll <- function(opt,item){
      items <- sapply(multi,function(x){
        if(!is.null(x$options[[item]]))
          return(x$options[[item]])
        else
          return(NA)
      })
      if(length(unique(items))!=1)
        opt[[item]] <- items
      return(opt)
  }
  for(i in c("main","note","repulsion","distance","zoom"))
    options <- getAll(options,i)
  options$frames <- frames
  options$frame <- frame
  options$speed <- speed
  net <- structure(list(links=links,nodes=nodes,options=options),class="netCoin")

  tree <- list()
  for(i in seq_along(frames)){
    if(!is.null(multi[[i]]$tree)){
      tree[[i]] <- multi[[i]]$tree
      tree[[i]][["_frame_"]] <- i-1
    }
  }
  if(length(tree)){
    tree <- do.call(rbind,tree)
    rownames(tree) <- NULL
    net$tree <- tree
  }

  netCreate(net,dir)
}

#create html wrapper for multigraph
multigraphCreate <- function(...,  mode = c("default","parallel","frame"), frame = 0, speed = 50, dir = "MultiGraph", show = TRUE){
  graphs <- list(...)
  if(!length(graphs))
    stop("Cannot make a multigraph without graphs!")

  if(is.null(names(graphs)) || !all(!duplicated(names(graphs)))){
    message("Graph names will be generated automatically")
    names(graphs) <- paste0("graph",seq_along(graphs))
  }

  mode <- substr(mode[1],1,1)
  if(mode=="f" && length(graphs)==1){
    mode <- "d"
    warning("Cannot make a dynamic graph with only one graph")
  }
  if(mode=="p"){
    polyGraph(graphs,dir)
  }else if(mode=="f"){
    if(!(is.numeric(frame) && frame>=0)){
      frame <- formals(multigraphCreate)[["frame"]]
      warning("frame: must be integer greater than 0")
    }
    if(!(is.numeric(speed) && speed>=0 && speed<=100)){
      speed <- formals(multigraphCreate)[["speed"]]
      warning("speed: must be numeric between 0 and 100")
    }
    frameGraph(graphs,frame,speed,dir)
  }else{
    multiGraph(graphs,dir)
  }

  if(identical(show,TRUE))
    browseURL(normalizePath(paste(dir, "index.html", sep = "/")))
}
