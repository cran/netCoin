#create json
networkJSON<-function(net){

links <- net$links
nodes <- net$nodes
options <- net$options

if(length(links)){
sourcenames <- as.vector(links$Source)
targetnames <- as.vector(links$Target)
name <- nodes[[options$nodeName]]

nodesid <- (1:length(name))-1
uniqueid <- data.frame(nodesid,name)

nlinks <- nrow(links)
source <- numeric(nlinks)
target <- numeric(nlinks)
for(i in seq_len(nlinks)){
  source[i] <- uniqueid[(sourcenames[i]==uniqueid[,2]),1]
  target[i] <- uniqueid[(targetnames[i]==uniqueid[,2]),1]
}

links$Source <- source
links$Target <- target
}

json <- list(nodes = nodes, links = links, options = options)
  
return(toJSON(json))
}

#start a network graph
netStart <- function(nodes,links,name="name",main=NULL,note=NULL,help=NULL){

links <- data.frame(Source = links[,1],Target = links[,2])
nodes <- data.frame(nodes)
names(nodes) <- name
options <- list(nodeName=name)
if(!is.null(main))
  options$main <- main
if(!is.null(note))
  options$note <- note
if(!is.null(help))
  options$help <- help

structure(list(links = links, nodes = nodes, options = options, call = match.call()), class = "netCoin")
}

# add link attributes
netAddLinkAttr <- function(net,values,source="Source",target="Target",width=NULL,weight=NULL,color=NULL,text=NULL){
if(nrow(net$links)>0){
  net$links <- merge(net$links,values,by.x=c("Source","Target"),by.y=c(source,target),all.x=TRUE,sort=FALSE)
  if(!is.null(width))
    net$options[["linkWidth"]] <- width
  if(!is.null(weight))
    net$options[["linkWeight"]] <- weight
  if(!is.null(color))
    net$options[["linkColor"]] <- color
  if(!is.null(text))
    net$options[["linkText"]] <- text
}else{
  warning("no links!")
}
return(net)
} 

# add node attributes
netAddNodeAttr <- function(net,values,name="name",label=NULL,group=NULL,size=NULL,color=NULL,shape=NULL,text=NULL){
net$nodes <- merge(net$nodes,values,by.x=net$options$nodeName,by.y=name,all.x=TRUE,sort=FALSE)
if(!is.null(label))
  net$options[["nodeLabel"]] <- label
if(!is.null(group))
  net$options[["nodeGroup"]] <- group
if(!is.null(size))
  net$options[["nodeSize"]] <- size
if(!is.null(color))
  net$options[["nodeColor"]] <- color
if(!is.null(shape))
  net$options[["nodeShape"]] <- shape
if(!is.null(text))
  net$options[["nodeText"]] <- text
return(net)
}

# add images to nodes
netNodeImage <- function(net,image,name="images"){
  if(length(image)==nrow(net$nodes)){
    net$images <- as.vector(image)
    net$options[["nodeItem"]] <- "image"
    net$options[["imageLegend"]] <- name
  }else
    warning("must have an image per node")
  return(net)
}

# add pie charts to nodes
netNodePie <- function(net,pie){
  if(nrow(pie)==nrow(net$nodes)){
    net$nodes[["pie"]] <- apply(data.matrix(pie),1,function(x) paste0("[",paste0(x,collapse=","),"]"))
    net$options[["nodeItem"]] <- "pie"
    net$options[["pieLegend"]] <- colnames(pie,FALSE,"piece")
  }else
    warning("must have a pie per node")
  return(net)
}

# add layout
netAddLayout <- function(net,layout){
  if(nrow(layout)==nrow(net$nodes)){
    net$options[["stopped"]] <- TRUE
    net$nodes[["x"]] <- layout[,1]
    net$nodes[["y"]] <- layout[,2]
    net$nodes[["fixed"]] <- apply(layout,1,function(x) ifelse(is.na(x[1])||is.na(x[2]),0,1))
  }else
    warning("layout must have a coordinate per node")
  return(net)
}

#copy images to net graph
images2net <- function(images,dir){
  imgDir <- paste(dir,"images",sep="/")
  dir.create(imgDir, showWarnings = FALSE)
  file.copy(images, imgDir)
  if(length(images)==1)
    return(paste("images",sub("^.*/","",images),sep="/"))
  else
    return(sapply(strsplit(images,"/"),function(x) paste("images",x[length(x)],sep="/")))
}

imgWrapper <- function(net,dir){
  if("images" %in% names(net))
    net$nodes[["image"]] <- images2net(net$images,dir)
  if(!is.null(net$options[["background"]]) && file.exists(net$options[["background"]]))
    net$options[["background"]] <- paste0('url("',images2net(net$options[["background"]],dir),'")')
  return(networkJSON(net))
}

#create html wrapper for network graph
netCreate <- function(net, language = c("en","es"), dir = "netCoin", show = FALSE){
  if(length(language) && language[1]=="es")
    language <- "es.js"
  else
    language <- "en.js"
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","jszip.min.js","functions.js",language,"colorScales.js","network.js"),function(){
    return(imgWrapper(net,dir))
  },show)
}

#meta function
netAll <- function(nodes,links,name="name",source="Source",target="Target",layout=NULL){
  net <- netStart(nodes[,name],links[,c(source,target)],name)
  net <- netAddLinkAttr(net,links,source=source,target=target)
  net <- netAddNodeAttr(net,nodes,name=name)
  if(!is.null(layout))
    net <- netAddLayout(net,layout)
  return(net)
}

#meta function for igraph objects
fromIgraph <- function(G, layout=NULL, language = c("en","es"), dir=NULL){
  if (class(G)=="igraph"){
    nodeNames <- V(G)$name
    if(is.null(nodeNames))
      nodeNames <- as.character(seq_along(V(G)))
    nodes <- data.frame(name=nodeNames)
    links <- get.edgelist(G)
    links <- data.frame(Source=links[,1],Target=links[,2])
    for(i in list.vertex.attributes(G))
      nodes[[i]] <- get.vertex.attribute(G,i)
    for(i in list.edge.attributes(G))
      links[[i]] <- get.edge.attribute(G,i)
    net <- netAll(nodes,links,layout=layout)
    if (!is.null(dir)) netCreate(net,language=language,dir=dir)
    return(net)
  }
  else warning("Is not an igraph object")
}
