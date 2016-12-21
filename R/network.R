#create json
networkJSON<-function(net){

links <- net$links
nodes <- net$nodes
options <- net$options

sourcenames <- as.vector(links$source)
targetnames <- as.vector(links$target)
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

links$source <- source
links$target <- target

json <- list(nodes = nodes, links = links, options = options)
  
return(toJSON(json))
}

#start a network graph
network.start <- function(nodes,links,name="name",main=NULL,minor=NULL,help=NULL){

links <- data.frame(source = links[,1],target = links[,2])
nodes <- data.frame(nodes)
names(nodes) <- name
options <- list(nodeName=name)
if(!is.null(main))
  options$main <- main
if(!is.null(minor))
  options$minor <- minor
if(!is.null(help))
  options$help <- help

structure(list(links = links, nodes = nodes, options = options, call = match.call()), class = "network")
}

# add link attributes
network.addLinkAttr <- function(net,values,source="source",target="target",width=NULL,weight=NULL,color=NULL,text=NULL){
if(nrow(net$links)>0){
  net$links <- merge(net$links,values,by.x=c("source","target"),by.y=c(source,target),all.x=TRUE,sort=FALSE)
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
network.addNodeAttr <- function(net,values,name="name",label=NULL,group=NULL,size=NULL,color=NULL,shape=NULL,text=NULL){
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
network.nodeImage <- function(net,image,name="images"){
  if(length(image)==nrow(net$nodes)){
    net$images <- as.vector(image)
    net$options[["nodeItem"]] <- "image"
    net$options[["imageLegend"]] <- name
  }else
    warning("must have an image per node")
  return(net)
}

# add pie charts to nodes
network.nodePie <- function(net,pie){
  if(nrow(pie)==nrow(net$nodes)){
    net$nodes[["pie"]] <- apply(data.matrix(pie),1,function(x) paste0("[",paste0(x,collapse=","),"]"))
    net$options[["nodeItem"]] <- "pie"
    net$options[["pieLegend"]] <- colnames(pie,FALSE,"piece")
  }else
    warning("must have a pie per node")
  return(net)
}

# add layout
network.addLayout <- function(net,layout){
  if(nrow(layout)==nrow(net$nodes)){
    net$options[["stopped"]] <- TRUE
    net$nodes[["x"]] <- layout[,1]
    net$nodes[["y"]] <- layout[,2]
    net$nodes[["fixed"]] <- apply(layout,1,function(x) ifelse(is.na(x[1])||is.na(x[2]),0,1))
  }else
    warning("layout must have a coordinate per node")
  return(net)
}

# apply a filter to nodes
network.addNodeFilter <- function(net,...){
  filter <- list(...)
  filter <- paste0("(net$nodes$",filter,")",collapse="&")
  net$nodes$noShow <- !eval(parse(text=filter))
  return(net)
}

# apply a filter to links
network.addLinkFilter <- function(net,...){
  filter <- list(...)
  filter <- paste0("(net$links$",filter,")",collapse="&")
  net$links$noShow <- !eval(parse(text=filter))
  return(net)
}

#copy images to net graph
images2net <- function(images,dir){
  dir.create(paste(dir,"images",sep="/"), showWarnings = FALSE) 
  file.copy(images, paste(dir, "images", sep = "/"))
  return(sapply(strsplit(images,"/"),function(x) paste("images",x[length(x)],sep="/")))
}

#create html wrapper for network graph
network.create <- function(net, language = c("en","es"), dir = "Network"){
if(language[1]=="es")
  language <- "es.js"
else
  language <- "en.js"
createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","jszip.min.js","functions.js",language,"colorScales.js","network.js"),function(){
  if("images" %in% names(net))
    net$nodes[["image"]] <- images2net(net$images,dir)
  return(networkJSON(net))
})
}

#meta function
network.all <- function(nodes,links,name="name",source="source",target="target",layout=NULL,language=c("en","es"),dir="Network"){
net <- network.start(nodes[,name],links[,c(source,target)],name)
net <- network.addLinkAttr(net,links,source=source,target=target)
net <- network.addNodeAttr(net,nodes,name=name)
if(!is.null(layout))
  net <- network.addLayout(net,layout)
network.create(net, language, dir)
}

#meta function for igraph objects
network.fromIgraph <- function(G,layout=NULL,language=c("en","es"),dir="Network"){
nodes <- data.frame(name=V(G)$name)
links <- get.edgelist(G)
links <- data.frame(source=links[,1],target=links[,2])
for(i in list.vertex.attributes(G))
  nodes[[i]] <- get.vertex.attribute(G,i)
for(i in list.edge.attributes(G))
  links[[i]] <- get.edge.attribute(G,i)
network.all(nodes,links,layout=layout,language=language,dir=dir)
}
