#create json
networkJSON<-function(net){

  links <- net$links
  tree <- net$tree
  nodes <- net$nodes
  options <- net$options

  name <- as.character(nodes[[options$nodeName]])

  #prepare links
  if(length(links)){

    #get link intensity
    count <- 3
    while(count <= ncol(links) && is.null(options$linkIntensity)){
        cname <- colnames(links)[count]
        if(cname!="_frame_" && is.numeric(links[,cname]))
          options$linkIntensity <- cname
        count <- count + 1
    }

    sourcenames <- as.character(links$Source)
    targetnames <- as.character(links$Target)

    nlinks <- nrow(links)
    source <- numeric(nlinks)
    target <- numeric(nlinks)
    for(i in seq_len(nlinks)){
      source[i] <- which(sourcenames[i]==name)-1
      target[i] <- which(targetnames[i]==name)-1
    }

    links$Source <- source
    links$Target <- target
  }

  #prepare tree
  if(length(tree)){
      sourcenames <- as.character(tree$Source)
      targetnames <- as.character(tree$Target)

      checkdup <- targetnames
      if("_frame_" %in% names(tree))
        checkdup <- paste0(targetnames,tree[["_frame_"]])

      if(all(!duplicated(checkdup))){
        nlinks <- nrow(net$tree)
        source <- numeric(nlinks)
        target <- numeric(nlinks)
        for(i in seq_len(nlinks)){
          source[i] <- which(sourcenames[i]==name)-1
          target[i] <- which(targetnames[i]==name)-1
        }

        tree$Source <- source
        tree$Target <- target

        tree <- as.list(tree)
        names(tree) <- NULL
      }else{
        tree <- NULL
        warning("tree: there must be only one parent per node")
      }
  }

  nodenames <- colnames(nodes)
  nodes <- as.list(nodes)
  names(nodes) <- NULL
  json <- list(nodes = nodes, nodenames = array(nodenames))
  if(length(links)){
    linknames <- colnames(links)
    links <- as.list(links)
    names(links) <- NULL
    json$links <- links
    json$linknames <- linknames
  }
  if(length(tree))
    json$tree <- tree
  json$options <- options
  
  return(toJSON(json))
}

# add layout
netAddLayout <- function(net,layout){
  if(nrow(layout)==nrow(net$nodes)){
    net$nodes[["fx"]] <- layout[,1]
    net$nodes[["fy"]] <- layout[,2]
  }else
    warning("layout: must have a coordinate per node")
  return(net)
}

getRawName <- function(filepath){
  filename <- strsplit(basename(filepath), split="\\.")[[1]]
  ext <- filename[length(filename)]
  filename <- paste0(filename[-length(filename)],collapse=".")
  return(paste(paste0(as.character(charToRaw(filename)),collapse=""),ext,sep="."))
}

#copy images to net graph
imgWrapper <- function(net,dir){
  imgDir <- paste(dir,"images",sep="/")
  if("imageItems" %in% names(net$options)){
    dir.create(imgDir, showWarnings = FALSE)
    if(is.null(net$options[["imageNames"]])){
      net$options[["imageNames"]] <- net$options[["imageItems"]]
      net$options[["imageItems"]] <- paste0(net$options[["imageItems"]],"_url")
      for(i in seq_along(net$options[["imageItems"]])){
        net$nodes[[net$options[["imageItems"]][i]]] <- net$nodes[[net$options[["imageNames"]][i]]]
        net$nodes[[net$options[["imageNames"]][i]]] <- sub("\\.[a-zA-Z0-9]+$","",basename(as.character(net$nodes[[net$options[["imageNames"]][i]]])))
      }
    }
    for(img in net$options[["imageItems"]]){
      net$nodes[[img]] <- vapply(as.character(net$nodes[[img]]),function(filepath){
        rawname <- getRawName(filepath)
        file.copy(filepath, paste(imgDir,rawname,sep="/"), overwrite = FALSE)
        paste("images",rawname,sep="/")
      },character(1))
    }
  }
  if(!is.null(net$options[["background"]])){
    if(file.exists(net$options[["background"]])){
      filepath <- net$options[["background"]]
      rawname <- getRawName(filepath)
      dir.create(imgDir, showWarnings = FALSE)
      file.copy(filepath, paste(imgDir,rawname,sep="/"))
      net$options[["background"]] <- paste0('url("',paste("images",rawname,sep="/"),'")')
    }
  }
  return(networkJSON(net))
}

#create html wrapper for network graph
netCreate <- function(net, dir = "netCoin"){
  #get language
  language <- getLanguageScript(net)

  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","jszip.min.js","functions.js",language,"colorScales.js","network.js"),function(){ return(imgWrapper(net,dir)) })
}
