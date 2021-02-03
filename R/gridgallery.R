galleryJSON <- function(gallery){
  json <- list(nodes=gallery$nodes)
  json$options <- gallery$options
  return(toJSON(json))
}

galleryCreate <- function(gallery, dir){
  language <- getLanguageScript(gallery)
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","functions.js",language,"gallery.js"), function(){ return(imgWrapper(gallery,galleryJSON,dir)) })
}

gridGallery <- function(nodes, name = NULL, label = NULL, info = NULL, image = NULL, main = NULL, note = NULL, help = NULL, language = c("en", "es", "ca"), dir = NULL){
  name <- nameByLanguage(name,language,nodes)
  if(!is.null(name)) rownames(nodes) <- nodes[[name]]

  options <- list(nodeName = name)
  if(is.null(label)){
      options[["nodeLabel"]] <- name
  }else if(label!=""){
      options[["nodeLabel"]] <- label
  }
  if (!is.null(info)) options[["nodeInfo"]] <- info
  if (!is.null(main)) options[["main"]] <- main
  if (!is.null(note)) options[["note"]] <- note
  if (!is.null(help)) options[["help"]] <- help

  if (!is.null(image)){
    if(length(setdiff(image,colnames(nodes)))){
      warning("image: names must match in nodes colnames.")
    }else{
      options[["imageItems"]] <- image
    }
  }

  gallery <- structure(list(nodes=nodes,options=options),class="gridGallery")
  if (!is.null(dir)) galleryCreate(gallery,dir)
  return(gallery)
}
