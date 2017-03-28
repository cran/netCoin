wwwDirectory = function(){
  path <- system.file("www",package="netCoin")
  return(path)
}

createHTML <- function(directory, styles, dependencies, json){
  if(file.exists(directory))
    unlink(directory, recursive = TRUE)
  dir.create(directory)

  www <- wwwDirectory()
  html <- scan(file = paste(www, "template.html", sep = "/"), what = character(0), sep = "\n", quiet = TRUE)
  name <- strsplit(directory,"/")[[1]]
  name <- name[length(name)]
  html <- sub("titulo", name, html)

  if(length(dependencies))
    dir.create(paste(directory, "scripts", sep = "/"),FALSE)
  scripts <- "<!--scripts-->";
  for(i in dependencies){
    scripts <- paste(scripts, paste0("<script src=\"scripts/",i,"\"></script>"), sep = "\n");
    file.copy(paste(www, i, sep = "/"), paste(directory, "scripts", sep = "/"))
  }
  html <- sub("<!--scripts-->", scripts, html)

  if(length(styles))
    dir.create(paste(directory, "styles", sep = "/"),FALSE)
  scripts <- "<!--scripts-->";
  for(i in styles){
    scripts <- paste(scripts, paste0("<link rel=\"stylesheet\" type=\"text/css\" href=\"styles/",i,"\"></link>"), sep = "\n");
    file.copy(paste(www, i, sep = "/"), paste(directory, "styles", sep = "/"))
  }
  html <- sub("<!--scripts-->", scripts, html)

  if(is.function(json))
    json <- json()
  html <- sub("<!--json-->",paste0('<script type="application/json" id="data">',json,'</script>'),html)
  write(html, paste(directory, "index.html", sep = "/"))
}


toJSON <- function(x){
  sanitize.string <- function(x){
    return(paste0('"',gsub('"','\'',gsub("[\r\n\t]","",x)),'"'))
  }
  json <- ""
  if(length(x)<=1){
    if(is.null(x)||identical(is.na(x),TRUE)){
      json <- "null"
    }else{
      if(is.vector(x)){
	if(is.numeric(x))
	  json <- x
	if(is.logical(x)){
	  if(identical(x,TRUE))
	    json <- "true"
	  if(identical(x,FALSE))
	    json <- "false"
	}
	if(is.character(x))
	  json <- sanitize.string(x)
	if(is.list(x)){
	  if(length(x)==0)
	    json <- "{}"
	  else{
	    if(is.null(names(x))){
	      json <- paste0("[", toJSON(x[[1]]), "]", collapse = "")
	    }else{
	      aux <- paste0('"',names(x),'":',toJSON(x[[1]]))
	      json <- paste0("{", aux, "}", collapse = "")
	    }
	  }
	}
      }
      if(is.factor(x)){
	json <- sanitize.string(x)
      }
      if(is.array(x)){
	if(dim(x)[1]>0&&dim(x)[2]>0)
	  aux <- toJSON(x[1,1])
	else
	  aux <- ""
	json <- paste0("[",aux,"]", collapse = "")
      }
      if(is.data.frame(x)){
	aux <- apply(x, 1, function(x)  paste0('{', paste0('"',names(x)[1],'":',toJSON(x)), '}', collapse = ""))
	aux <- paste0(aux , collapse = ",")
	json <- paste0("[", aux, "]", collapse = "")
      }
    }
  } else {
    if(is.vector(x)){
      aux <- paste0(lapply(x, toJSON), collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
    }
    if(is.array(x)){
      aux <- apply(x, 1, toJSON)
      aux <- paste0(aux, collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
    }
    if(is.data.frame(x)){      
      aux <- lapply(seq_len(dim(x)[1]), function(x,z) paste0("{", paste0(lapply(seq_along(z[x,]), function(x,y,n) paste0('"',n[[x]],'":',toJSON(y[[x]])), y=z[x,], n=names(z)), collapse = ","), "}", collapse = ""), z=x)
      aux <- paste0(aux , collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
    }
    if(is.list(x)&&!is.data.frame(x)){
      if(is.null(names(x))){
	aux <- paste0(lapply(x, toJSON), collapse = ",")
	json <- paste0("[", aux, "]", collapse = "")
      }else{
	aux <- lapply(seq_along(x), function(x,y,n) paste0('"',n[[x]],'":',toJSON(y[[x]])),y=x,n=names(x))
	aux <- paste0(aux , collapse = ",")
	json <- paste0("{", aux, "}", collapse = "")
      }      
    }
  }
  return(json)
}
