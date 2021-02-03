wwwDirectory = function(){
  path <- system.file("www",package="netCoin")
  return(path)
}

createHTML <- function(directory, styles, dependencies, json){
  indexfile <- paste(directory, "index.html", sep = "/")
  if(file.exists(directory)){
    if(length(dir(directory))==0 || file.exists(indexfile)){
      unlink(directory, recursive = TRUE)
    }else{
      stop(paste0("directory: '",directory,"' already exists"))
    }
  }
  dir.create(directory)

  www <- wwwDirectory()
  html <- scan(file = paste(www, "template.html", sep = "/"), what = character(0), sep = "\n", quiet = TRUE)
  name <- strsplit(directory,"/")[[1]]
  name <- name[length(name)]
  html <- sub("titulo", name, html)

  scripts <- "<!--scripts-->"
  if(length(styles)){
    dir.create(paste(directory, "styles", sep = "/"),FALSE)
  }
  for(i in styles){
    scripts <- paste(scripts, paste0("<link rel=\"stylesheet\" type=\"text/css\" href=\"styles/",i,"\"></link>"), sep = "\n")
    file.copy(paste(www, i, sep = "/"), paste(directory, "styles", sep = "/"))
    if(i=="styles.css"){
      for(font in c("Roboto-Regular-latin.woff2","Roboto-Regular-latin-ext.woff2")){
        file.copy(paste(www, font, sep = "/"), paste(directory, "styles", sep = "/"))
      }
    }
  }

  if(length(dependencies)){
    dir.create(paste(directory, "scripts", sep = "/"),FALSE)
  }
  for(i in dependencies){
    scripts <- paste(scripts, paste0("<script src=\"scripts/",i,"\"></script>"), sep = "\n")
    file.copy(paste(www, i, sep = "/"), paste(directory, "scripts", sep = "/"))
  }
  html[html=="<!--scripts-->"] <- scripts

  if(!is.null(json)){
    if(is.function(json))
      json <- json()

    enc <- Encoding(json)
    if(enc=="latin1" || (l10n_info()[["Latin-1"]] && enc=="unknown")){
      Encoding(json) <- "latin1"
      json <- enc2utf8(json)
    }

    html[html=="<!--json-->"] <- paste0('<script type="application/json" id="data">',json,'</script>')
  }

  con <- file(indexfile, encoding = "UTF-8")
  writeLines(html,con)
  close(con)
}

getLanguageScript <- function(obj){
  if(typeof(obj)=="list" && !is.null(obj$options)){
    language <- obj$options$language
    if(!is.null(language) && language[1] %in% c("es","ca"))
      language <- paste0(language[1],".js")
    else
      language <- "en.js"
    return(language)
  }else
    return(NULL)
}


toJSON <- function(x){

  prepare_number <- function(x){
    mod <- suppressWarnings(x%%1)
    if(is.nan(mod)){
      warning("Non-finite values not supported")
      return("null")
    }
    if(mod!=0)
      x <- signif(x,4)
    return(toString(x))
  }

  sanitize_string <- function(x){
    x <- unname(x)
    n <- suppressWarnings(as.numeric(x))
    if(is.na(n)){
      x <- gsub("[[:cntrl:]]","",x)
      x <- deparse(x)
      if(l10n_info()[["Latin-1"]]){
        x <- gsub("([^\\])\\\\[0-7]{3}","\\1_",x)
        x <- gsub("<U\\+([0-9a-fA-F]{4})>","\\\\u\\1",x)
      }
      if(l10n_info()[["UTF-8"]]){
        x <- gsub("([^\\])\\\\x([0-9a-fA-F]{2})","\\1_",x)
      }
      return(x)
    }else
      return(prepare_number(n))
  }

  json <- ""
  if(inherits(x,"POSIXt")){
    json <- toJSON(as.character(x))
  }else if(length(x)<=1){
    if(is.null(x)||identical(is.na(x),TRUE)){
        json <- "null"
    }else if(is.vector(x)){
        if(is.numeric(x)){
          json <- prepare_number(x)
        }else if(is.logical(x)){
          if(x){
            json <- "true"
          }else{
            json <- "false"
          }
        }else if(is.character(x)){
          json <- sanitize_string(x)
        }else if(is.list(x)){
          if(length(x)==0){
            json <- "{}"
          }else if(is.null(names(x))){
            json <- paste0("[", toJSON(x[[1]]), "]", collapse = "")
          }else{
            aux <- paste0('"',names(x),'":',toJSON(x[[1]]))
            json <- paste0("{", aux, "}", collapse = "")
          }
        }
    }else if(is.factor(x)){
        json <- sanitize_string(as.character(x))
    }else if(is.array(x)){
        aux <- "null"
        if(length(dim)==1)
          aux <- toJSON(x[1])
        else if(length(dim)==2 && dim(x)[1] > 0 && dim(x)[2] > 0)
          aux <- toJSON(x[1,1])
        json <- paste0("[",aux,"]", collapse = "")
    }else if(is.data.frame(x)){
        aux <- apply(x, 1, function(x)  paste0('{', paste0('"',names(x)[1],'":',toJSON(x)), '}', collapse = ""))
        aux <- paste0(aux , collapse = ",")
        json <- paste0("[", aux, "]", collapse = "")
    }
  }else if(is.data.frame(x)){      
      aux <- lapply(seq_len(dim(x)[1]), function(x,z)
        paste0("{", paste0(lapply(seq_along(z[x,]), function(x,y,n)
          paste0('"',n[[x]],'":',toJSON(y[[x]])),
        y=z[x,], n=names(z)), collapse = ","), "}", collapse = ""),
      z=x)
      aux <- paste0(aux , collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
  }else if(is.list(x)){
      if(is.null(names(x))){
        aux <- vapply(x, function(x){
          if(is.vector(x)||is.factor(x))
            toJSON(array(x))
          else
            toJSON(x)
        }, character(1))
        aux <- paste0(aux, collapse = ",")
        json <- paste0("[", aux, "]", collapse = "")
      }else{
        aux <- vapply(x, toJSON, character(1))
        aux <- paste0('"',names(x),'":',aux)
        aux <- paste0(aux , collapse = ",")
        json <- paste0("{", aux, "}", collapse = "")
      }      
  }else if(is.array(x)){
      aux <- apply(x, 1, toJSON)
      aux <- paste0(aux, collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
  }else if(is.vector(x)||is.factor(x)){
      aux <- paste0(vapply(x, toJSON, character(1)), collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
  }
  return(json)
}
