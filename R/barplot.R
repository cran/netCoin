barplotJSON <- function(bar){
  return(toJSON(list(nodes=bar$nodes,links=bar$links,options=bar$options)))
}

barStart <- function(nodes, links, options){
  lNames <- unique(links$source,links$target)
  notListed <- length(setdiff(lNames,nodes[[options$name]]))
  if(notListed!=0)
    stop(paste(notListed," node link(s) not defined in nodes data frame."))
  structure(list(nodes=nodes, links=links, options=options, call=match.call()), class="barCoin")
}

barCreate <- function(bar, language = c("en","es"), dir = "barCoin"){
  if(language[1]=="es")
    language <- "es.js"
  else
    language <- "en.js"
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","functions.js",language,"barplot.js"), barplotJSON(bar))
}

barCoin <- function(nodes, links, name = "name", label = NULL, text = NULL, incidences = "frequency", coincidences = "coincidences", note = NULL, language = c("en","es"), dir = NULL){
  if (name %in% names(nodes) & incidences %in% names(nodes) & coincidences %in% names(links)) {
    options <- list(name = name, incidences = incidences, coincidences = coincidences)
    if(!is.null(note))
      options[["note"]] <- note
    if(!is.null(label))
      options[["label"]] <- label
    if(!is.null(text))
      options[["text"]] <- text
    bar <- barStart(nodes, links, options)
    if (!is.null(dir)) barCreate(bar,language=language,dir=dir)
    return(bar)
  }
  else warning("name, incidences or coincidences are not in nodes or links data frames")
}

cbarCoin <- function(nodes, links, name = "name", label = NULL, text = NULL, incidences = "frequency", coincidences = "coincidences", expected = "expected", confidence.interval = NULL,  note = NULL, language = c("en","es"), dir = NULL){
  if (name %in% names(nodes) & incidences %in% names(nodes) & coincidences %in% names(links) & expected %in% names(links)) {
    options <- list(name = name, coincidences = coincidences, incidences = incidences, expected = expected)
    if(!is.null(note))
      options[["note"]] <- note
    if(!is.null(label))
      options[["label"]] <- label
    if(!is.null(text))
      options[["text"]] <- text
    if(!is.null(confidence.interval))
      options[["line"]] <- confidence.interval
    bar <- barStart(nodes, links, options)
    if (!is.null(dir)) barCreate(bar,language=language,dir=dir)
    return(bar)
  }
  else warning("name, incidences, coincidences or expected are not in nodes or links data frames")
}
