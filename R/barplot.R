barplotJSON <- function(bar){
  return(toJSON(list(nodes=bar$nodes,links=bar$links,options=bar$options)))
}

barplot.start <- function(nodes, links, options){
  lNames <- unique(links$source,links$target)
  notListed <- length(setdiff(lNames,nodes[[options$name]]))
  if(notListed!=0)
    stop(paste(notListed," node link(s) not defined in nodes data frame."))
  structure(list(nodes=nodes, links=links, options=options, call=match.call()), class="barplot")
}

barplot.create <- function(bar, language = c("en","es"), dir = "barplot"){
  if(language[1]=="es")
    language <- "es.js"
  else
    language <- "en.js"
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","functions.js",language,"barplot.js"), barplotJSON(bar))
}

barCoin <- function(nodes, links, name = "name", label = NULL, text = NULL, incidences = "frequency", coincidences = "coincidences", minor = NULL, language = c("en","es"), dir = NULL){
  if (name %in% names(nodes) & incidences %in% names(nodes) & coincidences %in% names(links)) {
    options <- list(name = name, coincidences = coincidences, incidences = incidences)
    if(!is.null(minor))
      options[["minor"]] <- minor
    if(!is.null(label))
      options[["label"]] <- label
    if(!is.null(text))
      options[["text"]] <- text
    bar <- barplot.start(nodes, links, options)
    if (!is.null(dir)) barplot.create(bar,language=language,dir=dir)
    return(bar)
  }
  else warning("name, incidences or coincidences are not in nodes or links data frames")
}

cbarCoin <- function(nodes, links, name = "name", label = NULL, text = NULL, incidences = "frequency", coincidences = "coincidences", expected = "expected", confidence.interval = NULL,  minor = NULL, language = c("en","es"), dir = NULL){
  if (name %in% names(nodes) & incidences %in% names(nodes) & coincidences %in% names(links) & expected %in% names(links)) {
    options <- list(name = name, coincidences = coincidences, incidences = incidences, expected = expected)
    if(!is.null(minor))
      options[["minor"]] <- minor
    if(!is.null(label))
      options[["label"]] <- label
    if(!is.null(text))
      options[["text"]] <- text
    if(!is.null(confidence.interval))
      options[["line"]] <- confidence.interval
    bar <- barplot.start(nodes, links, options)
    if (!is.null(dir)) barplot.create(bar,language=language,dir=dir)
    return(bar)
  }
  else warning("name, incidences, coincidences or expected are not in nodes or links data frames")
}
