timelineJSON <- function(time){
  json <- list(nodes=time$nodes,options=time$options)
  return(toJSON(json))
}

timeCreate <- function(time, dir = "timeCoin"){
  language <- getLanguageScript(time)
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","functions.js",language,"colorScales.js","timeline.js"), timelineJSON(time))
}

timeCoin <- function(nodes, name = "name", start = "start", end = "end",
                     group = NULL, text = NULL, main = NULL, note = NULL, 
                     cex = 1, language = c("en","es","ca"), dir = NULL){
  options <- list(name = name, start = start, end = end, cex = as.numeric(cex))
  if (!is.null(group)) options[['group']] <- group
  if (!is.null(text)) options[['text']] <- text
  if (!is.null(main)) options[['main']] <- main
  if (!is.null(note)) options[['note']] <- note
  if(!is.null(language)) options[['language']] <- language[1]
  time <- structure(list(nodes=nodes,options=options),class="timeCoin")
  if (!is.null(dir)) timeCreate(time, dir)
  return(time)
}
