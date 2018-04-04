timelineJSON <- function(time){
  json <- list(nodes=time$nodes,options=time$options)
  return(toJSON(json))
}

timeCreate <- function(time, language = c("en","es"), dir = "timeCoin", show = TRUE){
  if(length(language) && language[1]=="es")
    language <- "es.js"
  else
    language <- "en.js"
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","functions.js",language,"colorScales.js","timeline.js"), timelineJSON(time), show)
}

timeCoin <- function(nodes, name = "name", start = "start", end = "end", group = NULL, text = NULL, main = NULL, note = NULL, 
                     cex = 1, language = c("en","es"), dir = NULL, show = TRUE){
  options <- list(name = name, start = start, end = end, cex = as.numeric(cex))
  if (!is.null(group)) options[['group']] <- group
  if (!is.null(text)) options[['text']] <- text
  if (!is.null(main)) options[['main']] <- main
  if (!is.null(note)) options[['note']] <- note
  time <- structure(list(nodes=nodes,options=options,call=match.call()),class="timeCoin")
  if (!is.null(dir)) timeCreate(time, language = language, dir = dir, show = show)
  return(time)
}
