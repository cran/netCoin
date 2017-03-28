timelineJSON <- function(time){
  json <- list(nodes=time$nodes,options=time$options)
  return(toJSON(json))
}

timeCreate <- function(time, language = c("en","es"), dir = "timeCoin"){
  if(language[1]=="es")
    language <- "es.js"
  else
    language <- "en.js"
  createHTML(dir, c("reset.css","styles.css"), c("d3.min.js","jspdf.min.js","functions.js",language,"timeline.js"), timelineJSON(time))
}

timeCoin <- function(nodes, name = "name", start = "start", end = "end", group = NULL, text = NULL, main = NULL, note = NULL, language = c("en","es"), dir = NULL){
  options <- list(name=name,start=start,end=end)
  if (!is.null(group)) options[['group']] <- group
  if (!is.null(text)) options[['text']] <- text
  if (!is.null(main)) options[['main']] <- main
  if (!is.null(note)) options[['note']] <- note
  time <- structure(list(nodes=nodes,options=options,call=match.call()),class="timeCoin")
  if (!is.null(dir)) timeCreate(time,language=language,dir=dir)
  return(time)
}
