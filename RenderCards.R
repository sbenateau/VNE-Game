#' @title renderCardsUI
#' 
#' @description UI part of the renderCards module. Used to render a display for different cards.
#' 
#' @param id The n° of the tool in the entered sequence (cf. parsedCode)
#' @param parsedCode The sequence entered by the player
#' 
#' 
renderCardsUI <- function(id, parsedCode) {
  ns <- NS(id)
  toolCode <- parsedCode[id]
  
  # retrieve first letter: what does the tool do
  # DISABLED: keep toolCode if needed furtherly
  toolID = unlist(strsplit(toolCode, ""))[1]
  
  # return UI
  return(
    bsCollapsePanel(
      title = switch(toolID,
                     G = "Graphique",
                     R = "Regrouper des lignes",
                     S = "Scientifique",
                     D = "Données",
                     "Autre outil"),
      tagList(
        switch(toolID,
               G = plotOutput(ns(id)),
               S = uiOutput(ns(id)), 
               tableOutput(ns(id))
        )
      )
    )
  )
}

renderCards <- function(input, output, session, 
                        id, parsedCode, Results, fullCode) {
  toolCode <- parsedCode[id]
  
  # retrieve first letter: what does the tool do
  # DISABLED: keep toolCode if needed furtherly
  # toolCode = unlist(strsplit(toolCode, ""))[1]
  
  # values are returned in `rv` in app.R
  output[[id]] <- switch(unlist(strsplit(toolCode, ""))[1],
                         G = renderPlot(Results),
                         S = renderUI({
                           if (fullCode == "D:S") {
                             link = "377267799" # Seulement le tableau des données
                           } else if (fullCode == "D:GEnvXyInd:S"){
                             link = "377267820"
                           } else {
                             link = "377267700"
                           }
                           HTML(paste0('<iframe src="https://player.vimeo.com/video/',link,'" width="640" height="480" frameborder="0" allow="autoplay" allowfullscreen></iframe>'))
                         }),
                         renderTable(head(Results,40)))
  
}