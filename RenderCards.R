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
                     B = "Graphique avec barres d'erreurs",
                     R = "Regrouper des lignes",
                     S = "Scientifique",
                     P = "Carte",
                     D = "Données",
                     T = "Tri",
                     "Autre outil"),
      tagList(
        switch(toolID,
               G = plotOutput(ns(id)),
               B = plotOutput(ns(id)),
               P = plotOutput(ns(id)),
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
                         P = renderPlot(Results),
                         B = renderPlot(Results),
                         S = renderUI({
                           # Découpage du code
                           # noms des outils
                           toolUsed <-unlist(lapply(strsplit(fullCode, ":"), function(x) substr(x,0,1)))
                           toolUsed <- toolUsed[!sapply(toolUsed, function (x) x == "S")]
                           # variables utilisées
                           toolUsed <-unlist(lapply(strsplit(fullCode, ":"), function(x) substr(x,0,1)))
                           # function utilisées
                           if (!"D" %in% toolUsed) { # no data
                             # Il manque les données : vidéo à refaire
                             link = "1143628"
                           } else if (length(toolUsed) == 1){ # only data
                             link = "377267769"
                           } else if ("M" %in% toolUsed){ # random all was used
                             link = "377267700"
                           } else if ("P" %in% toolUsed) { # map
                             # Pas de carte nécessaire : vidéo à refaire
                             link = "1143628"
                           } else if (fullCode == "D:GEnvXyInd:S"){
                             link = "377267820"
                           } else {
                             link = "377267700"
                           }
                           HTML(paste0('<iframe src="https://player.vimeo.com/video/',link,'" width="640" height="480" frameborder="0" allow="autoplay" allowfullscreen></iframe>'))
                         }),
                         renderTable(head(Results,40)))
  
}