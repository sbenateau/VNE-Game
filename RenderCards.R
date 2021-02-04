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
  print(toolID)
  # return UI
  return(
    bsCollapsePanel(
      title = switch(toolID,
                     G = "Graphique",
                     A = "Calculer le nombre moyen d’individus",
                     E = "Calculer le nombre moyen d’espèces",
                     N = "Compter le nombre d’observations",
                     V = "Compter le nombre de fois où les espèces ont été vues",
                     M = "Mélanger les données",
                     B = "Graphique avec barres d'erreurs",
                     R = "Regrouper des lignes",
                     S = "Scientifique",
                     C = "Carte",
                     D = "Données",
                     T = "Top",
                     "Autre outil"),
      tagList(
        switch(toolID,
               G = plotOutput(ns(id)),
               B = plotOutput(ns(id)),
               C = plotOutput(ns(id)),
               S = uiOutput(ns(id)),
               D = DT::dataTableOutput(ns(id)),
               R = DT::dataTableOutput(ns(id)),
               A = DT::dataTableOutput(ns(id)),
               N = DT::dataTableOutput(ns(id)),
               E = DT::dataTableOutput(ns(id)),
               V = DT::dataTableOutput(ns(id)),
               tableOutput(ns(id))
        )
      )
    )
  )
}

renderCards <- function(input, output, session, 
                        id, parsedCode, Results, fullCode) {
  toolCode <- parsedCode[id]
  
  # values are returned in `rv` in app.R
  output[[id]] <- switch(unlist(strsplit(toolCode, ""))[1],
                         G = renderPlot(Results),
                         C = renderPlot(Results),
                         B = renderPlot(Results),
                         D = DT::renderDataTable(Results, options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE),
                         R = DT::renderDataTable(Results, options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE),
                         A = DT::renderDataTable(Results, options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE),
                         N = DT::renderDataTable(Results, options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE),
                         E = DT::renderDataTable(Results, options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE),
                         V = DT::renderDataTable(Results, options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE),
                         renderTable(head(Results,40)))
  
}