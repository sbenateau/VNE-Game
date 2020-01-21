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
                     M = "Mélanger les données",
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
               D = DT::dataTableOutput(ns(id)),
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
                         D = DT::renderDataTable(Results, options = list(pageLength = 24, dom = 'tp', searching = FALSE)),
                         S = renderUI({
                           # Découpage du code
                           informations <- codeInformation(fullCode)

                           if (!"D" %in% informations$toolUsed) { # Pas de données
                             # Il manque les données : vidéo à refaire
                             link = "1143628"
                           } else if (length(informations$toolUsed) == 1){ # Seulement les données
                             link = "377267769"
                           } else if ("M" %in% informations$toolUsed){ # utilisation de random all (non souhaité)
                             # améliorer la vidéo en n'ayant pas forcement de graph
                             link = "377267700"
                           } else if ("P" %in% informations$toolUsed) { # Utilisation d'une carte (non souhaité)
                             # Pas de carte nécessaire pour répondre à la question : vidéo à refaire
                             link = "1143628"
                           } else if (!"Env" %in% informations$varUsed){ # Pas env
                             # video à faire manque la carte environnement
                             link = "1143628"
                           } else if (!"R" %in% informations$toolUsed){ # Pas de regroupement
                             # regroupement souhaitable
                             link = "377267820"
                           } else if ("So" %in% informations$funUsed & !"Mo" %in% informations$funUsed){ # Seulement une somme
                             # fonction non adaptée
                             link = "377267625"
                           } else if ("Co" %in% informations$funUsed){ # comptage
                             # fonction non adaptée
                             link = "377267747"
                           } else if (!"Pla" %in% informations$varUsed){ # Pas placette
                             # video à faire manque la carte placette
                             link = "1143628"
                           } else if (!"Num" %in% informations$varUsed){ # Pas numéro d'observation
                             # video à faire manque la carte placette
                             link = "1143628"
                           } else if (!"G" %in% informations$toolUsed){ # No graph
                             # Manque de graph nécessaire : vidéo à refaire
                             link = "1143628"
                           } else if (!"B" %in% informations$toolUsed){ # No error bars
                             link = "1143628"
                           } else if ("TOUT BONNN"){ # good graph
                             link = "377267723"
                           } else { # nope
                             link = sample(c("377267872", "377267851", "377267555"))
                             
                           }
                           HTML(paste0('<iframe src="https://player.vimeo.com/video/',link,'" width="640" height="480" frameborder="0" allow="autoplay" allowfullscreen></iframe>'))
                         }),
                         renderTable(head(Results,40)))
  
}