######################################################
#      VNE Game
#
#         Authors: Simon Benateau, Sebastien Turpin and Elie Arnaud
#
#
#####################################################

# Masquer le changement de page ???


# TODO ajouter une legende indiquant ce qu'est la valeur de variable (ex: moyenne individus)
# D:RZonEnvSoInd:REnvMoInd:GEnvXyInd

library(shiny)
library(memisc) # to view iframe
library(shinyBS)
#library(DT)
# load the functions to run the game
source('functionGame.R')
source('RenderCards.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  #titlePanel("Datathon"),
  
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      # Logo de l'application
      tags$img(src = "Logo_Paper.png", width = '100%'),
      HTML("<br><br>"),
      
      
      textInput("code", "Entrez votre code :", value = ""),
      tags$head(
        tags$style(HTML('#run{background-color:orange}'))
      ),
      submitButton(text = "Validez le code", icon = NULL, width = NULL)
    ),
    
    # Show the outputs from the treatment
    mainPanel(
      uiOutput("Cards"),
      #tableOutput("Debug"),
      textOutput("Error")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Variable initialization
  rv <- reactiveValues()
  
  #Import map
  mapToPlot <- reactive({
    extraWD = "data"
    if (!file.exists(file.path(extraWD, "departement.zip"))) {
      githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-07-14-introduction-to-mapping-with-sf-and-co/data/departement.zip"
      download.file(githubURL, file.path(extraWD, "departement.zip"))
      unzip(file.path(extraWD, "departement.zip"), exdir = extraWD)
    }
    departements_L93 <- sf::st_read(dsn = extraWD, layer = "DEPARTEMENT",
                                    quiet = TRUE) %>%
      dplyr::rename(Departement = CODE_DEPT) %>%
      sf::st_transform(2154)
    departements_L93
  })
  
  # Breakdown code
  parsedCode <- reactive({
    unlist(strsplit(input$code, ":"))
  })
  
  # Reactive containing a list for each tool.
  results <- reactive({
    code <- input$code
    
    if (code != "" ){
      
      # Get the data
      
      #Data <- data.frame(getDataInitial(observatory = "Ois"))
      # TODO allow different possibilities
      
      #separate code to get clear instructions
      tools <- parsedCode()
      #tools <- unlist(strsplit(code, ":"))
      
      # create a list with size = nomber of tools
      results <- vector(mode = "list", length = length(tools))
      
      #loop to execute all the steps
      for (i in 1:length(tools)){
        Parameters <- separateParametersTreatment(tools[i])
        toolname = substring(tools[i], 1, 1)
        # case tool = data
        if (toolname ==  "D"){
          results[[i]] <- data.frame(getDataInitial(observatory = Parameters[2]))
        } else if (toolname == "M") {
          results[[i]] = randomAll(results[[i-1]])
        } else if (toolname == "A") {
          results[[i]] <- abundanceCard(dataset = results[[i-1]], groupVariable = correspond(Parameters[[2]], EquivalenceVar))
        } else if (toolname == "E") {
          results[[i]] <- diversityCard(dataset = results[[i-1]], groupVariable = correspond(Parameters[[2]], EquivalenceVar))
        } else if (toolname == "N") {
          results[[i]] <- observationCard(dataset = results[[i-1]], groupVariable = correspond(Parameters[[2]], EquivalenceVar))
        }
        else if (toolname == "R"){
          results[[i]] <- makeSummary(tools, results, i)
        } else if (toolname == "T"){
          results[[i-1]] <- makeTop(results[[i-1]])
        # } else if (toolname == "T"){
        #   results[[i]] <- results[[i-1]] %>%
        #     dplyr::arrange(desc(!!sym(correspond(Parameters[[2]], EquivalenceVar))))
        } else if (toolname == "G"){
          results[[i]] <- makeGraphEasy(dataset = results[[i-1]])
        } else if (toolname == "B"){
          results[[i]] <- makeErrorBars(tools, results, i)
        } else if (toolname == "C"){
          if ("Departement" %in% colnames(results[[i-1]])){
            departements_L93 <- mapToPlot()
            print(departements_L93)
            print(results[[i-1]])
            print(nrow(results[[i-1]]))
            geoData = dplyr::left_join(departements_L93, results[[i-1]], by = 'Departement') %>%
              sf::st_transform(2154)
            
            
            results[[i]] <- tmap::tm_shape(geoData) +
              tmap::tm_borders() +
              tmap::tm_fill(col = colnames(results[[i-1]])[2])
          }
        } else if (toolname == "V"){
          results[[i]] <- getSpeciesNumber(results[[i-1]])
        } else {
          msg <- paste0("Tool", tools[i], "seems to be mis-formated (code:", paste0(tools, collapse = ":"), ")\n")
          warning(msg)
        }
        # TODO in case of typo, any default value?
      }
      
      # Remove se column for display (only used to add error bars)
      if (any(stringr::str_detect(tools,"Mo"))){
        results[[which(str_detect(tools, "Mo"))]] <- results[[which(str_detect(tools, "Mo"))]][-which(names(results[[which(str_detect(tools, "Mo"))]]) == "se")]
      }
      # clean results
      for (i in 1:length(tools)){

        if ("IntervalleDeConfiance" %in% colnames(results[[i]])){
          results[[i]] <- results[[i]][ , -which(colnames(results[[i]]) == "IntervalleDeConfiance")]
        }
      }
      results
    }
    
  })
  
  # Procedurally generate UI by calling multiple times the renderCards module
  output$Cards <- renderUI({
    # Check for input's content
    if(length(parsedCode()) != 0){
      do.call(bsCollapse,
              lapply(seq_along(parsedCode()), function (toolPosition){
                renderCardsUI(toolPosition, parsedCode())
              })
      )
    }
    else
      # no display
      NULL
  })
  output$Debug <- renderTable({
    fullCode <- input$code
    Debug <- vector(mode = "list", length = 1)
    # Découpage du code
    informations <- codeInformation(fullCode)
    # function utilisées
    Debug[[1]] <- c(!"D" %in% informations$toolUsed, '!"D" %in% informations$toolUsed', 'Pas de données')
    Debug[[2]] <- c(length(informations$toolUsed) == 1, 'length(toolUsed) == 1', 'Seulement les données')
    Debug[[3]] <- c("M" %in% informations$toolUsed, '"M" %in% informations$toolUsed', 'utilisation de random all (non souhaité)')
    Debug[[4]] <- c("P" %in% informations$toolUsed, '"P" %in% informations$toolUsed', 'Utilisation dune carte (non souhaité)')
    Debug[[5]] <- c(!"Env" %in% informations$varUsed, '!"Env" %in% informations$varUsed', 'anque Environnement dans les variables')
    Debug[[6]] <- c(!"R" %in% informations$toolUsed, '!"R" %in% informations$toolUsed', 'Pas de regroupement')
    Debug[[7]] <- c("So" %in% informations$funUsed & !"Mo" %in% informations$funUsed, '"So" %in% informations$funUsed & !"Mo" %in% informations$funUsed', 'Pas de données')
    Debug[[8]] <- c("Co" %in% informations$funUsed, '"Co" %in% informations$funUsed', 'Utilisation dun comptage (non souhaité)')
    Debug[[9]] <- c(!"Pla" %in% informations$varUsed, '!"Pla" %in% informations$varUsed', 'Manque placette dans les variables')
    Debug[[10]] <- c(!"Num" %in% informations$varUsed, '!"Num" %in% informations$varUsed', 'Manque numéro dobservation  dans les variables')
    Debug[[11]] <- c(!"G" %in% informations$toolUsed, '!"G" %in% informations$toolUsed', 'Pas de graphique')
    Debug[[12]] <- c(!"B" %in% informations$toolUsed, '!"B" %in% informations$toolUsed', 'Pas de barres derreur')
    matrix(unlist(Debug), ncol = 3, byrow = TRUE)
  })
  
  output$Error <- renderText({
    # Découpage du code
    fullCode <- input$code
    informations <- codeInformation(fullCode)
    missingVariable <- informations$varUsed[!informations$varUsed %in% as.character(EquivalenceVar$input)]
    message <- c()
    if (length(missingVariable) == 1){
      message <- c(message, paste("La variable : ", missingVariable, "n'a pas été trouvée, "))
    } else if (length(missingVariable) > 1){
      message <- c(message, paste("Les variables : ", paste(missingVariable, collapse = ", "), "n'ont pas été trouvées, "))
    }
    missingFunction <- informations$funUsed[!informations$funUsed %in% as.character(EquivalenceFun$input)]
    if (length(missingFunction) == 1){
      message <- c(message, paste("La fonction : ", missingFunction, "n'a pas été trouvée, "))
    } else if (length(missingFunction) > 1){
      message <- c(message, paste("Les fonctions : ", paste(missingFunction, collapse = ", "), "n'ont pas été trouvées, "))
    }
    missingTool <- informations$toolUsed[!informations$toolUsed %in% c("D", "M", "R", "P", "T", "G", "B", "A", "C", "E" )]
    if (length(missingTool) == 1){
      message <- c(message, paste("L'outil : ", missingTool, "n'a pas été trouvé, "))
    } else if (length(missingTool) > 1){
      message <- c(message, paste("Les outils : ", paste(missingTool, collapse = ", "), "n'ont pas été trouvés, "))
    }
    if (length(missingVariable) >= 1 | length(missingFunction) >= 1 | length(missingTool) >= 1){
      paste(paste(message, collapse = ""), "Vérifiez votre code.")
    }
  })
  
  # Procedurally generate server by calling multiple times renderCards module's server
  observe(
    sapply(seq_along(parsedCode()), function (toolPosition){
      rv[[ as.character(toolPosition)]] <- callModule(renderCards, toolPosition,
                                                      toolPosition, # repeated to be given as additional argument
                                                      parsedCode(),
                                                      results()[[toolPosition]], input$code)
    })
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
