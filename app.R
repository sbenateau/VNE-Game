######################################################
#      VNE Game
#
#         Authors: Simon Benateau, Sebastien Turpin and Elie Arnaud
#
#
#####################################################
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
      tags$img(src = "LOGO_ordonne.png", width = '100%'),
      HTML("<br><br>"),
      
      
      textInput("code", "Entrez votre code :", value = ""),
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
  Results <- reactive({
    code <- input$code
    
    if (code != "" ){
      
      # Get the data
     
      #Data <- data.frame(getDataInitial())
      # TODO allow different possibilities
      
      #separate code to get clear instructions
      tools <- parsedCode()
      #tools <- unlist(strsplit(code, ":"))
      
      # create a list with size = nomber of tools
      Results <- vector(mode = "list", length = length(tools))
      
      #loop to execute all the steps
      for (i in 1:length(tools)){
        # case tool = data
        if (substring(tools[i], 1, 1) ==  "D"){
          Parameters <- separateParametersTreatment(tools[i])
          Results[[i]] <- data.frame(getDataInitial(observatory = Parameters[2]))
        } else if (tools[i] == "M") {
          Results[[i]] = randomAll(Results[[i-1]])
        } else if (substring(tools[i], 1, 1) == "R"){
          Parameters <- separateParametersTreatment(tools[i])
          if (Parameters[[3]] == "Mo"){
            Results[[i]] <- Results[[i-1]] %>%
              dplyr::group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
              dplyr::summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = c("mean","se"))
            colnames(Results[[i]])[which(colnames(Results[[i]]) == "mean")]  <- correspond(Parameters[[2]], EquivalenceVar)
          } else if (Parameters[[3]] == "Me"){
            #https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
            Results[[i]] <- Results[[i-1]] %>%
              dplyr::group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
              dplyr::summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = "quantile")
            colnames(Results[[i]])[which(colnames(Results[[i]]) == "50%")]  <- correspond(Parameters[[2]], EquivalenceVar)
          }
        } else if (substring(tools[i], 1, 1) == "T"){
          Parameters <- separateParametersTreatment(tools[i])
          Results[[i]] <- Results[[i-1]] %>%
            dplyr::arrange(desc(!!sym(correspond(Parameters[[2]], EquivalenceVar))))
        } else if (substring(tools[i], 1, 1) == "G"){
          # get parameters (improve by locating the graph within the code)
          Parameters <- separateParametersTreatment(tools[i])
          # get the name of the column to check few thing
          colNamesData <- colnames(Results[[i-1]])
          # Add errors if the columns are not in the code
          # if sp is in the dataset, separate by  species (if species as columns then change)
          if ("Espece" %in% colNamesData & Parameters[[1]] != "Esp" & Parameters[[1]] != "Esp") facet = ggplot2::facet_wrap(.~Espece) else facet = NULL
          # if data not summarised plot points else plot barplot
          if (nrow(Results[[i-1]]) < 30) representation <- ggplot2::geom_col(ggplot2::aes_string(fill = correspond(Parameters[[1]], EquivalenceVar))) else representation <- geom_jitter(aes_string(col = correspond(Parameters[[1]], EquivalenceVar)))
          # graph is too specific right now
          Results[[i]] <- ggplot2::ggplot(Results[[i-1]], ggplot2::aes_string(x = correspond(Parameters[[1]], EquivalenceVar), y = correspond(Parameters[[2]], EquivalenceVar)), environment = environment()) +
            representation +
            facet +
            ggplot2::theme_minimal()+
            ggplot2::theme(axis.text=element_text(size=12),
                           axis.title=element_text(size=16),
                           strip.text.x = element_text(size = 14))
        } else if (substring(tools[i], 1, 1) == "B"){
          Parameters <- separateParametersTreatment(tools[i-1])
          Results[[i-1]]$data <- Results[[i-1]]$data %>%
            dplyr::mutate(ymin = Nombre_individus + 1.96 * se,
                          ymax = Nombre_individus - 1.96 * se)
          Results[[i]] <- Results[[i-1]] + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2)
        } else if (substring(tools[i], 1, 1) == "P"){
          Parameters <- separateParametersTreatment(tools[i])
          if (!Parameters[[2]] == "Dep"){
            departements_L93 <- mapToPlot()
            geoData = dplyr::left_join(departements_L93, Results[[i-1]], by = 'Departement') %>%
              sf::st_transform(2154)
            
            Results[[i]] <- tmap::tm_shape(geoData) +
              tmap::tm_borders() +
              tmap::tm_fill(col = correspond(Parameters[[2]], EquivalenceVar))
          }
          
        }
        else{
          msg <- paste0("Tool", tools[i], "seems to be mis-formated (code:", paste0(tools, collapse = ":"), ")\n")
          warning(msg)
        }
        # TODO in case of typo, any default value?
      }
      # Remove se column for display (only used to add error bars)
      if (any(stringr::str_detect(tools,"Mo"))){
        Results[[which(str_detect(tools, "Mo"))]] <- Results[[which(str_detect(tools, "Mo"))]][-which(names(Results[[which(str_detect(tools, "Mo"))]]) == "se")]
      }
      if (any(stringr::str_detect(tools,"Me"))){
        Results[[which(str_detect(tools, "Me"))]] <- Results[[which(str_detect(tools, "Me"))]][-which(names(Results[[which(str_detect(tools, "Me"))]]) == "0%" | names(Results[[which(str_detect(tools, "Me"))]]) == "25%")]
      }
      Results
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
    missingTool <- informations$toolUsed[!informations$toolUsed %in% c("D", "M", "R", "P", "T", "G", "B" )]
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
                                                      Results()[[toolPosition]], input$code)
    })
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

