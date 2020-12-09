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
library(shinysense)
# load the functions to run the game
source('functionGame.R')
source('RenderCards.R')
source('functions_detection.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      # Logo de l'application
      tags$img(src = "logo_papers.png", width = '100%'),
      HTML("<br><br>"),
      selectInput("choice", "Type d'interface :", c("image","code"), selected = "image"),
      actionButton("choose_ui","Valider le choix d'interface"),
      HTML("<br><br>"),
      uiOutput("code_text_ui"),
      uiOutput("code_button_ui"),
      uiOutput("cam_ui")
    ),
    
    # Show the outputs from the treatment
    mainPanel(
      uiOutput("Cards")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  rv <- reactiveValues(
    ui = "NULL"
  )
  
  # make ui
  observeEvent(input$choose_ui, {
    if (input$choice == "code"){
      rv$ui = "code"
      
      output$code_text_ui <- renderUI({
        textInput("code", "Entrez votre code :", value = "")
      })
      
      output$code_button_ui <- renderUI({
        actionButton("send_code", "Validez le code", icon = NULL, width = NULL)
      })
    } else {
      rv$ui = "image"
      output$cam_ui <- renderUI({
        shinyviewr_UI("my_camera", height = '1800px')
      })
      
      camera_snapshot <- callModule(
        shinyviewr,
        'my_camera',
        output_width = 400,
        output_height = 300
      )
    }
  })
  
  
  observe({
    if (rv$ui == "image"){
      # Breakdown code
      parsedCode <- reactive({
        req(camera_snapshot())
        
        png(filename="cam.png")
        plot(camera_snapshot(), main = 'My Photo!')
        dev.off()
        
        results_from_detection <- detection_of_codes("cam.png", show_image = TRUE)
        code <- coordinates_to_code(results_from_detection)
        print(code)
        unlist(strsplit(code, ":"))
        
      })
    } else if (rv$ui == "code") {
      
      # Breakdown code
      parsedCode <- reactive({
        unlist(strsplit(isolate(input$code), ":"))
      })
    }
  })
  
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
  
  
  
  
  observeEvent(input$send_code,{
    # Variable initialization
    # Breakdown code
    parsedCode <- reactive({
      unlist(strsplit(isolate(isolate(input$code)), ":"))
    })
    
    
    # Reactive containing a list for each tool.
    results <- reactive({
      code <- isolate(input$code)
      
      if (code != "" ){
        
        # Get the data
        
        #Data <- data.frame(getDataInitial(observatory = "Ois"))
        # TODO allow different possibilities
        
        #separate code to get clear instructions
        tools <- parsedCode()
        # code = "DOis:AEnv"
        #tools <- unlist(strsplit(code, ":"))
        
        # create a list with size = nomber of tools
        results <- vector(mode = "list", length = length(tools))
        
        #loop to execute all the steps
        for (i in 1:length(tools)){
          Parameters <- separateParametersTreatment(tools[i])
          toolname = substring(tools[i], 1, 1)
          # case tool = data
          if (toolname ==  "D"){
            results[[i]][[1]] <- data.frame(getDataInitial(observatory = Parameters[2]))
            results[[i]][[2]] <- results[[i]][[1]]
          } else if (toolname == "A") {
            results[[i]] <- abundanceCard(dataset = results[[i-1]][[1]], group_variable = correspond(Parameters[[2]], EquivalenceVar))
          } else if (toolname == "E") {
            results[[i]] <- diversityCard(dataset = results[[i-1]][[1]], group_variable = correspond(Parameters[[2]], EquivalenceVar))
          } else if (toolname == "N") {
            results[[i]] <- observationCard(dataset = results[[i-1]][[1]], group_variable = correspond(Parameters[[2]], EquivalenceVar))
          } else if (toolname == "T"){
            results[[i-1]] <- makeTop(results[[i-1]][[1]])
          } else if (toolname == "G"){
            results[[i]][[2]] <- makeGraphEasy(dataset = results[[i-1]][[1]])
          } else if (toolname == "C"){
            if ("Departement" %in% colnames(results[[i-1]][[1]])){
              departements_L93 <- mapToPlot()
              geoData = dplyr::left_join(departements_L93, results[[i-1]][[1]], by = 'Departement') %>%
                sf::st_transform(2154)
              results[[i]][[2]] <- tmap::tm_shape(geoData) +
                tmap::tm_borders() +
                tmap::tm_fill(col = colnames(results[[i-1]][[1]])[2])
            }
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
      # clean results for print functions
      for (i in 1:length(tools)){
        
        if ("IntervalleDeConfiance" %in% colnames(results[[i]])){
          results[[i]] <- results[[i]][ , -which(colnames(results[[i]]) == "IntervalleDeConfiance")]
        } 
        else
          # no display
          NULL
      }
    })
    
    output$Error <- renderText({
      # Découpage du code
      fullCode <- isolate(input$code)
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
                                                        results()[[toolPosition]], isolate(input$code))
      })
    )
    
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
    
    output$Error <- renderText({
      # Découpage du code
      fullCode <- isolate(input$code)
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
                                                        results()[[toolPosition]][[2]], isolate(input$code))
      })
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
