######################################################
#      VNE Game
#
#         Authors: Simon Benateau, Sebastien Turpin and Elie Arnaud
#
#         This application aims to give an introduction to data analysis
#         It has to be use with a card game to get the code that will lauch the following workflow:
#           - Import data
#           - Calculate indices
#           - make visualisation
#
#####################################################

# import all packages
library(shiny)
library(memisc) # to view iframe
library(shinyBS) # for the collapse part 
library(shinysense) # for the cature of image
# load the functions to run the game
source('functionGame.R') # calculations
source('RenderCards.R') # module to add the values
source('functions_detection.R') # card detection

# Define UI for application
ui <- fluidPage(
  
  # Sidebar with inputs
  # Logo de l'application
  div(img(src = "logo_papers.png", width = '50%'), style="text-align: center;"),
  HTML("<br><br>"),
  # ui for the 
  shinyviewr_UI("my_camera", height = '720px'),
  
  # Show the outputs from the treatment
  mainPanel(
    # shows the results
    uiOutput("Cards")
    
  )
)

# Define server logic
server <- function(input, output) {
  
  rv <- reactiveValues(parsedCode = "")
  
  #Import map
  depart_map <- reactive({
    departements_L93 <- sf::st_read(dsn = "data", layer = "DEPARTEMENT",
                                    quiet = TRUE) %>%
      dplyr::rename(Departement = CODE_DEPT) %>%
      sf::st_transform(2154)
    departements_L93
  })
  
  camera_snapshot <- callModule(
    shinyviewr,
    'my_camera',
    output_width = 1280,
    output_height = 720
  )
  
  observeEvent(camera_snapshot(), {
    photo <- camera_snapshot()
    plot(photo, main = 'My Photo!')
    png(filename="cam.png", width = dim(photo)[2], height = dim(photo)[1])
    # may improve quality of png with dim(photo)
    plot(photo, main = 'My Photo!')
    dev.off()
    
    results_from_detection <- detection_of_codes("cam.png", show_image = TRUE)
    print(results_from_detection)
    code <- coordinates_to_code(results_from_detection)
    print(code)
    code = "DOis"
    rv$code = code
    rv$parsedCode <- unlist(strsplit(code, ":"))
    
    # Reactive containing a list for each tool.
    results <- reactive({
      code <- isolate(rv$code)
      if (code != "" ){
        
        # Get the data
        
        #Data <- data.frame(getDataInitial(observatory = "Ois"))
        # TODO allow different possibilities
        
        #separate code to get clear instructions
        tools <- rv$parsedCode
        # code = "DOis:ADep:C"
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
            results[[i]][[2]] <- makeMapEasy(results[[i-1]][[1]])
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
      if(length(rv$parsedCode) != 0){
        do.call(bsCollapse,
                lapply(seq_along(rv$parsedCode), function (toolPosition){
                  renderCardsUI(toolPosition, rv$parsedCode)
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
      fullCode <- isolate(rv$code)
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
      sapply(seq_along(rv$parsedCode), function (toolPosition){
        rv[[ as.character(toolPosition)]] <- callModule(renderCards, toolPosition,
                                                        toolPosition, # repeated to be given as additional argument
                                                        rv$parsedCode,
                                                        results()[[toolPosition]], isolate(rv$code))
      })
    )
    
    # Procedurally generate UI by calling multiple times the renderCards module
    output$Cards <- renderUI({
      # Check for input's content
      if(length(rv$parsedCode) != 0){
        do.call(bsCollapse,
                lapply(seq_along(rv$parsedCode), function (toolPosition){
                  renderCardsUI(toolPosition, rv$parsedCode)
                })
        )
      }
      else
        # no display
        NULL
    })
    
    output$Error <- renderText({
      # Découpage du code
      fullCode <- isolate(rv$code)
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
      sapply(seq_along(rv$parsedCode), function (toolPosition){
        rv[[ as.character(toolPosition)]] <- callModule(renderCards, toolPosition,
                                                        toolPosition, # repeated to be given as additional argument
                                                        rv$parsedCode,
                                                        results()[[toolPosition]][[2]], isolate(rv$code))
      })
    )
    
    
    
  })
}
  # Run the application
  shinyApp(ui = ui, server = server)