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
# load the functions to run the game
source('functionGame.R')
source('RenderCards.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Jeu sur les données"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      textInput("code", "Entrez votre code :", value = ""),
      submitButton(text = "Validez le code", icon = NULL, width = NULL)
    ),
    
    # Show the outputs from the treatment
    mainPanel(
      uiOutput("Cards")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Variable initialization
  rv <- reactiveValues()
  
  #Import data
  Data <- reactive({
    data.frame(getDataInitial())
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
      Data <- Data()
      # TODO allow different possibilities
      
      #separate code to get clear instructions
      tools <- parsedCode()
      
      # create a list with size = nomber of tools
      Results <- vector(mode = "list", length = length(tools))
      
      #loop to execute all the steps
      for (i in 1:length(tools)){
        # case tool = data
        if (tools[i] == "D"){
          Results[[i]] <- Data
        } else if (tools[i] == "M") {
          Results[[i]] = randomAll(Results[[i-1]])
        } else if (substring(tools[i], 1, 1) == "R"){
          Parameters <- separateParametersTreatment(tools[i])
          Results[[i]] <- Results[[i-1]] %>%
            group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
            summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = correspond(Parameters[[3]], EquivalenceFun))
        } else if (substring(tools[i], 1, 1) == "T"){
          Parameters <- separateParametersTreatment(tools[i])
          Results[[i]] <- Results[[i-1]] %>%
            arrange(Parameters[[1]])
        } else if (substring(tools[i], 1, 1) == "G"){
          # get parameters (improve by locating the graph within the code)
          Parameters <- separateParametersTreatment(tools[length(tools)])
          # get the name of the column to check few thing
          colNamesData <- colnames(Results[[length(Results)-1]])
          # Add errors if the columns are not in the code
          # if sp is in the dataset, separate by  species (if species as columns then change)
          if ("Espece" %in% colNamesData & Parameters[[1]] != "Esp" & Parameters[[1]] != "Esp") facet = facet_wrap(.~Espece) else facet = NULL
          # if data not summarised plot points else plot barplot
          if (nrow(Results[[length(Results)-1]]) < 30) representation <- geom_col(aes_string(fill = correspond(Parameters[[1]], EquivalenceVar))) else representation <- geom_jitter(aes_string(col = correspond(Parameters[[1]], EquivalenceVar)))
          # graph is too specific right now
          Results[[i]] <- ggplot(Results[[length(Results)-1]], aes_string(x = correspond(Parameters[[1]], EquivalenceVar), y = correspond(Parameters[[2]], EquivalenceVar)), environment = environment()) +
            representation +
            facet
        }
        else{
          msg <- paste0("Tool", tools[i], "seems to be mis-formated (code:", paste0(tools, collapse = ":"), ")\n")
          warning(msg)
        }
        # TODO in case of typo, any default value?
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
      
      # Procedurally generate server by calling multiple times renderCards module's server
      observe(
        sapply(seq_along(parsedCode()), function (toolPosition){
          rv[[ as.character(toolPosition)]] <- callModule(renderCards, toolPosition,
                                                          toolPosition, # repeated to be given as additional argument
                                                          parsedCode(), 
                                                          Results()[[toolPosition]], input$code)
        })
      )
      
      # Old 
      {
        # output$Table <- renderTable({
        #   code <- input$code
        #   if (code != "" ){
        #     Results <- Results()
        #     head(Results[[input$viewResult]], 20)
        #   }
        # })
        # 
        # output$Plot <- renderPlot({
        #   code <- input$code
        #   if (str_detect(code, "G")){
        #     # load results
        #     Results <- Results()
        #     # load information about the tools
        #     tools <- Tools()
        #     # get parameters (improve by locating the graph within the code)
        #     Parameters <- separateParametersTreatment(tools[length(tools)])
        #     # get the name of the column to check few thing
        #     colNamesData <- colnames(Results[[length(Results)-1]])
        #     # Add errors if the columns are not in the code
        #     # if sp is in the dataset, separate by  species (if species as columns then change)
        #     if ("Espece" %in% colNamesData & Parameters[[1]] != "Esp" & Parameters[[1]] != "Esp") facet = facet_wrap(.~Espece) else facet = NULL
        #     # if data not summarised plot points else plot barplot
        #     if (nrow(Results[[length(Results)-1]]) < 30) representation <- geom_col(aes_string(fill = correspond(Parameters[[1]], EquivalenceVar))) else representation <- geom_jitter(aes_string(col = correspond(Parameters[[1]], EquivalenceVar)))
        #     # graph is too specific right now
        #     YourPlot <- ggplot(Results[[length(Results)-1]], aes_string(x = correspond(Parameters[[1]], EquivalenceVar), y = correspond(Parameters[[2]], EquivalenceVar)), environment = environment()) +
        #       representation +
        #       facet
        #     YourPlot
        #   } else {
        #     plot(1,1,type = 'n',ann = FALSE, axes = FALSE)
        #     text(1,1,"Votre chaine d'analyse ne contient pas de représentations graphique")
        #   }
        # })
        
        
        
        # output$video <- renderUI({
        #   if (input$code == "D") {
        #     link = "YBEgmD8ik68"
        #   } else if (input$code == "DG"){
        #     link = "VULkZb6zUNs"
        #   }
        #   HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/',link,'" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
        # })
        
      }
      
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
  