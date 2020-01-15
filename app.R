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
      tableOutput("Debug")
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
  
  #Import map
  mapToPlot <- reactive({
    extraWD = "data"
    if (!file.exists(file.path(extraWD, "departement.zip"))) {
      githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-07-14-introduction-to-mapping-with-sf-and-co/data/departement.zip"
      download.file(githubURL, file.path(extraWD, "departement.zip"))
      unzip(file.path(extraWD, "departement.zip"), exdir = extraWD)
    }
    departements_L93 <- st_read(dsn = extraWD, layer = "DEPARTEMENT",
                                quiet = TRUE) %>% 
      rename(Departement = CODE_DEPT) %>%
      st_transform(2154)
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
      Data <- Data()
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
          Results[[i]] <- Data
        } else if (tools[i] == "M") {
          Results[[i]] = randomAll(Results[[i-1]])
        } else if (substring(tools[i], 1, 1) == "R"){
          Parameters <- separateParametersTreatment(tools[i])
          if (Parameters[[3]] == "Mo"){
            Results[[i]] <- Results[[i-1]] %>%
              group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
              summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = c("mean","se")) %>%
              rename(Nombre_individus = mean)
          } else {
            Results[[i]] <- Results[[i-1]] %>%
              group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
              summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = correspond(Parameters[[3]], EquivalenceFun))
          }
        } else if (substring(tools[i], 1, 1) == "T"){
          Parameters <- separateParametersTreatment(tools[i])
          Results[[i]] <- Results[[i-1]] %>%
            arrange(desc(!!sym(correspond(Parameters[[2]], EquivalenceVar))))
        } else if (substring(tools[i], 1, 1) == "G"){
          # get parameters (improve by locating the graph within the code)
          Parameters <- separateParametersTreatment(tools[i])
          # get the name of the column to check few thing
          colNamesData <- colnames(Results[[i-1]])
          # Add errors if the columns are not in the code
          # if sp is in the dataset, separate by  species (if species as columns then change)
          if ("Espece" %in% colNamesData & Parameters[[1]] != "Esp" & Parameters[[1]] != "Esp") facet = facet_wrap(.~Espece) else facet = NULL
          # if data not summarised plot points else plot barplot
          if (nrow(Results[[i-1]]) < 30) representation <- geom_col(aes_string(fill = correspond(Parameters[[1]], EquivalenceVar))) else representation <- geom_jitter(aes_string(col = correspond(Parameters[[1]], EquivalenceVar)))
          # graph is too specific right now
          Results[[i]] <- ggplot(Results[[i-1]], aes_string(x = correspond(Parameters[[1]], EquivalenceVar), y = correspond(Parameters[[2]], EquivalenceVar)), environment = environment()) +
            representation +
            facet +
            theme_minimal()+
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=16),
                  strip.text.x = element_text(size = 14))
        } else if (substring(tools[i], 1, 1) == "B"){
          Parameters <- separateParametersTreatment(tools[i-1])
          Results[[i-1]]$data <- Results[[i-1]]$data %>%
            mutate(ymin = Nombre_individus + se,
                   ymax = Nombre_individus - se)
          Results[[i]] <- Results[[i-1]] + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2)
        } else if (substring(tools[i], 1, 1) == "P"){
          Parameters <- separateParametersTreatment(tools[i])
          departements_L93 <- mapToPlot()
          geoData = left_join(departements_L93, Results[[i-1]], by = 'Departement') %>%
            st_transform(2154)
          
          Results[[i]] <- tm_shape(geoData) +
            tm_borders() +
            tm_fill(col = correspond(Parameters[[2]], EquivalenceVar))
          
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
  output$Debug <- renderTable({
    Debug <- vector(mode = "list", length = 1)
    # Découpage du code
    # noms des outils
    AllTools <- unlist(strsplit(input$code, ":"))
    AllToolsNames <- unlist(lapply(AllTools, function(x) str_sub(x,1,1)))
    toolUsed <- AllToolsNames[!sapply(AllToolsNames, function (x) x == "S")]
    # variables utilisées
    varUsed <-unlist(str_extract_all(AllTools, "[A-Z][a-z][a-z]"))
    # fonctions utilisées
    funUsed <-unlist(str_extract_all(AllTools, "[A-Z][a-z][A-Z]"))
    funUsed <- str_sub(funUsed, start = 1, end = 2)
    # function utilisées
    Debug[[1]] <- c(!"D" %in% toolUsed, '!"D" %in% toolUsed', 'Pas de données')
    Debug[[2]] <- c(length(toolUsed) == 1, 'length(toolUsed) == 1', 'Seulement les données')
    Debug[[3]] <- c("M" %in% toolUsed, '"M" %in% toolUsed', 'utilisation de random all (non souhaité)')
    Debug[[4]] <- c("P" %in% toolUsed, '"P" %in% toolUsed', 'Utilisation dune carte (non souhaité)')
    Debug[[5]] <- c(!"Env" %in% varUsed, '!"Env" %in% varUsed', 'anque Environnement dans les variables')
    Debug[[6]] <- c(!"R" %in% toolUsed, '!"R" %in% toolUsed', 'Pas de regroupement')
    Debug[[7]] <- c("So" %in% funUsed & !"Mo" %in% funUsed, '"So" %in% funUsed & !"Mo" %in% funUsed', 'Pas de données')
    Debug[[8]] <- c("Co" %in% funUsed, '"Co" %in% funUsed', 'Utilisation dun comptage (non souhaité)')
    Debug[[9]] <- c(!"Pla" %in% varUsed, '!"Pla" %in% varUsed', 'Manque placette dans les variables')
    Debug[[10]] <- c(!"Num" %in% varUsed, '!"Num" %in% varUsed', 'Manque numéro dobservation  dans les variables')
    Debug[[11]] <- c(!"G" %in% toolUsed, '!"G" %in% toolUsed', 'Pas de graphique')
    Debug[[12]] <- c(!"B" %in% toolUsed, '!"B" %in% toolUsed', 'Pas de barres derreur')
    matrix(unlist(Debug), ncol = 3, byrow = TRUE)
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

