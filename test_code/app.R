#
#      Galaxy-Papers
#
#         Authors: Simon Benateau, Sebastien Turpin and Elie Arnaud
#
#         This application aims to give an introduction to data analysis
#         It has to be used with a card game to get the code that will lauch the following workflow:
#           - Import data
#           - Calculate indices
#           - make visualisation
#
#

library(shiny)
library(shinyBS) # for the collapse part 
library(shinysense) # for the capture of image
# load the functions to run the game
source('functionGame.R') # calculations
source('RenderCards.R') # module to add the values
source('functions_detection.R') # card detection

# Define UI for application that draws a histogram
ui <-   fluidPage(
    list(tags$head(HTML('<link rel="icon", href="favicon_papers.png", 
                                   type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
            title="", windowTitle="Galaxy-Papers"
        )
    ),
    navbarPage(div(img(src="logo_papers.png", height = 25),""), id = "galaxy_papers",
               tabPanel("Le projet", value = "proj",
                        tags$div(class="header", checked=NA,
                                 tags$img(src="logo_papers.png"),
                                 tags$h2("Mais qu’est-ce donc que Galaxy Papers ?"),
                                 tags$p("Le projet Galaxy Papers est né d’une demande des enseignants ! 
                                        Ils sont de plus en plus nombreux à nous demander la possibilité 
                                        de télécharger les données collectivement produites par le réseau Vigie-Nature École. 
                                        Galaxy Papers permet d’aller plus loin. Cet outil permet d'accéder à l'ensemble des données 
                                        produites par le réseau d'établissements qui contribue au programme Vigie-Nature École et de 
                                        les analyser avec des outils adaptés. L’objectif étant d’aider les élèves (et les enseignants) 
                                        à comprendre comment se déroule une analyse de données en écologie."),
                                 tags$h2("Commencer à utiliser Galaxy Papers")
                        ),
                        actionButton("new_code_proj", "Entrer un code manuellement", icon("text", lib = "glyphicon"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("new_reco_proj", "Utiliser la webcam ou l'appareil photo", icon("camera", lib = "glyphicon"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
               navbarMenu("Lancer une analyse de données",
                          tabPanel("Utiliser la webcam ou l'appareil photo", value = "reco",
                                   shinyviewr_UI("my_camera", height = '400px')
                          ),
                          tabPanel("Entrer un code manuellement", value = "code",
                                   textInput("code", "Entrez votre code :", value = ""),
                                   actionButton("validation_code", "Valider le code", 
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          )
               ),
               tabPanel("Résultats", value = "resu",
                        # shows the results
                        uiOutput("Cards"),
                        span(textOutput("error_message"), style = "color:red; font-weight: bold"),
                        tags$div(tags$br()),
                        actionButton("new_code_resu", "Entrer un nouveau code manuellement",  icon("text", lib = "glyphicon"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("new_reco_resu", "Utiliser la webcam ou l'appareil photo à nouveau", icon("camera", lib = "glyphicon"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               )
    )
    
)

# Define server logic required to run the application
server <- function(input, output, session) {
    
    # Import Data
    # initiate reactive values
    app_values = reactiveValues(code = NULL,
                                code_valid = FALSE,
                                depart_map = sf::st_read(dsn = "data", layer = "DEPARTEMENT",
                                                         quiet = TRUE) %>%
                                    dplyr::rename(Departement = CODE_DEPT) %>%
                                    sf::st_transform(2154),
                                region_map = sf::st_read(dsn = "data", layer = "REGION",
                                                         quiet = TRUE) %>%
                                    dplyr::rename(Region = NOM_REG) %>%
                                    sf::st_transform(2154)
    )
    
    
    rv = reactiveValues(ui = "NULL")
    
    data_values = reactiveValues(
        oiseaux = NULL,
        escargots = NULL,
        sauvages = NULL,
        vdt = NULL
    )
    
    observeEvent(data_values, {
        for (i in c("oiseaux", "escargots", "sauvages", "vdt")){
            URL_data_VNE <- RCurl::getURL(paste0("https://depot.vigienature-ecole.fr/datasets/test/papers/", i, ".csv"), .encoding = "UTF-8")
            
            data_values[[i]] <- read.csv(
                #paste0("../../../github/Requetes-et-restitutions/R-pour-restitutions/import_add_data/papers/",i,".csv")
                text = URL_data_VNE, encoding = 'UTF-8')

        }
    })
    
    # get image
    camera_snapshot <- callModule(
        shinyviewr,
        'my_camera',
        output_width = 1280/2,
        output_height = 720/2
    )
    
    # get code from image
    observeEvent(camera_snapshot(), {
        
        # print image from cam
        png(filename="cam.png", width = 1200/2, height = 720/2)
        par(mar = c(0,0,0,0))
        plot(camera_snapshot())
        dev.off()
        
        results_from_detection <- detection_of_codes("cam.png", show_image = TRUE)
        code <- coordinates_to_code(results_from_detection)
        
        app_values$code = code
        updateTabsetPanel(session, "galaxy_papers",
                          selected = "resu")
    })
    
    # get code from text input
    observeEvent(input$validation_code,{
        app_values$code = input$code
        updateTabsetPanel(session, "galaxy_papers",
                          selected = "resu")
    })
    
    # go back to code input
    observeEvent(input$new_code_proj | input$new_code_resu, {
        if (input$new_code_proj == 0 && input$new_code_resu == 0){
            return()
        }
        updateTabsetPanel(session, "galaxy_papers",
                          selected = "code")
    })
    
    # go back to image input
    observeEvent(input$new_reco_proj | input$new_reco_resu, {
        if (input$new_reco_proj == 0 && input$new_reco_resu == 0){
            return()
        }
        updateTabsetPanel(session, "galaxy_papers",
                          selected = "reco")
    })
    
    
    # check the code
    
    
    
    # run the app
    observeEvent(app_values$code, {
        
        # check if the code is ok
        app_values$code_valid <- FALSE # hypothesis code is false
        print(app_values$code)
        if (app_values$code != "") {
            print(app_values$code)
            app_values$parced_code <- unlist(strsplit(app_values$code, ":"))
        } else {
            print(app_values$code)
            app_values$parced_code <- NULL
        }
        
        if (!is.null(app_values$parced_code)){
            
            # first card must be data
            if (!grepl("D", app_values$parced_code[1])){
                app_values$error_message <- "Il faut obligatoirement poser une carte importer des données en première position"
                # 
            } else if (length(app_values$parced_code) > 3) {
                app_values$error_message <- "Attention plus de 3 cartes de traitements ont été rentrées, vérifiez votre analyse"
            } else if(length(app_values$parced_code) > 1) {
                if (grepl("D", app_values$parced_code[2])) {
                    app_values$error_message <- "il n'est possible d'importer des données qu'une seule fois"
                } else if (length(app_values$parced_code) == 2 & !substring(app_values$parced_code[3], 0, 1) %in% c("E", "V", "A", "N")){
                    app_values$error_message <- "Attention, il faut utiliser une carte manipuler les données en deuxième position"
                } else if (length(app_values$parced_code) == 3 & !app_values$parced_code[3] %in% c("T", "C", "G")){
                    app_values$error_message <- "Attention, il faut utiliser une carte visualiser les données en troisième position"
                } else {
                    app_values$code_valid <- TRUE
                }
            } else {
                app_values$code_valid <- TRUE
            }
        }
        
        if (app_values$code_valid){
            
            tools <- app_values$parced_code
            # code = "DOis:ADep:C"
            # code = "DOis:AMoi:G"
            
            #tools <- unlist(strsplit(code, ":"))
            
            # create a list with size = nomber of tools
            results <- vector(mode = "list", length = length(tools))
            
            #loop to execute all the steps
            for (i in 1:length(tools)){
                Parameters <- separateParametersTreatment(tools[i])
                toolname = substring(tools[i], 1, 1)
                # case tool = data
                if (toolname ==  "D"){
                    results[[i]][[1]] <- data.frame(getDataInitial(data_values, observatory = Parameters[2]))
                    results[[i]][[2]] <- results[[i]][[1]]
                } else if (toolname == "A") {
                    results[[i]] <- abundanceCard(dataset = results[[i-1]][[1]], group_variable = correspond(Parameters[[2]], EquivalenceVar))
                } else if (toolname == "E") {
                    results[[i]] <- diversityCard(dataset = results[[i-1]][[1]], group_variable = correspond(Parameters[[2]], EquivalenceVar))
                } else if (toolname == "V") {
                    results[[i]] <- total_species_card(dataset = results[[i-1]][[1]])
                } else if (toolname == "N") {
                    results[[i]] <- observationCard(dataset = results[[i-1]][[1]], group_variable = correspond(Parameters[[2]], EquivalenceVar))
                } else if (toolname == "T"){
                    results[[i]][[2]] <- makeTop(results[[i-1]][[1]])
                } else if (toolname == "G"){
                    results[[i]][[2]] <- makeGraphEasy(dataset = results[[i-1]][[1]])
                } else if (toolname == "C"){
                    results[[i]][[2]] <- makeMapEasy(results[[i-1]][[1]], app_values$depart_map, app_values$region_map)
                } else {
                    msg <- paste0("Tool", tools[i], "seems to be mis-formated (code:", paste0(tools, collapse = ":"), ")\n")
                    warning(msg)
                }
                # TODO in case of typo, any default value?
            }
            
            app_values$results <- results
        }
        
        
        # Procedurally generate UI by calling multiple times the renderCards module
        output$Cards <- renderUI({
            print(app_values$parced_code)
            print(app_values$code_valid)
            # Check for input's content
            if(length(app_values$parced_code) != 0 & app_values$code_valid){
                do.call(bsCollapse,
                        lapply(seq_along(app_values$parced_code), function (toolPosition){
                            renderCardsUI(toolPosition, app_values$parced_code)
                        })
                )
            }
            else
                # no display
                NULL
        })
        
        # Procedurally generate server by calling multiple times renderCards module's server
        observe(
            sapply(seq_along(app_values$parced_code), function (toolPosition){
                rv[[ as.character(toolPosition)]] <- callModule(renderCards, toolPosition,
                                                                toolPosition, # repeated to be given as additional argument
                                                                app_values$parced_code,
                                                                app_values$results[[toolPosition]][[2]], isolate(app_values$code))
            })
        )

        output$error_message <- renderText({
            if(!app_values$code_valid){
                app_values$error_message
            } else {
                NULL
            }
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
