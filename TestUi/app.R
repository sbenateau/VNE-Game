library(shiny)
library(shinysense)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # choix du type d'interface
            selectInput("choice", "Type d'interface :", c("image","code"), selected = "image"),
            actionButton("choose_ui","Valider le choix d'interface"),
            HTML("<br><br>"),
            # interface pour code
            uiOutput("code_text_ui"),
            uiOutput("code_button_ui"),
            # interface pour reconnaissance d'image
            uiOutput("cam_ui")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            imageOutput("snapshot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # add reactive values to store information between chunks
    rv <- reactiveValues(
        ui = "NULL"
    )
    
    # build conditionnaly UI
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
            
            camera_snapshot <- reactive({
                callModule(
                    shinyviewr,
                    'my_camera',
                    output_width = 400,
                    output_height = 300
                )
            })
            
            output$snapshot <- renderPlot({
                req(camera_snapshot())
                plot(camera_snapshot(), main = 'My Photo!')
            })
            
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

            output$codeImg <- renderText({
                parsedCode()
            })
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
