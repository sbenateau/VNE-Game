#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("add", "Add UI"),
            actionButton("rmv", "rmv button"),
            textInput("txt", "This is no longer useful")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    observeEvent(input$add, {
        insertUI(
            selector = "#add",
            where = "afterEnd",
            ui = textInput("txt",
                           "Insert some text")
        )
    })
    
    
    observeEvent(input$rmv, {
        removeUI(
            selector = "div:has(> #txt)"
        )
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
