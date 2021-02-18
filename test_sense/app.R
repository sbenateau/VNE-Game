# to install the opencv virtualenv 

library(shiny)
library(shinysense)
reticulate::use_virtualenv("../opencv/", required = TRUE)
library(reticulate)

ui <- fluidPage(
    shinyviewr_UI("myCamera")
)

server <- function(input, output) {
    #server side call of the drawr module
    myCamera <- callModule(
        shinyviewr, "myCamera",
        output_height = 300,
        output_width = 300)
    
    observeEvent(myCamera(), {
        
        # get image
        matrix_res <<- myCamera()
        #send_to_python <-matrix(NA, 400,300)
        
        print(dim(matrix_res))
        test_matrix <- matrix(matrix_res, 300, 300)
        png("photo.png", 300,300)
        par(mar = c(0,0,0,0))
        plot(matrix_res)
        dev.off()
        
        # send image to python
        py$image <- test_matrix
        # write image with R
        py_run_file("test_sense/load_image.py") 
        print(py$corners)
    })
}

shinyApp(ui = ui, server = server)
