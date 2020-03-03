#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(keras)
library(tensorflow)
library(tcltk)
library(kerasR)
library(EBImage)
library(shiny)
library(scales)



ui <- fluidPage(
    titlePanel("Facial Expression Test"),
    sidebarLayout(
        sidebarPanel(
            fileInput("myFile", label = "Choose a File",
                      accept = c("image/png", "image/jpeg"))
        ),
        
        #Show a plot and text of generated expression
        mainPanel(
            h4("Let's calculate image expression percentage for happy or sad."),
            imageOutput("img"),
            textOutput("percent"),
            textOutput("expression"),
            tags$head(tags$style(HTML("
                                      #percent {
                                      font-size: 20px;
                                      text-align:center;
                                      }
                                      
                                      #expression {
                                      font-size: 20px;
                                      text-align:center;
                                      }
                                      ")))
            )
        )
    
)


server <- function(input, output, session) {
    options(shiny.maxRequestSize = 50*1024*2)
    
    path <- "/Users/maryanmahamed/Downloads/face.h5"
    model <- load_model_tf(path, custom_objects = NULL, compile = TRUE)
    
    output$img <- renderImage({
        image_file <- input$myFile$datapath
        
        return(list(
            src = image_file,
            file_type = c("image/jpeg", "image/png"),
            height = 244,
            width = 244
        ))
    }, deleteFile = FALSE)
    
    output$percent <- renderText({
        req(input$myFile)
        img <- readImage(input$myFile$datapath)
        img <- resize(img, w = 244, h = 244)
        x <- image_to_array(img)
        x <- array_reshape(x, c(1, dim(x)))
        preds <- model %>% predict(x)
        if(preds[,1] > preds[,2]) {
            print(paste("With an expression percentage of ", percent(preds[,1]), "..."))
        } else {
            print(paste("With an expression percentage of ", percent(preds[,2]), "..."))
        }
    })
    
    

    output$expression <- renderText({
        req(input$myFile)
        img <- readImage(input$myFile$datapath)
        img <- resize(img, w = 244, h = 244)
        x <- image_to_array(img)
        x <- array_reshape(x, c(1, dim(x)))
        preds <- model %>% predict(x)
        b <- percent(preds)
        if(preds[,1] > preds[,2]){
            print("the image expression is more happy.")
        } else {
            print("the image expression is more sad")
        }
    })
    
}




# Run the application 
shinyApp(ui = ui, server = server)
