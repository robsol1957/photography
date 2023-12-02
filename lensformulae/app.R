#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("photofuns.R")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Lens Calculations"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "camera",
        label = "Select Camera",
        #choices = c("D5", "D7100"),
        choices = camera_info$camera,
        selected = NULL,
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("Camera_name"),
      textOutput("Pixel_size"),
      textOutput("balanced_f"),
      numericInput(
        inputId = "focal_length",
        label = "Focal length in mm",
        value = 50,
        width = '70px'
      ),
      numericInput(
        inputId = "focal_distance",
        label = "Focal distance in m",
        value = 100,
        width = '70px'
      ),
      numericInput(
        inputId = "f_stop",
        label = "f stop ",
        value = 11,
        width = '70px'
      ),
      textOutput("hyperfocal_dist"),
      textOutput("min_focus_dist"),
      textOutput("max_focus_dist")
    )
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Camera_name <- renderText(paste(input$camera))
  pixel <- reactive({
    this_camera_info <- subset(camera_info, camera == input$camera)
    pixel_size <- this_camera_info$pixel_size
  })
  hyperfocal_dist <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = pixel() ,
      focal_dist = input$focal_distance
    )
    f$focusdata$H
  })
  focal_dist_min <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = pixel() ,
      focal_dist = input$focal_distance
    )
    f$focusdata$near
  })
  focal_dist_max <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = pixel() ,
      focal_dist = input$focal_distance
    )
    f$focusdata$far
  })
  output$Pixel_size <-
    renderText(paste0("pixel size = ", pixel()))
  output$balanced_f <-
    reactive(paste0("Balanced F stop = f_", round(balancedf(pixel(
      
    )))))
  
  output$hyperfocal_dist = renderText(paste0("hyper-focal dist ", signif(hyperfocal_dist(), digits =
                                                                         3)))
  output$min_focus_dist = renderText(paste0("min focal dist ", signif(focal_dist_min(), digits =
                                                                        3)))
  output$max_focus_dist = renderText(paste0("max focal dist ", signif(focal_dist_max(), digits =
                                                                        3)))
}

# Run the application
shinyApp(ui = ui, server = server)
