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
width <- NA
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
        label = "Focal length of lens in mm",
        value = 50,
        width = width
      ),
      numericInput(
        inputId = "focal_distance",
        label = "Focal distance in m",
        value = 100,
        width = width
      ),
      numericInput(
        inputId = "f_stop",
        label = "f stop ",
        value = 11,
        width = width
      ),
      numericInput(
        inputId = "Zone_of_Confusion",
        label = "Zone of confusion in Pixels ",
        value = 1,
        width = width
      ),
      textOutput("hyperfocal_dist"),
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
  zofc_pix <- reactive(
    input$Zone_of_Confusion
  )
  hyperfocal_dist <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = pixel()*zofc_pix() ,
      focal_dist = input$focal_distance
    )
    f$focusdata$H
  })
  focal_dist_min <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = pixel()*zofc_pix() ,
      focal_dist = input$focal_distance
    )
    f$focusdata$near
  })
  focal_dist_max <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = pixel()*zofc_pix() ,
      focal_dist = input$focal_distance
    )
    f$focusdata$far
  })
  output$Pixel_size <-
    renderText(paste0("Pixel Size = ", pixel()," microns"))
  output$balanced_f <-
    reactive(paste0("Balanced F stop = f_",
                    round(balancedf(pixel())),
                    " (focal ratio where lens resolution = sensor resolution)")
             )
  
  output$hyperfocal_dist = renderText(
    paste0("Hyper-focal Dist "
           , signif(hyperfocal_dist(),
                    digits = 3),
           " meters (minimum focal distance where infinity is within focus)")
    )
  # output$min_focus_dist = renderText(
  #   paste0("min focal dist ",
  #          signif(focal_dist_min()
  #                 , digits = 3),
  #          " meters"
  #          )
  #   )
  # output$max_focus_dist = renderText(
  #   paste0("max focal dist ",
  #          signif(focal_dist_max(),
  #                 digits = 3),
  #          " meters")
  #   )
  
  output$max_focus_dist = renderText(
    paste0("Depth of Field (meters) ",
           signif(focal_dist_min()
                  , digits = 3),
          " -> ",
           signif(focal_dist_max(),
                  digits = 3))
  )
}

# Run the application
shinyApp(ui = ui, server = server)
