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
label <- print_sizes$Label[2]
camera_label <- camera_info$camera[1]
get_zoc <- function(label,camera_label){
  print <- subset(print_sizes, Label==label)[1,]
  camera <- subset(camera_info,camera==camera_label)[1,]
  magx <- print$x_mm/camera$x
  magy <- print$y_mm/camera$y
  mag <- max(magx,magy)
  signif(174.625/mag,digits = 3)
}
# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Lens Calculations"),
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
      ),
      radioButtons(
        inputId = "Print_Size",
        label = "Select Final Print Size",
        choices = print_sizes$Label,
        selected = NULL,
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      )
    ),
    mainPanel(
      textOutput("Camera_name"),
      textOutput("Pixel_size"),
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
      textOutput("Zone_of_Confusion"),
      textOutput("balanced_f"),
      textOutput("hyperfocal_dist"),
      textOutput("depth_of_field")
    )
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Camera_name <- renderText(paste(input$camera))
  pixel <- reactive({
    this_camera_info <- subset(camera_info, camera == input$camera)
    pixel_size <- this_camera_info$pixel_size
  })
  output$Pixel_size <-
    renderText(paste0("Pixel Size = ",
                      signif(pixel(),digits=3),
                      " microns"))
  ZofC = reactive(get_zoc(input$Print_Size,input$camera))
  output$Zone_of_Confusion <- renderText(paste0("Zone of Confusion ",ZofC()," micron"))
  output$balanced_f <-
    reactive(paste0("Balanced F stop = f_",
                    round(balancedf(pixel())),
                    " (focal ratio where lens resolution = sensor resolution)")
             )
  hyperfocal_dist <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      #ZofC =  pixel()*zofc_pix() ,
      ZofC = ZofC(),
      focal_dist = input$focal_distance
    )
    f$focusdata$H
  })
  output$hyperfocal_dist = renderText(
    paste0("Hyper-focal Dist ",
            signif(hyperfocal_dist(),digits = 3),
           " meters (minimum focal distance where infinity is within focus)")
  )
  focal_dist_min <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      #ZofC = pixel()*zofc_pix() ,
      ZofC = ZofC(),
      focal_dist = input$focal_distance
    )
    f$focusdata$near
  })
  focal_dist_max <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
     # ZofC = pixel()*zofc_pix() ,
      ZofC = ZofC(),
      focal_dist = input$focal_distance
    )
    f$focusdata$far
  })



  # # output$min_focus_dist = renderText(
  # #   paste0("min focal dist ",
  # #          signif(focal_dist_min()
  # #                 , digits = 3),
  # #          " meters"
  # #          )
  # #   )
  # # output$max_focus_dist = renderText(
  # #   paste0("max focal dist ",
  # #          signif(focal_dist_max(),
  # #                 digits = 3),
  # #          " meters")
  # #   )
  # 
  output$depth_of_field = renderText(
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
