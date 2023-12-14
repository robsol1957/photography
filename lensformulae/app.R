#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#dir <- "lensformulae//"
# dir <- ""
# source(paste0(dir,"photofuns.R"))
source("photofuns.R")
library(shiny)
width <- NA
label <- print_sizes$Label[2]
camera_label <- camera_info$camera[1]





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
        selected = "A4 210 x 297 mm",
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      )
    ),
    mainPanel(
      textOutput("Camera_name"),
      textOutput("Pixel_size"),
      textOutput("balanced_f"),
      textOutput("p_header"),
      textOutput("max_mag"),
      textOutput("p_in_mm"),
      textOutput("p_in_inch"),
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
      textOutput("magnification"),
      textOutput("Zone_of_Confusion"),
      textOutput("minf"),
      textOutput("hyperfocal_dist"),
      textOutput("depth_of_field"),
      textOutput("pixelation"),
      textOutput("dpi")
    )
    
  ))
get_mag <- function(print_label,camera_label){
  print <- subset(print_sizes, Label==print_label)[1,]
  camera <- subset(camera_info,camera==camera_label)[1,]
  magx <- print$x_mm/camera$x
  magy <- print$y_mm/camera$y
  mag <- max(magx,magy)
}
get_zoc <- function(print_label,camera_label){
  signif(174.625/get_mag(print_label,camera_label),digits = 3)
}

get_pixel_size <- function(camera_label){
  this_camera_info <- subset(camera_info, camera == camera_label)[1,]
  pixel_size <- signif(this_camera_info$pixel_size,digits=3)
}

get_max_print_size <- function(camera_label){
  this_camera_info <- subset(camera_info, camera == camera_label)[1,]
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  sunits="mm"
  r_get_pixel_size <- reactive(get_pixel_size(input$camera))
  r_magnification <- reactive(get_mag(input$Print_Size,input$camera))
  ZofC = reactive(get_zoc(input$Print_Size,input$camera))
  r_small_f <- reactive(signif(balancedf(ZofC()),digits = 3))
  r_get_max_print_size <- reactive(get_max_print_size(input$camera))
  
  output$Camera_name <- renderText(paste(input$camera))
  output$Pixel_size <-renderText(
    paste0("Pixel Size = ",
           r_get_pixel_size(),
           " microns")
    )
  output$balanced_f <-
    reactive(paste0("-Balanced F stop = f_",
                    round(balancedf(r_get_pixel_size())),
                    " (focal ratio where lens resolution = sensor resolution)")
    )
  output$p_header <- renderText("Maximum Print size to manage pixalation")
  output$max_mag <- renderText(paste0("- max Magnification ",signif(r_get_max_print_size()$Max_mag,3)))
  output$p_in_mm <- renderText(r_get_max_print_size()$MPmm)
  output$p_in_inch <- renderText(r_get_max_print_size()$MPinch)
  output$magnification <- renderText(
    paste0("-Viewing Magnification ",
           signif(r_magnification(),
                  digits = 3)," times"
           )
    )
  output$Zone_of_Confusion <- renderText(
    paste0("-Recommended Zone of Confusion for this magnification",
           ZofC(),
           " micron")
    )
  output$minf <- renderText(
    paste0("-Min focal ratio for zoc f ",r_small_f())
  )
  
  hyperfocal_dist <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = ZofC(),
      focal_dist = input$focal_distance
    )
    f$focusdata$H
  })
  output$hyperfocal_dist = renderText(
    paste0("-Hyper-focal Dist ",
            signif(hyperfocal_dist(),digits = 3),
           " meters (minimum focal distance where infinity is within focus)")
  )
  focal_dist_min <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = ZofC(),
      focal_dist = input$focal_distance
    )
    f$focusdata$near
  })
  focal_dist_max <- reactive({
    f = depth_of_field(
      focal_length = input$focal_length,
      f = input$f_stop,
      ZofC = ZofC(),
      focal_dist = input$focal_distance
    )
    f$focusdata$far
  })

  output$depth_of_field = renderText(
    paste0("-Depth of Field (meters) ",
           signif(focal_dist_min()
                  , digits = 3),
          " -> ",
           signif(focal_dist_max(),
                  digits = 3))
  )
  output$pixelation <- renderText(
    paste0("-Pixel size on print ",signif(r_get_pixel_size()*r_magnification()/1000,digits=3))
  )
  output$dpi <- renderText(
    paste0("-Pixels/inch (dpi) ",signif(25.4/(r_get_pixel_size()*r_magnification()/1000),digits=3))
  )
}

# Run the application
shinyApp(ui = ui, server = server)
