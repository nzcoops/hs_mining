#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Hades Star - Mining Calculator"),
  dashboardSidebar(
    menuItem(
      "Basic Calc",
      tabName = "userinputs",
      icon = icon("bar-chart-o")
    ),
    menuItem("Full Table",
             tabName = "full_table",
             icon = icon("th"))
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
    
      box(
        title = "Inputs",
        selectInput("select_speed", "Miner level:", c(1, 2, 3, 4, 5), selected = 5),
        selectInput("select_boost", "Boost level:", c(1:10), selected = 5),
        selectInput("select_remote", "Remote mining level:", c(1:10), selected = 5),
        textInput("asteroids", "Asteroids (hydro):", placeholder = "separate by commas e.g. '322,125,157,123'", value = "322,125,157,123"),
        textOutput("text_calc")
      ),
      infoBoxOutput('pinfoBox1', width = 8),
      tableOutput('table')
      
      )
      )
    )

server <- function(input, output) {
  
    mining_speed <<- c(6, 7.5, 12, 24, 60)/60
    mining_boost <<- c(2, 2.5, 3, 3.5, 4, 4.5, 5, 6, 7, 8)
    mining_remote <<- c(0.36, 0.4, 0.45, 0.51, 0.57, 0.64, 0.72, 0.81, 0.9, 1)
    
    vals <- reactiveValues()
    observe({
      #vals$s <- input$select_speed
      vals$s <- mining_speed[as.numeric(input$select_speed)]
      vals$b <- mining_boost[as.numeric(input$select_boost)]
      vals$r <- mining_remote[as.numeric(input$select_remote)]
      vals$mining_e_speed = vals$s * vals$b * vals$r
      
      vals$roids <- as.numeric(unlist(strsplit(input$asteroids, split=",")))
      
    })
    
    output$text_calc <- renderText({
      paste("The result is =", vals$roids  )
    })
    
    

  calcs <- function() {

    n_roids <- length(vals$roids)

    dat <- data.frame(matrix(ncol = length(vals$roids)))
    
    names(dat) <- paste0("roid", 1:length(vals$roids))
    dat[1,] <- vals$roids
    
    biggest_roid = max(vals$roids)
    max_time_seconds = biggest_roid / vals$mining_e_speed

    dat$time = ceiling(max_time_seconds)
    dat$time_up = 0
    dat$total_hydro = 0

    i = 1

    while(any(dat[nrow(dat), 1:n_roids] > 0)){
      dat[nrow(dat) + 1, 1:n_roids] = dat[nrow(dat), 1:n_roids] - vals$mining_e_speed
      dat$time_up[nrow(dat)] = i + 1
      dat$time[nrow(dat)] = dat$time[i] - 1
      dat$total_hydro[nrow(dat)] = sum(dat[1, 1:n_roids] - dat[nrow(dat), 1:n_roids])

      dat[nrow(dat), 1:n_roids][dat[nrow(dat), 1:n_roids] < 0] <- 0

      i = i +1
    }
    
    dat
    }
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$pinfoBox1 <- renderInfoBox({
    infoBox(
      "Hydro mined per second:",
      vals$mining_e_speed,
      icon = icon("credit-card"),
      width = 8
    )
  })
  
  
  output$table <- renderTable(calcs())

}

shinyApp(ui, server)
