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
library(lubridate)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[, nums] <- round(df[, nums], digits = digits)
  
  (df)
}

ui <- dashboardPage(
  dashboardHeader(title = "Hades Star - Mining Calculator"),
  dashboardSidebar(sidebarMenu(
    tags$img(src ="https://pbs.twimg.com/profile_images/1035337796188172288/cbHBZTxQ_400x400.jpg", width = 225),
    
    menuItem(
      "Basic Calc",
      tabName = "userinputs",
      icon = icon("bar-chart-o")
    ),
    menuItem("Full Table",
             tabName = "full_table",
             icon = icon("th")),
    
    menuItem(
      "SSSputnik",
      tabName = "ssput",
      icon = icon("exclamation-triangle")
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "userinputs",
      fluidRow(
        box(
          title = "Inputs",
          selectInput("select_speed", "Miner level:", c(1, 2, 3, 4, 5), selected = 5),
          selectInput("select_boost", "Boost level:", c(1:10), selected = 5),
          selectInput("select_remote", "Remote mining level:", c(1:10), selected = 5),
          selectInput("select_crunch", "Crunch level:", c(0:10), selected = 0),
          textInput(
            "asteroids",
            "Asteroids (hydro):",
            placeholder = "separate by commas e.g. '322,125,157,123'",
            value = "322,125,157,123"
          ),
          checkboxInput("ws_dich", "Is this for a white star?", F)
        ),
        infoBoxOutput('pinfoBox1', width = 6),
        # infoBoxOutput('pinfoBox2', width = 6),
        # infoBoxOutput('pinfoBox2b', width = 6),
        # infoBoxOutput('pinfoBox2c', width = 6),
        infoBoxOutput('pinfoBox2d', width = 6),
        infoBoxOutput('pinfoBox3', width = 6),
        infoBoxOutput('pinfoBox4', width = 6)
      )
    ),
    
    tabItem(tabName = "full_table",
            tableOutput('table')),
    
    tabItem(
      tabName = "ssput",
      tags$img(src = "https://i.ibb.co/Vgk92VD/IMG-20.jpg")
      #imageOutput(output$image_sput)
      
      
    )
  ))
)


server <- function(input, output) {
  mining_speed <<- c(6, 7.5, 12, 24, 60) / 60
  mining_boost <<- c(2, 2.5, 3, 3.5, 4, 4.5, 5, 6, 7, 8)
  mining_remote <<- c(0.36, 0.4, 0.45, 0.51, 0.57, 0.64, 0.72, 0.81, 0.9, 1)
  mining_unity <<- c(1.25, 1.29, 1.34, 1.39, 1.45, 1.52, 1.61, 1.71, 1.83, 2)
  mining_crunch <<- c(300, 350, 400, 450, 500, 600, 700, 800, 900, 1000)
  
  vals <- reactiveValues()
  observe({
    vals$s <- mining_speed[as.numeric(input$select_speed)]
    vals$b <- mining_boost[as.numeric(input$select_boost)]
    vals$r <- mining_remote[as.numeric(input$select_remote)]
    vals$c <- ifelse(input$select_crunch == 0,
                     0,
                     mining_crunch[as.numeric(input$select_crunch)])
    #vals$u <- c(1.25,1.29,1.34,1.39,1.45,1.52,1.61,1.71,1.83,2)
    vals$mining_e_speed = vals$s * vals$b * vals$r
    vals$roids <- as.numeric(unlist(strsplit(input$asteroids, split = ",")))
    vals$n_roids <- length(vals$roids)
  })
  
  calcs <- function() {
    
    n_roids <<- length(vals$roids)
    dat <<- data.frame(matrix(ncol = length(vals$roids)))
    
    names(dat) <- paste0("roid", 1:length(vals$roids))
    dat[1, ] <- vals$roids
    
    biggest_roid = max(vals$roids)
    max_time_seconds = biggest_roid / vals$mining_e_speed
    
    dat$time = ceiling(max_time_seconds)
    dat$time_up = 0
    dat$total_hydro = 0
    dat$hydro_remaining = sum(vals$roids)
    
    i = 1
    
    while (any(dat[nrow(dat), 1:n_roids] > 0)) {
      dat[nrow(dat) + 1, 1:n_roids] = dat[nrow(dat), 1:n_roids] - vals$mining_e_speed
      dat[nrow(dat), 1:n_roids][dat[nrow(dat), 1:n_roids] < 0] <- 0
      dat$time_up[nrow(dat)] = i
      dat$time[nrow(dat)] = dat$time[i] - 1
      dat$total_hydro[nrow(dat)] = sum(dat[1, 1:n_roids]) - sum(dat[nrow(dat), 1:n_roids])
      dat$hydro_remaining[nrow(dat)] = sum(dat[nrow(dat), 1:n_roids])
      i = i + 1
    }
    
    dat <- round_df(dat, 2)
    
    if(input$select_crunch > 0){
      end = which.max(dat$hydro_remaining < vals$c) 
      dat <- dat[-c((end+1):nrow(dat)), ]
    } else {
    }
    
    if (input$ws_dich == F) {
      dat
    } else{
      dat$time_up[nrow(dat)] <- dat$time_up[nrow(dat)]*600
      dat
    }
  }
  
  output$pinfoBox1 <- renderInfoBox({
    infoBox("Hydro mined per second:",
            vals$mining_e_speed,
            icon = icon("gas-pump"),)
  })
  
  # output$pinfoBox2 <- renderInfoBox({
  #   infoBox("Time (seconds) to empty sector:",
  #           calcs()$time_up[nrow(calcs())],
  #           icon = icon("clock"),)
  # })
  # 
  # output$pinfoBox2b <- renderInfoBox({
  #   infoBox("Time (minutes) to empty sector:",
  #           round(calcs()$time_up[nrow(calcs())] / 60, 2),
  #           icon = icon("clock"),)
  # })
  # 
  # output$pinfoBox2c <- renderInfoBox({
  #   infoBox("Time (hours) to empty sector:",
  #           round(calcs()$time_up[nrow(calcs())] / 60 / 60, 2),
  #           icon = icon("clock"),)
  # })
  # 
  output$pinfoBox2d <- renderInfoBox({
    if(input$select_crunch >0) {
      infoBox("Hit crunch after:",
              seconds_to_period(calcs()$time_up[nrow(calcs())]),
              icon = icon("cookie-bite"),
              color = "orange")
    } else {
      infoBox("Time to empty sector:",
              seconds_to_period(calcs()$time_up[nrow(calcs())]),
              icon = icon("clock"))
    }
  })
  
  output$pinfoBox3 <- renderInfoBox({
    infoBox("Total Hydro:",
            sum(vals$roids),
            icon = icon("fill-drip"))
  })
  
  output$pinfoBox4 <- renderInfoBox({
    infoBox("Number of asteroids:",
            vals$n_roids,
            icon = icon("globe"))
  })
  
  output$table <- renderTable(if (nrow(calcs()) > 100) {
    rbind(head(calcs()),
          "truncated",
          tail(calcs()))
    
  } else {
    calcs()
  })
  
}

shinyApp(ui, server)
