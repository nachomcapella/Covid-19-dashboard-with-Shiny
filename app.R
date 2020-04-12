#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("plotly")
library('rsconnect')

source("util.R")

# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage(
  "COVID-19 Spain",
  tabPanel("Sick",
           sidebarLayout(
             sidebarPanel(
               h3("Choose a visualization"),
               checkboxInput(
                 "check1","Total cases (linear)", value= T),
               checkboxInput(
                 "check2","Total cases (log)", value= F),
               checkboxInput(
                 "check3","New cases (absolute)", value= F),
               checkboxInput(
                 "check4","New cases (%)", value= F),
                
               
               sliderInput(
                 "dates",
                 h3("Choose a date range"),
                 min = as.Date("2020-02-25", "%Y-%m-%d"),
                 max = as.Date("2020-04-12", "%Y-%m-%d"),
                 value = c(as.Date("2020-02-25"), as.Date("2020-04-12")),
                 timeFormat = "%Y-%m-%d"
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("plot_total_linear"),
               # plotlyOutput("plot_total_log"),
               # plotlyOutput("plot_new_cases_abs"),
               # plotlyOutput("plot_new_cases_perc")
               
             )
           )),
  tabPanel("Dead",
           h2("I am an empty panel!")),
  tabPanel("Regions",
           h2("I am an empty panel!"))
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  library("ggplot2")
  library("ggpubr")
  
  #Reading the data:
  data <- read.csv(file = "./data/nacional_covid19.csv")
  colnames(data)[1] <- "fecha"
  data$fecha <- as.Date(data$fecha)
  data$fallecimientos[is.na(data$fallecimientos)] <- 0
  
  
  
  #Contagiados:
  plot_total_linear_func <- function() {
    date_range <- c(input$dates[1], input$dates[2])
    dates <-
      data$fecha[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    cases <-
      data$casos[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    date <- as.character.Date(dates)
    df <- data.frame(cases, date)
    plot_total <-
      ggplot(data = df, aes(x = date, y = cases, group = 1)) + geom_line(color = "red") +
      geom_point() +
      ggtitle("Total cases vs date") +
      ylab("Cases (linear)") +
      xlab("Date") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(plot_total)
  }
  
  plot_total_log_func <- function() {
    date_range <- c(input$dates[1], input$dates[2])
    dates <-
      data$fecha[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    cases <-
      data$casos[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    date <- as.character.Date(dates)
    df <- data.frame(cases, date)
    plot_total <-
      ggplot(data = df, aes(x = date, y = cases, group = 1)) +
      geom_line(color = "red") +
      geom_point() +
      scale_y_continuous(trans = 'log10') +
      ggtitle("Total cases vs date") +
      ylab("Cases (log)") +
      xlab("Date") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(plot_total)
  }
  
  plot_new_cases_abs_func <- function() {
    date_range <- c(input$dates[1], input$dates[2])
    dates <-
      data$fecha[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    cases <-
      data$casos[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    cases <- get_daily_increment_absolute(cases)
    date <- as.character.Date(dates)
    df <- data.frame(cases, date)
    plot_new_cases_abs <-
      ggplot(data = df,
             aes(x = date, y = cases, group = 1)) +
      geom_line(color = "green") +
      geom_point() +
      ggtitle("New cases vs date") +
      ylab("New cases (absolute)") +
      xlab("Date") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(plot_new_cases_abs)
  }
  
  plot_new_cases_perc_func <- function() {
    date_range <- c(input$dates[1], input$dates[2])
    dates <-
      data$fecha[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    cases <-
      data$casos[data$fecha >= input$dates[1] &
                   data$fecha <= input$dates[2]]
    cases <- get_daily_increment_percentage(cases)
    date <- as.character.Date(dates)
    df <- data.frame(cases, date)
    plot_new_cases_perc <-
      ggplot(data = df,
             aes(x = date, y = cases, group = 1)) +
      geom_line(color = "blue") +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      ggtitle("New cases vs date") +
      ylab("New cases (+%)") +
      xlab("Date") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(plot_new_cases_perc)
  }
  
  # output$plot_total_linear <- renderPlotly({
  #   options <- input$checkGroup
  #   if ('1' %in% options) {
  #     plot_final <- (ggplotly(plot_total_linear_func()))
  #   } else{
  #     plot_final = NULL
  #   }
  #   return(plot_final)
  # })
  # output$plot_total_log <- renderPlotly({
  #   options <- input$checkGroup
  #   if ('2' %in% options) {
  #     plot_final <- (ggplotly(plot_total_log_func()))
  #   } else{
  #     plot_final = NULL
  #   }
  #   return(plot_final)
  # })
  # output$plot_new_cases_abs <- renderPlotly({
  #   options <- input$checkGroup
  #   if ('3' %in% options) {
  #     plot_final <- (ggplotly(plot_new_cases_abs_func()))
  #   } else{
  #     plot_final = NULL
  #   }
  #   return(plot_final)
  # })
  # output$plot_new_cases_perc <- renderPlotly({
  #   options <- input$checkGroup
  #   if ('4' %in% options) {
  #     plot_final <- (ggplotly(plot_new_cases_perc_func()))
  #   } else{
  #     plot_final = NULL
  #   }
  #   return(plot_final)
  # })
  
  
  pt1 <- reactive({
    if (!input$check1) return(NULL)
    ggplotly(plot_total_linear_func())
      })
  pt2 <- reactive({
    if (!input$check2) return(NULL)
    ggplotly(plot_total_log_func())
    })
  pt3 <- reactive({
    if (!input$check3) return(NULL)
    ggplotly(plot_new_cases_abs_func())
  })
  pt4 <- reactive({
    if (!input$check4) return(NULL)
    ggplotly(plot_new_cases_perc_func())
    })
  
  output$plot_total_linear = renderPlotly({
    ptlist <- list(pt1(),pt2(),pt3(),pt4())
    if (length(ptlist)==1){
      wtlist=c(100)
    } 
    if (length(ptlist)==2){
      wtlist=c(50,50)
    } 
    if (length(ptlist)==3){
      wtlist=c(33.33,33.33,33.33)
    }
    if (length(ptlist)==4){
      wtlist=c(25,25,25,25)
    } 
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)

    #ggplotly(grid.arrange(grobs=ptlist,heights=wtlist,nrow=length(ptlist)))
    return(subplot(ptlist, nrows=length(ptlist),shareX = T, shareY = F))
    })
  
}

# Run the application
shinyApp(ui = ui, server = server)
