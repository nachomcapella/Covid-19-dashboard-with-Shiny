#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#APP V2

library(shiny)
library("plotly")
library('rsconnect')

source("util.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   sidebarPanel(
     h3("Choose a visualization"),
     checkboxInput(
       "check_sick_1","Total cases (linear)", value= T),
     checkboxInput(
       "check_sick_2","Total cases (log)", value= F),
     checkboxInput(
       "check_sick_3","New cases", value= F),
     checkboxInput(
       "check_sick_4","New cases variation (+%)", value= F),
     
     h3("Choose the data"),
     checkboxInput(
       "check_sick_data_1","Total cases", value= T),
     checkboxInput(
       "check_sick_data_2","PCR cases", value= F),
     checkboxInput(
       "check_sick_data_3","Antibody test cases", value= F),
     
     sliderInput(
       "dates_sick",
       h3("Choose a date range"),
       min = as.Date("2020-02-25", "%Y-%m-%d"),
       max = as.Date("2020-05-11", "%Y-%m-%d"),
       value = c(as.Date("2020-02-25"), as.Date("2020-05-11")),
       timeFormat = "%Y-%m-%d"
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("plot_sick")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library("ggplot2")
  library("ggpubr")
  
  df<-as.data.frame(get_table_example())
  output$tbl <- renderTable({ df},  
                            striped = TRUE,  
                            spacing = 's')  
  
  datasets <- get_data()
  data <- datasets[[1]]
  data_2 <- datasets[[2]]
  
  last_day <- data[dim(data)[1], 1]
  
  
  
  #Creating the plots:
  plot_total_linear_func <-
    function(date_range, dataset, title, ylab, xlab) {
      dates <-
        dataset$dates[dataset$dates >= date_range[1] &
                        dataset$dates <= date_range[2]]
      cases <-
        dataset$values[dataset$dates >= date_range[1] &
                         dataset$dates <= date_range[2]]
      date <- as.character.Date(dates)
      df <- data.frame(cases, date)
      plot_total <-
        ggplot(data = df, aes(x = date, y = cases, group = 1)) + geom_line(color = "red") +
        geom_point() +
        ggtitle(title) +
        ylab(ylab) +
        xlab(xlab) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      return(plot_total)
    }
  
  plot_total_log_func <-
    function(date_range, dataset, title, ylab, xlab) {
      dates <-
        dataset$dates[dataset$dates >= date_range[1] &
                        dataset$dates <= date_range[2]]
      cases <-
        dataset$values[dataset$dates >= date_range[1] &
                         dataset$dates <= date_range[2]]
      date <- as.character.Date(dates)
      df <- data.frame(cases, date)
      plot_total <-
        ggplot(data = df, aes(x = date, y = cases, group = 1)) +
        geom_line(color = "red") +
        geom_point() +
        scale_y_continuous(trans = 'log10') +
        ggtitle(title) +
        ylab(ylab) +
        xlab(xlab) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      return(plot_total)
    }
  
  plot_new_cases_abs_func <-
    function(date_range, dataset, title, ylab, xlab) {
      dates <-
        dataset$dates[dataset$dates >= date_range[1] &
                        dataset$dates <= date_range[2]]
      cases <-
        dataset$values[dataset$dates >= date_range[1] &
                         dataset$dates <= date_range[2]]
      cases <- get_daily_increment_absolute(cases)
      date <- as.character.Date(dates)
      df <- data.frame(cases, date)
      plot_new_cases_abs <-
        ggplot(data = df,
               aes(x = date, y = cases, group = 1)) +
        geom_line(color = "green") +
        geom_point() +
        ggtitle(title) +
        ylab(ylab) +
        xlab(xlab) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      return(plot_new_cases_abs)
    }
  
  plot_new_cases_perc_func <-
    function(date_range, dataset, title, ylab, xlab) {
      dates <-
        dataset$dates[dataset$dates >= date_range[1] &
                        dataset$dates <= date_range[2]]
      cases <-
        dataset$values[dataset$dates >= date_range[1] &
                         dataset$dates <= date_range[2]]
      cases <- get_daily_increment_percentage(cases)
      date <- as.character.Date(dates)
      df <- data.frame(cases, date)
      plot_new_cases_perc <-
        ggplot(data = df,
               aes(x = date, y = cases, group = 1)) +
        geom_line(color = "blue") +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        ggtitle(title) +
        ylab(ylab) +
        xlab(xlab) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      return(plot_new_cases_perc)
    }
  
  ######################################################################
  # Sick
  ######################################################################
  plot_sick_1 <- reactive({
    if (!input$check_sick_1)
      return(NULL)
    date_range <-
      c(input$dates_sick[1], input$dates_sick[2])
    dataset <- data.frame(data$casos_total, data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title <- "Sick vs date"
    ylab <- "Cases (linear)"
    xlab <- "Date"
    ggplotly(plot_total_linear_func(date_range, dataset, title, ylab, xlab))
  })
  plot_sick_2 <- reactive({
    if (!input$check_sick_2)
      return(NULL)
    date_range <-
      c(input$dates_sick[1], input$dates_sick[2])
    dataset <- data.frame(data$casos_total, data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title <- "Sick vs date"
    ylab <- "Cases (log)"
    xlab <- "Date"
    ggplotly(plot_total_log_func(date_range, dataset, title, ylab, xlab))
  })
  plot_sick_3 <- reactive({
    if (!input$check_sick_3)
      return(NULL)
    date_range <-
      c(input$dates_sick[1], input$dates_sick[2])
    dataset <- data.frame(data$casos_total, data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title <- "Sick vs date"
    ylab <- "New cases"
    xlab <- "Date"
    ggplotly(plot_new_cases_abs_func(date_range, dataset, title, ylab, xlab))
  })
  plot_sick_4 <- reactive({
    if (!input$check_sick_4)
      return(NULL)
    date_range <-
      c(input$dates_sick[1], input$dates_sick[2])
    dataset <- data.frame(data$casos_total, data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title <- "Sick vs date"
    ylab <- "New cases variation (+%)"
    xlab <- "Date"
    ggplotly(plot_new_cases_perc_func(date_range, dataset, title, ylab, xlab))
  })
  
  output$plot_sick = renderPlotly({
    ptlist <-
      list(plot_sick_1(),
           plot_sick_2(),
           plot_sick_3(),
           plot_sick_4())
    if (length(ptlist) == 1) {
      wtlist = c(100)
    }
    if (length(ptlist) == 2) {
      wtlist = c(50, 50)
    }
    if (length(ptlist) == 3) {
      wtlist = c(33.33, 33.33, 33.33)
    }
    if (length(ptlist) == 4) {
      wtlist = c(25, 25, 25, 25)
    }
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist, is.null)
    ptlist <- ptlist[to_delete]
    #wtlist <- wtlist[to_delete]
    if (length(ptlist) == 0)
      return(NULL)
    return(subplot(
      ptlist,
      nrows = length(ptlist),
      shareX = T,
      shareY = F
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

