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
                 "check_sick_1","Total cases (linear)", value= T),
               checkboxInput(
                 "check_sick_2","Total cases (log)", value= F),
               checkboxInput(
                 "check_sick_3","New cases (absolute)", value= F),
               checkboxInput(
                 "check_sick_4","New cases (%)", value= F),
               sliderInput(
                 "dates_sick",
                 h3("Choose a date range"),
                 min = as.Date("2020-02-25", "%Y-%m-%d"),
                 max = as.Date("2020-04-14", "%Y-%m-%d"),
                 value = c(as.Date("2020-02-25"), as.Date("2020-04-14")),
                 timeFormat = "%Y-%m-%d"
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("plot_sick")
             )
           )),
  tabPanel("Dead",
           sidebarLayout(
             sidebarPanel(
               h3("Choose a visualization"),
               checkboxInput(
                 "check_dead_1","Total cases (linear)", value= T),
               checkboxInput(
                 "check_dead_2","Total cases (log)", value= F),
               checkboxInput(
                 "check_dead_3","New cases (absolute)", value= F),
               checkboxInput(
                 "check_dead_4","New cases (%)", value= F),
               sliderInput(
                 "dates_dead",
                 h3("Choose a date range"),
                 min = as.Date("2020-02-25", "%Y-%m-%d"),
                 max = as.Date("2020-04-14", "%Y-%m-%d"),
                 value = c(as.Date("2020-02-25"), as.Date("2020-04-14")),
                 timeFormat = "%Y-%m-%d"
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("plot_dead")
             ))),
  tabPanel("Regions",
           sidebarLayout(
             sidebarPanel(
               
               selectInput("select_reg", h3("Select CC.AA."), 
                           choices = list("Andalucia" = "andalucia",
                                          "Aragon" = "aragon",
                                          "Asturias" = "asturias",
                                          "Baleares" = "baleares",
                                          "Canarias" = "canarias",
                                          "Cantabria" = "cantabria",
                                          "Castilla-La Mancha" = "castillalamancha",
                                          "Castilla y Leon" = "castillayleon",
                                          "Cataluna" = "cataluna",
                                          "Ceuta" = "ceuta",
                                          "C. Valenciana" = "cvalenciana",
                                          "C. de Madrid" = "cdemadrid",
                                          "Extremadura" = "extremadura",
                                          "Galicia" = "galicia",
                                          "Melilla" = "melilla",
                                          "Murcia" = "murcia",
                                          "Navarra" = "navarra",
                                          "Pais Vasco" = "paisvasco",
                                          "La Rioja" = "larioja"),
                           selected = "andalucia"),
               
               h3("Choose a visualization"),
               checkboxInput(
                 "check_reg_1","Total cases (linear)", value= T),
               checkboxInput(
                 "check_reg_2","Total cases (log)", value= F),
               checkboxInput(
                 "check_reg_3","New cases (absolute)", value= F),
               checkboxInput(
                 "check_reg_4","New cases (%)", value= F),
               sliderInput(
                 "dates_reg",
                 h3("Choose a date range"),
                 min = as.Date("2020-02-25", "%Y-%m-%d"),
                 max = as.Date("2020-04-14", "%Y-%m-%d"),
                 value = c(as.Date("2020-02-25"), as.Date("2020-04-14")),
                 timeFormat = "%Y-%m-%d"
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("plot_reg")
             )))
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  library("ggplot2")
  library("ggpubr")
  
  #Reading the data:
  print("Reading the data...")
  data <- read.csv(file = "./data/nacional_covid19.csv")
  colnames(data)[1] <- "fecha"
  data$fecha <- as.Date(data$fecha)
  data$fallecimientos[is.na(data$fallecimientos)] <- 0
  
  data_2 <- read.csv(file = "./data/ccaa_covid19_casos.csv", header = T)
  data_2<-data_2[1:19,3:50]
  data_2<-t(data_2)
  names <- c("andalucia","aragon","asturias","baleares","canarias","cantabria","castillalamancha","castillayleon","cataluna","ceuta","cvalenciana","extremadura","galicia","cdemadrid","melilla","murcia","navarra","paisvasco","larioja")
  colnames(data_2)<-names
  data_2<-as.data.frame(rbind(rep(0,18), rep(0,18), data_2))
  fecha<-data$fecha
  data_2<-as.data.frame(cbind(fecha,data_2))
  
  print("Data modified!")
  
  #Creating the plots:
  plot_total_linear_func <- function(date_range, dataset, title, ylab, xlab) {
    
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
  
  plot_total_log_func <- function(date_range,dataset, title, ylab, xlab) {
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
  
  plot_new_cases_abs_func <- function(date_range,dataset, title, ylab, xlab) {
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
  
  plot_new_cases_perc_func <- function(date_range,dataset, title, ylab, xlab) {
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
    if (!input$check_sick_1) return(NULL)
    date_range <- c(input$dates_sick[1], input$dates_sick[2])
    dataset<-data.frame(data$casos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Sick vs date"
    ylab<-"Cases (linear)"
    xlab<-"Date"
    ggplotly(plot_total_linear_func(date_range,dataset,title,ylab,xlab))
      })
  plot_sick_2 <- reactive({
    if (!input$check_sick_2) return(NULL)
    date_range <- c(input$dates_sick[1], input$dates_sick[2])
    dataset<-data.frame(data$casos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Sick vs date"
    ylab<-"Cases (log)"
    xlab<-"Date"
    ggplotly(plot_total_log_func(date_range,dataset,title,ylab,xlab))
    })
  plot_sick_3 <- reactive({
    if (!input$check_sick_3) return(NULL)
    date_range <- c(input$dates_sick[1], input$dates_sick[2])
    dataset<-data.frame(data$casos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Sick vs date"
    ylab<-"New cases (absolute)"
    xlab<-"Date"
    ggplotly(plot_new_cases_abs_func(date_range,dataset,title,ylab,xlab))
  })
  plot_sick_4 <- reactive({
    if (!input$check_sick_4) return(NULL)
    date_range <- c(input$dates_sick[1], input$dates_sick[2])
    dataset<-data.frame(data$casos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Sick vs date"
    ylab<-"New cases (+%)"
    xlab<-"Date"
    ggplotly(plot_new_cases_perc_func(date_range,dataset,title,ylab,xlab))
    })
  
  output$plot_sick = renderPlotly({
    ptlist <- list(plot_sick_1(),plot_sick_2(),plot_sick_3(),plot_sick_4())
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
    #wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    return(subplot(ptlist, nrows=length(ptlist),shareX = T, shareY = F))
    })
  
  ######################################################################
  # Dead
  ######################################################################
  plot_dead_1 <- reactive({
    if (!input$check_dead_1) return(NULL)
    date_range <- c(input$dates_dead[1], input$dates_dead[2])
    dataset<-data.frame(data$fallecimientos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Dead vs date"
    ylab<-"Dead (linear)"
    xlab<-"Date"
    ggplotly(plot_total_linear_func(date_range,dataset,title,ylab,xlab))
  })
  plot_dead_2 <- reactive({
    if (!input$check_dead_2) return(NULL)
    date_range <- c(input$dates_dead[1], input$dates_dead[2])
    dataset<-data.frame(data$fallecimientos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Dead vs date"
    ylab<-"Dead (log)"
    xlab<-"Date"
    ggplotly(plot_total_log_func(date_range,dataset,title,ylab,xlab))
  })
  plot_dead_3 <- reactive({
    if (!input$check_dead_3) return(NULL)
    date_range <- c(input$dates_dead[1], input$dates_dead[2])
    dataset<-data.frame(data$fallecimientos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Dead vs date"
    ylab<-"New dead (absolute)"
    xlab<-"Date"
    ggplotly(plot_new_cases_abs_func(date_range,dataset,title,ylab,xlab))
  })
  plot_dead_4 <- reactive({
    if (!input$check_dead_4) return(NULL)
    date_range <- c(input$dates_dead[1], input$dates_dead[2])
    dataset<-data.frame(data$fallecimientos,data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title<-"Dead vs date"
    ylab<-"New dead (+%)"
    xlab<-"Date"
    ggplotly(plot_new_cases_perc_func(date_range,dataset,title,ylab,xlab))
  })
  
  output$plot_dead = renderPlotly({
    ptlist <- list(plot_dead_1(),plot_dead_2(),plot_dead_3(),plot_dead_4())
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
    #wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    return(subplot(ptlist, nrows=length(ptlist),shareX = T, shareY = F))
  })
  


######################################################################
# CC.AA.
######################################################################

region_data <- reactive({
  return(data_2[,input$select_reg])
})
  
plot_reg_1 <- reactive({
  if (!input$check_reg_1) return(NULL)
  date_range <- c(input$dates_reg[1], input$dates_reg[2])
  dataset<-data.frame(region_data(),data$fecha)
  colnames(dataset)[1] <- "values"
  colnames(dataset)[2] <- "dates"
  title<-"Sick vs date"
  ylab<-"Cases (linear)"
  xlab<-"Date"
  ggplotly(plot_total_linear_func(date_range,dataset,title,ylab,xlab))
})
plot_reg_2 <- reactive({
  if (!input$check_reg_2) return(NULL)
  date_range <- c(input$dates_reg[1], input$dates_reg[2])
  dataset<-data.frame(region_data(),data$fecha)
  colnames(dataset)[1] <- "values"
  colnames(dataset)[2] <- "dates"
  title<-"Sick vs date"
  ylab<-"Cases (log)"
  xlab<-"Date"
  ggplotly(plot_total_log_func(date_range,dataset,title,ylab,xlab))
})
plot_reg_3 <- reactive({
  if (!input$check_reg_3) return(NULL)
  date_range <- c(input$dates_reg[1], input$dates_reg[2])
  dataset<-data.frame(region_data(),data$fecha)
  colnames(dataset)[1] <- "values"
  colnames(dataset)[2] <- "dates"
  title<-"Sick vs date"
  ylab<-"New cases (absolute)"
  xlab<-"Date"
  ggplotly(plot_new_cases_abs_func(date_range,dataset,title,ylab,xlab))
})
plot_reg_4 <- reactive({
  if (!input$check_reg_4) return(NULL)
  date_range <- c(input$dates_reg[1], input$dates_reg[2])
  dataset<-data.frame(region_data(),data$fecha)
  colnames(dataset)[1] <- "values"
  colnames(dataset)[2] <- "dates"
  title<-"Sick vs date"
  ylab<-"New cases (+%)"
  xlab<-"Date"
  ggplotly(plot_new_cases_perc_func(date_range,dataset,title,ylab,xlab))
})

output$plot_reg = renderPlotly({
  ptlist <- list(plot_reg_1(),plot_reg_2(),plot_reg_3(),plot_reg_4())
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
  #wtlist <- wtlist[to_delete]
  if (length(ptlist)==0) return(NULL)
  return(subplot(ptlist, nrows=length(ptlist),shareX = T, shareY = F))
})

}

# Run the application
shinyApp(ui = ui, server = server)
