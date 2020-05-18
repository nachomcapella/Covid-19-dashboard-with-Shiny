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
source("get_data.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    ("COVID-19 Spain"),
    tabPanel(
      "National Level",
      tabsetPanel(
        tabPanel("Sick",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Choose the data"),
                     checkboxInput("check_sick_total", "Total sick cases", value = T),
                     checkboxInput("check_sick_pcr", "PCR", value = F),
                     checkboxInput("check_sick_test", "Antibody test", value = F),
                     
                     h3("Choose a visualization"),
                     checkboxInput("check_sick_1", "Accumulated cases (linear)", value = T),
                     checkboxInput("check_sick_2", "Accumulated cases (log)", value = F),
                     checkboxInput("check_sick_3", "New cases", value = F),
                     checkboxInput("check_sick_4", "New cases variation (+%)", value = F),
                     
                     sliderInput(
                       "dates_sick",
                       h3("Choose a date range"),
                       min = as.Date("2020-02-25", "%Y-%m-%d"),
                       max = as.Date("2020-05-17", "%Y-%m-%d"),
                       value = c(as.Date("2020-02-25"), as.Date("2020-05-17")),
                       timeFormat = "%Y-%m-%d"
                     )
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(plotlyOutput("plot_sick"))
                 )
        ),
        tabPanel("Dead",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Choose a visualization"),
                     checkboxInput(
                       "check_dead_1","Accumulated cases (linear)", value= T),
                     checkboxInput(
                       "check_dead_2","Accumulated cases (log)", value= F),
                     checkboxInput(
                       "check_dead_3","New cases", value= F),
                     checkboxInput(
                       "check_dead_4","New cases variation (+%)", value= F),
                     sliderInput(
                       "dates_dead",
                       h3("Choose a date range"),
                       min = as.Date("2020-02-25", "%Y-%m-%d"),
                       max = as.Date("2020-05-17", "%Y-%m-%d"),
                       value = c(as.Date("2020-02-25"), as.Date("2020-05-17")),
                       timeFormat = "%Y-%m-%d"
                     )
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     plotlyOutput("plot_dead")
                   ))
        ),
        tabPanel("Hospitalized",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Choose the data"),
                     checkboxInput("check_hosp_hospitalized", "Hospitalized patients", value = T),
                     checkboxInput("check_hosp_icu", "ICU patients", value = F),
                     checkboxInput("check_hosp_discharged", "Discharged patients", value = F),
                     
                     h3("Choose a visualization"),
                     checkboxInput("check_hosp_1", "Accumulated cases (linear)", value = T),
                     checkboxInput("check_hosp_2", "Accumulated cases (log)", value = F),
                     checkboxInput("check_hosp_3", "New cases", value = F),
                     checkboxInput("check_hosp_4", "New cases variation (+%)", value = F),
                     
                     sliderInput(
                       "dates_hosp",
                       h3("Choose a date range"),
                       min = as.Date("2020-02-25", "%Y-%m-%d"),
                       max = as.Date("2020-05-17", "%Y-%m-%d"),
                       value = c(as.Date("2020-02-25"), as.Date("2020-05-17")),
                       timeFormat = "%Y-%m-%d"
                     )
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(plotlyOutput("plot_hosp"))
                 )
        )
      ) #Close inner tabsetPanel
      
    ),
    tabPanel("Regional Level",
             tabsetPanel(
               tabPanel("Sick",
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput("select_reg_sick", h3("Select CC.AA."), 
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
                            
                            h3("Choose the data"),
                            checkboxInput("check_reg_sick_total", "Total sick cases", value = T),
                            checkboxInput("check_reg_sick_pcr", "PCR", value = F),
                            checkboxInput("check_reg_sick_test", "Antibody test", value = F),
                            
                            
                            h3("Choose a visualization"),
                            checkboxInput(
                              "check_reg_sick_1","Accumulated cases (linear)", value= T),
                            checkboxInput(
                              "check_reg_sick_2","Accumulated cases (log)", value= F),
                            checkboxInput(
                              "check_reg_sick_3","New cases", value= F),
                            checkboxInput(
                              "check_reg_sick_4","New cases variation (+%)", value= F),
                            
                            sliderInput(
                              "dates_reg_sick",
                              h3("Choose a date range"),
                              min = as.Date("2020-02-25", "%Y-%m-%d"),
                              max = as.Date("2020-05-17", "%Y-%m-%d"),
                              value = c(as.Date("2020-02-25"), as.Date("2020-05-17")),
                              timeFormat = "%Y-%m-%d")),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            plotlyOutput("plot_reg_sick")
                          )))
             )
               )
             )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  library("ggplot2")
  library("ggpubr")
  library("RColorBrewer")
  
  df <- as.data.frame(get_table_example())
  output$tbl <- renderTable({
    df
  },
  striped = TRUE,
  spacing = 's')
  
  datasets <- get_data()
  data_national <- as.data.frame(datasets[[1]])
  data_ccaa_sick_total <- as.data.frame(datasets[[2]])
  data_ccaa_sick_pcr <- as.data.frame(datasets[[3]])
  data_ccaa_sick_test <- as.data.frame(datasets[[4]])
  data_ccaa_dead <- as.data.frame(datasets[[5]])
  data_ccaa_hospitalized <- as.data.frame(datasets[[6]])
  data_ccaa_icu <- as.data.frame(datasets[[7]])
  data_ccaa_discharged <- as.data.frame(datasets[[8]])
  
  ######################################################################
  # National Sick                                                      #
  ######################################################################
  
  plot_sick_1 <- reactive({
    if (!input$check_sick_1)
      return(NULL)

    fecha<- data_national$fecha[data_national$fecha >= input$dates_sick[1] &
                              data_national$fecha <= input$dates_sick[2]]
    
    if(input$check_sick_total){
      casos_total<- data_national$casos_total[data_national$fecha >= input$dates_sick[1] &
                                    data_national$fecha <= input$dates_sick[2]]
    }else{
      casos_total<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_sick_pcr){
      casos_pcr<- data_national$casos_pcr[data_national$fecha >= input$dates_sick[1] &
                                                data_national$fecha <= input$dates_sick[2]]
    }else{
      casos_pcr<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_sick_test){
      casos_test<- data_national$casos_test_ac[data_national$fecha >= input$dates_sick[1] &
                                                data_national$fecha <= input$dates_sick[2]]
    }else{
      casos_test<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,casos_total,casos_pcr,casos_test)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "casos_total"
    colnames(dataset)[3] <- "casos_pcr"
    colnames(dataset)[4] <- "casos_test"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))

    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=casos_total, group=1, color="#005A32")) +
        geom_point(aes(y=casos_total, group=1, color = "#005A32")) +
        
        geom_line(aes(y = casos_pcr,group=1, color="#41AB5D")) + 
        geom_point(aes(y=casos_pcr, group=1, color = "#41AB5D")) +
        
        geom_line(aes(y = casos_test,group=1, color="#ADDD8E")) + 
        geom_point(aes(y=casos_test, group=1, color = "#ADDD8E")) +
        
        scale_color_manual(values=c("#005A32", "#41AB5D", "#ADDD8E"))+
        
        ggtitle("Sick cases") +
               ylab("Cases") +
               xlab("Date") +
               theme(axis.text.x = element_text(angle = 90, hjust = 1)) +

        theme(legend.position="none")
    )
    return(plot)
  }
  )
  
  plot_sick_2 <- reactive({
    if (!input$check_sick_2)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_sick[1] &
                                  data_national$fecha <= input$dates_sick[2]]
    
    if(input$check_sick_total){
      casos_total<- data_national$casos_total[data_national$fecha >= input$dates_sick[1] &
                                                data_national$fecha <= input$dates_sick[2]]
    }else{
      casos_total<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_sick_pcr){
      casos_pcr<- data_national$casos_pcr[data_national$fecha >= input$dates_sick[1] &
                                            data_national$fecha <= input$dates_sick[2]]
    }else{
      casos_pcr<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_sick_test){
      casos_test<- data_national$casos_test_ac[data_national$fecha >= input$dates_sick[1] &
                                                 data_national$fecha <= input$dates_sick[2]]
    }else{
      casos_test<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,casos_total,casos_pcr,casos_test)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "casos_total"
    colnames(dataset)[3] <- "casos_pcr"
    colnames(dataset)[4] <- "casos_test"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=casos_total, group=1, color="#005A32")) +
        geom_point(aes(y=casos_total, group=1, color = "#005A32")) +
        
        geom_line(aes(y = casos_pcr,group=1, color="#41AB5D")) + 
        geom_point(aes(y=casos_pcr, group=1, color = "#41AB5D")) +
        
        geom_line(aes(y = casos_test,group=1, color="#ADDD8E")) + 
        geom_point(aes(y=casos_test, group=1, color = "#ADDD8E")) +
        
        scale_color_manual(values=c("#005A32", "#41AB5D", "#ADDD8E"))+
        
        ggtitle("Sick cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none") + 
        
        scale_y_continuous(trans = 'log10') 
        
    )
    return(plot)
  })
  
  plot_sick_3 <- reactive({
    if (!input$check_sick_3)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_sick[1] &
                                  data_national$fecha <= input$dates_sick[2]]
    
    if(input$check_sick_total){
      casos_total<- data_national$casos_total[data_national$fecha >= input$dates_sick[1] &
                                                data_national$fecha <= input$dates_sick[2]]
      casos_total<-get_daily_increment_absolute(casos_total)
    }else{
      casos_total<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_sick_pcr){
      casos_pcr<- data_national$casos_pcr[data_national$fecha >= input$dates_sick[1] &
                                            data_national$fecha <= input$dates_sick[2]]
      casos_pcr<-get_daily_increment_absolute(casos_pcr)
    }else{
      casos_pcr<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_sick_test){
      casos_test<- data_national$casos_test_ac[data_national$fecha >= input$dates_sick[1] &
                                                 data_national$fecha <= input$dates_sick[2]]
      casos_test<-get_daily_increment_absolute(casos_test)
    }else{
      casos_test<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,casos_total,casos_pcr,casos_test)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "casos_total"
    colnames(dataset)[3] <- "casos_pcr"
    colnames(dataset)[4] <- "casos_test"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=casos_total, group=1, color="#005A32")) +
        geom_point(aes(y=casos_total, group=1, color = "#005A32")) +
        
        geom_line(aes(y = casos_pcr,group=1, color="#41AB5D")) + 
        geom_point(aes(y=casos_pcr, group=1, color = "#41AB5D")) +
        
        geom_line(aes(y = casos_test,group=1, color="#ADDD8E")) + 
        geom_point(aes(y=casos_test, group=1, color = "#ADDD8E")) +
        
        scale_color_manual(values=c("#005A32", "#41AB5D", "#ADDD8E"))+
        
        ggtitle("Sick cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none")
    )
    return(plot)
  })
  
  plot_sick_4 <- reactive({
    if (!input$check_sick_4)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_sick[1] &
                                  data_national$fecha <= input$dates_sick[2]]
    
    if(input$check_sick_total){
      casos_total<- data_national$casos_total[data_national$fecha >= input$dates_sick[1] &
                                                data_national$fecha <= input$dates_sick[2]]
      casos_total<-get_daily_increment_percentage(casos_total)
    }else{
      casos_total<-as.numeric(rep(NA,length(fecha)))
      
    }
    
    if(input$check_sick_pcr){
      casos_pcr<- data_national$casos_pcr[data_national$fecha >= input$dates_sick[1] &
                                            data_national$fecha <= input$dates_sick[2]]
      casos_pcr<-get_daily_increment_percentage(casos_pcr)

    }else{
      casos_pcr<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_sick_test){
      casos_test<- data_national$casos_test_ac[data_national$fecha >= input$dates_sick[1] &
                                                 data_national$fecha <= input$dates_sick[2]]
      casos_test<-get_daily_increment_percentage(casos_test)
      
    }else{
      casos_test<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,casos_total,casos_pcr,casos_test)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "casos_total"
    colnames(dataset)[3] <- "casos_pcr"
    colnames(dataset)[4] <- "casos_test"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))

    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=casos_total, group=1, color="#005A32")) +
        geom_point(aes(y=casos_total, group=1, color = "#005A32")) +
        
        geom_line(aes(y = casos_pcr,group=1, color="#41AB5D")) + 
        geom_point(aes(y=casos_pcr, group=1, color = "#41AB5D")) +
        
        geom_line(aes(y = casos_test,group=1, color="#ADDD8E")) + 
        geom_point(aes(y=casos_test, group=1, color = "#ADDD8E")) +
        
        scale_color_manual(values=c("#005A32", "#41AB5D", "#ADDD8E"))+
        
        ggtitle("Sick cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none") +
      
        geom_hline(yintercept = 0, color = "#88419D")
    )
    return(plot)
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
  
  ######################################################################
  # National Dead                                                      #
  ######################################################################
  
  plot_dead_1 <- reactive({
    if (!input$check_dead_1)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_dead[1] &
                                  data_national$fecha <= input$dates_dead[2]]
    
    fallecimientos<- data_national$fallecimientos[data_national$fecha >= input$dates_dead[1] &
                                                data_national$fecha <= input$dates_dead[2]]
    
    dataset<-data.frame(fecha,fallecimientos)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "fallecimientos"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=fallecimientos, group=1, color="#E41A1C")) +
        geom_point(aes(y=fallecimientos, group=1, color = "#E41A1C")) +
        
        scale_color_manual(values=c("#E41A1C"))+
        
        ggtitle("dead cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none")
    )
    return(plot)
  }
  )
  
  plot_dead_2 <- reactive({
    if (!input$check_dead_2)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_dead[1] &
                                  data_national$fecha <= input$dates_dead[2]]
    
      fallecimientos<- data_national$fallecimientos[data_national$fecha >= input$dates_dead[1] &
                                                data_national$fecha <= input$dates_dead[2]]
    
    
    dataset<-data.frame(fecha,fallecimientos)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "fallecimientos"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=fallecimientos, group=1, color="#E41A1C")) +
        geom_point(aes(y=fallecimientos, group=1, color = "#E41A1C")) +
        
        scale_color_manual(values=c("#E41A1C"))+
        
        ggtitle("dead cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none") + 
        
        scale_y_continuous(trans = 'log10') 
      
    )
    return(plot)
  })
  
  plot_dead_3 <- reactive({
    if (!input$check_dead_3)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_dead[1] &
                                  data_national$fecha <= input$dates_dead[2]]
    
      fallecimientos<- data_national$fallecimientos[data_national$fecha >= input$dates_dead[1] &
                                                data_national$fecha <= input$dates_dead[2]]
      fallecimientos<-get_daily_increment_absolute(fallecimientos)
    
    dataset<-data.frame(fecha,fallecimientos)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "fallecimientos"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=fallecimientos, group=1, color="#E41A1C")) +
        geom_point(aes(y=fallecimientos, group=1, color = "#E41A1C")) +
        
        scale_color_manual(values=c("#E41A1C"))+
        
        ggtitle("dead cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none")
    )
    return(plot)
  })
  
  plot_dead_4 <- reactive({
    if (!input$check_dead_4)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_dead[1] &
                                  data_national$fecha <= input$dates_dead[2]]
    
      fallecimientos<- data_national$fallecimientos[data_national$fecha >= input$dates_dead[1] &
                                                data_national$fecha <= input$dates_dead[2]]
      fallecimientos<-get_daily_increment_percentage(fallecimientos)
   
    
    dataset<-data.frame(fecha,fallecimientos)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "fallecimientos"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=fallecimientos, group=1, color="#E41A1C")) +
        geom_point(aes(y=fallecimientos, group=1, color = "#E41A1C")) +
        
        
        scale_color_manual(values=c("#E41A1C"))+
        
        ggtitle("dead cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none") +
        
        geom_hline(yintercept = 0, color = "#88419D")
    )
    return(plot)
  })
  
  output$plot_dead = renderPlotly({
    ptlist <-
      list(plot_dead_1(),
           plot_dead_2(),
           plot_dead_3(),
           plot_dead_4())
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
  
  
  ######################################################################
  # National Hospitalized                                              #
  ######################################################################
  
  plot_hosp_1 <- reactive({
    if (!input$check_hosp_1){
      return(NULL)
    }
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_hosp[1] &
                                  data_national$fecha <= input$dates_hosp[2]]
    
    if(input$check_hosp_hospitalized){
      hospitalizados<- data_national$hospitalizados[data_national$fecha >= input$dates_hosp[1] &
                                                data_national$fecha <= input$dates_hosp[2]]
    }else{
      hospitalizados<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_hosp_icu){
      ingresos_uci<- data_national$ingresos_uci[data_national$fecha >= input$dates_hosp[1] &
                                            data_national$fecha <= input$dates_hosp[2]]
    }else{
      ingresos_uci<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_hosp_discharged){
      altas<- data_national$altas[data_national$fecha >= input$dates_hosp[1] &
                                                 data_national$fecha <= input$dates_hosp[2]]
    }else{
      altas<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,hospitalizados,ingresos_uci,altas)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "hospitalizados"
    colnames(dataset)[3] <- "ingresos_uci"
    colnames(dataset)[4] <- "altas"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=hospitalizados, group=1, color="#0C2C84")) +
        geom_point(aes(y=hospitalizados, group=1, color = "#0C2C84")) +
        
        geom_line(aes(y = ingresos_uci,group=1, color="#1D91C0")) + 
        geom_point(aes(y=ingresos_uci, group=1, color = "#1D91C0")) +
        
        geom_line(aes(y = altas,group=1, color="#7FCDBB")) + 
        geom_point(aes(y=altas, group=1, color = "#7FCDBB")) +
        
        scale_color_manual(values=c("#0C2C84", "#1D91C0", "#7FCDBB"))+
        
        ggtitle("Hospitalized cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none")
    )
    return(plot)
  }
  )
  
  plot_hosp_2 <- reactive({
    if (!input$check_hosp_2)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_hosp[1] &
                                  data_national$fecha <= input$dates_hosp[2]]
    
    if(input$check_hosp_hospitalized){
      hospitalizados<- data_national$hospitalizados[data_national$fecha >= input$dates_hosp[1] &
                                                data_national$fecha <= input$dates_hosp[2]]
    }else{
      hospitalizados<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_hosp_icu){
      ingresos_uci<- data_national$ingresos_uci[data_national$fecha >= input$dates_hosp[1] &
                                            data_national$fecha <= input$dates_hosp[2]]
    }else{
      ingresos_uci<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_hosp_discharged){
      altas<- data_national$altas[data_national$fecha >= input$dates_hosp[1] &
                                                 data_national$fecha <= input$dates_hosp[2]]
    }else{
      altas<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,hospitalizados,ingresos_uci,altas)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "hospitalizados"
    colnames(dataset)[3] <- "ingresos_uci"
    colnames(dataset)[4] <- "altas"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=hospitalizados, group=1, color="#0C2C84")) +
        geom_point(aes(y=hospitalizados, group=1, color = "#0C2C84")) +
        
        geom_line(aes(y = ingresos_uci,group=1, color="#1D91C0")) + 
        geom_point(aes(y=ingresos_uci, group=1, color = "#1D91C0")) +
        
        geom_line(aes(y = altas,group=1, color="#7FCDBB")) + 
        geom_point(aes(y=altas, group=1, color = "#7FCDBB")) +
        
        scale_color_manual(values=c("#0C2C84", "#1D91C0", "#7FCDBB"))+
        
        ggtitle("Hospitalized cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none") + 
        
        scale_y_continuous(trans = 'log10') 
      
    )
    return(plot)
  })
  
  plot_hosp_3 <- reactive({
    if (!input$check_hosp_3)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_hosp[1] &
                                  data_national$fecha <= input$dates_hosp[2]]
    
    if(input$check_hosp_hospitalized){
      hospitalizados<- data_national$hospitalizados[data_national$fecha >= input$dates_hosp[1] &
                                                data_national$fecha <= input$dates_hosp[2]]
      hospitalizados<-get_daily_increment_absolute(hospitalizados)
    }else{
      hospitalizados<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_hosp_icu){
      ingresos_uci<- data_national$ingresos_uci[data_national$fecha >= input$dates_hosp[1] &
                                            data_national$fecha <= input$dates_hosp[2]]
      ingresos_uci<-get_daily_increment_absolute(ingresos_uci)
    }else{
      ingresos_uci<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_hosp_discharged){
      altas<- data_national$altas[data_national$fecha >= input$dates_hosp[1] &
                                                 data_national$fecha <= input$dates_hosp[2]]
      altas<-get_daily_increment_absolute(altas)
    }else{
      altas<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,hospitalizados,ingresos_uci,altas)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "hospitalizados"
    colnames(dataset)[3] <- "ingresos_uci"
    colnames(dataset)[4] <- "altas"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=hospitalizados, group=1, color="#0C2C84")) +
        geom_point(aes(y=hospitalizados, group=1, color = "#0C2C84")) +
        
        geom_line(aes(y = ingresos_uci,group=1, color="#1D91C0")) + 
        geom_point(aes(y=ingresos_uci, group=1, color = "#1D91C0")) +
        
        geom_line(aes(y = altas,group=1, color="#7FCDBB")) + 
        geom_point(aes(y=altas, group=1, color = "#7FCDBB")) +
        
        scale_color_manual(values=c("#0C2C84", "#1D91C0", "#7FCDBB"))+
        
        ggtitle("Hospitalized cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none")
    )
    return(plot)
  })
  
  plot_hosp_4 <- reactive({
    if (!input$check_hosp_4)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_hosp[1] &
                                  data_national$fecha <= input$dates_hosp[2]]
    
    if(input$check_hosp_hospitalized){
      hospitalizados<- data_national$hospitalizados[data_national$fecha >= input$dates_hosp[1] &
                                                data_national$fecha <= input$dates_hosp[2]]
      hospitalizados<-get_daily_increment_percentage(hospitalizados)
    }else{
      hospitalizados<-as.numeric(rep(NA,length(fecha)))
      
    }
    
    if(input$check_hosp_icu){
      ingresos_uci<- data_national$ingresos_uci[data_national$fecha >= input$dates_hosp[1] &
                                            data_national$fecha <= input$dates_hosp[2]]
      ingresos_uci<-get_daily_increment_percentage(ingresos_uci)
      
    }else{
      ingresos_uci<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_hosp_discharged){
      altas<- data_national$altas[data_national$fecha >= input$dates_hosp[1] &
                                                 data_national$fecha <= input$dates_hosp[2]]
      altas<-get_daily_increment_percentage(altas)
      
    }else{
      altas<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,hospitalizados,ingresos_uci,altas)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "hospitalizados"
    colnames(dataset)[3] <- "ingresos_uci"
    colnames(dataset)[4] <- "altas"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=hospitalizados, group=1, color="#0C2C84")) +
        geom_point(aes(y=hospitalizados, group=1, color = "#0C2C84")) +
        
        geom_line(aes(y = ingresos_uci,group=1, color="#1D91C0")) + 
        geom_point(aes(y=ingresos_uci, group=1, color = "#1D91C0")) +
        
        geom_line(aes(y = altas,group=1, color="#7FCDBB")) + 
        geom_point(aes(y=altas, group=1, color = "#7FCDBB")) +
        
        scale_color_manual(values=c("#0C2C84", "#1D91C0", "#7FCDBB"))+
        
        ggtitle("Hospitalized cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none") +
        
        geom_hline(yintercept = 0, color = "#88419D")
    )
    return(plot)
  })
  
  output$plot_hosp = renderPlotly({
    ptlist <-
      list(plot_hosp_1(),
           plot_hosp_2(),
           plot_hosp_3(),
           plot_hosp_4()
           )
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
  
  ######################################################################
  # CC.AA. Sick                                                        #
  ######################################################################
  region_data_sick_total <- reactive({
    return(data_ccaa_sick_total[, input$select_reg_sick])
  })
  region_data_sick_pcr <- reactive({
    return(data_ccaa_sick_pcr[, input$select_reg_sick])
  })
  region_data_sick_test <- reactive({
    return(data_ccaa_sick_test[, input$select_reg_sick])
  })
  
  plot_reg_sick_1 <- reactive({
    if (!input$check_reg_sick_1)
      return(NULL)

    fecha<- data_national$fecha[data_national$fecha >= input$dates_reg_sick[1] &
                                  data_national$fecha <= input$dates_reg_sick[2]]
    
    if(input$check_reg_sick_total){
      data<- region_data_sick_total()
      casos_total<- data[data_national$fecha >= input$dates_reg_sick[1] &
                                                data_national$fecha <= input$dates_reg_sick[2]]
    }else{
      casos_total<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_reg_sick_pcr){
      data<- region_data_sick_pcr()
      casos_pcr<- data[data_national$fecha >= input$dates_reg_sick[1] &
                                            data_national$fecha <= input$dates_reg_sick[2]]
    }else{
      casos_pcr<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_reg_sick_test){
      data<- region_data_sick_test()
      casos_test<- data[data_national$fecha >= input$dates_reg_sick[1] &
                                                 data_national$fecha <= input$dates_reg_sick[2]]
    }else{
      casos_test<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,casos_total,casos_pcr,casos_test)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "casos_total"
    colnames(dataset)[3] <- "casos_pcr"
    colnames(dataset)[4] <- "casos_test"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=casos_total, group=1, color="#005A32")) +
        geom_point(aes(y=casos_total, group=1, color = "#005A32")) +
        
        geom_line(aes(y = casos_pcr,group=1, color="#41AB5D")) + 
        geom_point(aes(y=casos_pcr, group=1, color = "#41AB5D")) +
        
        geom_line(aes(y = casos_test,group=1, color="#ADDD8E")) + 
        geom_point(aes(y=casos_test, group=1, color = "#ADDD8E")) +
        
        scale_color_manual(values=c("#005A32", "#41AB5D", "#ADDD8E"))+
        
        ggtitle("Sick cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none")
    )
    return(plot)
  })
  
  plot_reg_sick_2 <- reactive({
    if (!input$check_reg_sick_2)
      return(NULL)
    
    fecha<- data_national$fecha[data_national$fecha >= input$dates_reg_sick[1] &
                                  data_national$fecha <= input$dates_reg_sick[2]]
    
    if(input$check_reg_sick_total){
      data<- region_data_sick_total()
      casos_total<- data[data_national$fecha >= input$dates_reg_sick[1] &
                           data_national$fecha <= input$dates_reg_sick[2]]
    }else{
      casos_total<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_reg_sick_pcr){
      data<- region_data_sick_pcr()
      casos_pcr<- data[data_national$fecha >= input$dates_reg_sick[1] &
                         data_national$fecha <= input$dates_reg_sick[2]]
    }else{
      casos_pcr<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_reg_sick_test){
      data<- region_data_sick_test()
      casos_test<- data[data_national$fecha >= input$dates_reg_sick[1] &
                          data_national$fecha <= input$dates_reg_sick[2]]
    }else{
      casos_test<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,casos_total,casos_pcr,casos_test)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "casos_total"
    colnames(dataset)[3] <- "casos_pcr"
    colnames(dataset)[4] <- "casos_test"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=casos_total, group=1, color="#005A32")) +
        geom_point(aes(y=casos_total, group=1, color = "#005A32")) +
        
        geom_line(aes(y = casos_pcr,group=1, color="#41AB5D")) + 
        geom_point(aes(y=casos_pcr, group=1, color = "#41AB5D")) +
        
        geom_line(aes(y = casos_test,group=1, color="#ADDD8E")) + 
        geom_point(aes(y=casos_test, group=1, color = "#ADDD8E")) +
        
        scale_color_manual(values=c("#005A32", "#41AB5D", "#ADDD8E"))+
        
        ggtitle("Sick cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none") + 
        
        scale_y_continuous(trans = 'log10') 
      
    )
    return(plot)
    
  })
  plot_reg_sick_3 <- reactive({
    if (!input$check_reg_sick_3)
      return(NULL)
    fecha<- data_national$fecha[data_national$fecha >= input$dates_reg_sick[1] &
                                  data_national$fecha <= input$dates_reg_sick[2]]
    
    if(input$check_reg_sick_total){
      data<-region_data_sick_total()
      casos_total<- data[data_national$fecha >= input$dates_reg_sick[1] &
                                                data_national$fecha <= input$dates_reg_sick[2]]
      casos_total<-get_daily_increment_absolute(casos_total)
    }else{
      casos_total<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_reg_sick_pcr){
      data<-region_data_sick_pcr()
      casos_pcr<- data[data_national$fecha >= input$dates_reg_sick[1] &
                                            data_national$fecha <= input$dates_reg_sick[2]]
      casos_pcr<-get_daily_increment_absolute(casos_pcr)
    }else{
      casos_pcr<-as.numeric(rep(NA,length(fecha)))
    }
    
    if(input$check_reg_sick_test){
      data<-region_data_sick_test()
      casos_test<- data[data_national$fecha >= input$dates_reg_sick[1] &
                                                 data_national$fecha <= input$dates_reg_sick[2]]
      casos_test<-get_daily_increment_absolute(casos_test)
    }else{
      casos_test<-as.numeric(rep(NA,length(fecha)))
    }
    
    dataset<-data.frame(fecha,casos_total,casos_pcr,casos_test)
    
    colnames(dataset)[1] <- "fecha"
    colnames(dataset)[2] <- "casos_total"
    colnames(dataset)[3] <- "casos_pcr"
    colnames(dataset)[4] <- "casos_test"
    
    dataset$fecha <-(as.character.Date(dataset$fecha))
    
    plot<-ggplotly(
      ggplot(data = dataset, aes(x = fecha)) +
        geom_line(aes(y=casos_total, group=1, color="#005A32")) +
        geom_point(aes(y=casos_total, group=1, color = "#005A32")) +
        
        geom_line(aes(y = casos_pcr,group=1, color="#41AB5D")) + 
        geom_point(aes(y=casos_pcr, group=1, color = "#41AB5D")) +
        
        geom_line(aes(y = casos_test,group=1, color="#ADDD8E")) + 
        geom_point(aes(y=casos_test, group=1, color = "#ADDD8E")) +
        
        scale_color_manual(values=c("#005A32", "#41AB5D", "#ADDD8E"))+
        
        ggtitle("Sick cases") +
        ylab("Cases") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        
        theme(legend.position="none")
    )
    return(plot)
  })
  plot_reg_sick_4 <- reactive({
    if (!input$check_reg_sick_4)
      return(NULL)
    date_range <- c(input$dates_reg[1], input$dates_reg[2])
    dataset <- data.frame(region_data(), data$fecha)
    colnames(dataset)[1] <- "values"
    colnames(dataset)[2] <- "dates"
    title <- "Sick vs date"
    ylab <- "New cases variation (+%)"
    xlab <- "Date"
    ggplotly(plot_new_cases_perc_func(date_range, dataset, title, ylab, xlab))
  })
  
  output$plot_reg_sick = renderPlotly({
    ptlist <-
      list(plot_reg_sick_1(), plot_reg_sick_2(), plot_reg_sick_3(), plot_reg_sick_4())
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


