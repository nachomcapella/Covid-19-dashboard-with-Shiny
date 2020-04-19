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
                 max = as.Date("2020-04-19", "%Y-%m-%d"),
                 value = c(as.Date("2020-02-25"), as.Date("2020-04-19")),
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
                 max = as.Date("2020-04-19", "%Y-%m-%d"),
                 value = c(as.Date("2020-02-25"), as.Date("2020-04-19")),
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
                 max = as.Date("2020-04-19", "%Y-%m-%d"),
                 value = c(as.Date("2020-02-25"), as.Date("2020-04-19")),
                 timeFormat = "%Y-%m-%d")),

             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("plot_reg")
             ))),
  tabPanel("About the tool",
           mainPanel(
             h1("COVID-19 Spain"),
             h2("Description"),
             p("COVID-19 Spain is a visualization tool that allows the interactive study of the Spanish situation caused by the SARS-CoV-2 (COVID-19) virus."),
             p("The user can visualize the national sick and dead count and the regional sick count. Each of this options can be explored through an adjustable date range and four different visualizations:"),
             p("- Total cases (linear)"),
             p("- Total cases (log)"),
             p("- Daily cases (absolute)"),
             p("- Daily cases (+%)"),
             p("The tool has been developped with the ",a("R programming language,",href="https://www.r-project.org/"), "uses the ",a("Shiny library",href="https://shiny.rstudio.com/"), "in order to make the plots interactive and is hosted on the ", a("shinnyapps.io web page.",href="https://www.shinyapps.io/"),"The source code of the tool can be found in this ",a("GitHub repository.",href="https://github.com/nachomcapella/covid-19")),
             h2("Data"),
             p("The data used by the tool are the number of sick and dead caused by COVID-19, both at a national and regional (CC. AA.) level."),
             p("I am specially thankful to the ", strong("Datadista"), " team who have facilitated a clean version of the data in their", a("GitHub repository. ", href= "https://github.com/datadista/datasets/tree/master/COVID%2019"), "They provide a more detailed list of the sources", a("here, ", href = "https://github.com/datadista/datasets/blob/master/COVID%2019/readme.md"), "being the main ones:"),
             p("- ",a("Ministerio de Sanidad, Consumo y Bienestar Social",href="https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm"), "and press office of the Ministerio de Sanidad."),
             p("- ",a("Departamento de Seguridad Nacional.", href="https://www.dsn.gob.es/gl/current-affairs/press-room")),
             p("- ",a("Instituto de Salud Carlos III. Situacion de COVID-19 en Espana.", href="https://covid19.isciii.es/")),
             p("- ",a("BOE: Crisis Sanitaria COVID-19.", href="https://www.boe.es/biblioteca_juridica/codigos/codigo.php?id=355&modo=2&nota=0&tab=2")),
             p("- ",a("Ministerio de Transportes, Movilidad y Agenda Urbana.", href="https://www.mitma.gob.es/transporte-terrestre/punto-de-informacion-de-servicios-de-restauracion")),
             h2("How to use the tool"),
             p("The tool has been designed to be intuitive to use. The user does not have to introduce any kind of data and actions required to adjust the plots are simple."),
             p("When the tool is started, the user encounters a screen like this:"),
             img(src = "global_view.JPG", height=283,width=819),
             p("On the top of the screen we can see four tabs (Sick, Dead, Regions and Instructions). On the left part of the screen we can see the control panel and, on the left, the plot panel. The image corresponds to the Sick tab, which is the one opened by default."),
             h3("Sick tab"),
             p("Let's start explaining the Sick tab. The Sick, Dead and Regions tabs work pretty much the same, so the explanation of this first tab should illustrate how the other two are used."),
             p("The Sick tab displays plots concerning the number of people infected with COVID-19 at a national level. We can see that we have two main areas: the control panel and the plot panel. The control panel allows the user to select between the different visualizations available and adjust the date range of the data displayed. The user can choose between four different visualizations:"),
             p("- ",strong("Total cases (linear): "),"Plots the total number of sick people (y axis) against the date (x axis) using a linear scale for the y axis. That is, all the points of the y axis are spaced equally. With this plot we can see the accumulated number of sick people. For example, if the first day there are 10 sick people and on the second day there are 10 new more cases, the numbers on the plot will be 10 and 20, respectively."),
             p("- ",strong("Total cases (log): "),"It is similar to the previous plot. It displays the total number of sick people (y axis) against the date (x axis) using a log10 scale for the y axis. In this case, the points in the y axis are not equally distributed. The distance between 10 and 100 is the same as the distance between 100 and 1000."),
             p("- ",strong("New cases (absolute): "),"Plots the number of people that get infected each day. The number of sick people is displayed in the y axis (using a linear scale) and the date, in the x axis. Using the previous example, if the first day there are 10 sick people and on the second day there are 10 new more cases, the numbers on the plot will be 10 and 10, respectively."),
             p("- ",strong("New cases (+%): "),"It is similar to the previous plot. It displays the percertange of new infections with respect to the previous day against the date. The percertange is plotted in the y axis and the date, again, in the x axis. The percertange represented is the percertange difference between one date and the previous one. Using the same example as before, if the first day there are 10 sick people and on the second day there are 10 new more cases, the plot will display +0%, since there has been no variation. If the first day we have 10 cases and the second day we have 100, the plot will show +900%, since there are the same number of cases plus nine times more. On the other hand, if the first day we have 100 cases and the second one we have 50, the plot will display -50%. Also, a red horizontal line is plotted at the +0% value to help to help in understanding the values."),
             p("The user can place the mouse over any point in the plot and obtain information about the date and the number of sick people. Which visualization is shown is controlled by clicking on the control panel checkboxes. The four visualizations can be displayed simultaneously. The date range is adjusted by moving the slider ends to the desired dates. In the next image we can see an example where the user has chosen two visualizations (total cases (log) and new cases (absolute)) and a date range between the 2020-03-11 and the 2020-04-7, both included."),
             img(src = "sick_tab.JPG", height = 283, width = 834),
             h3("Dead tab"),
             p("Good news: this tab works the same way as the Sick tab. The only difference is that the data, instead of corresponding to infections, deals with deaths. Everything else is identical: the type of visualizations, how to select them, how to adjust the date range, etc. The death count, as the sick cases of the Sick tab, is for the whole country."),
             h3("Regions tab"),
             p("The Regions tan is concerned with the number of infections that have taken place in each region of Spain. These regions are called Comunidades Autonomas (or CC.AA.). The third tab of the tool is pretty similar to the previous two, as it can be seen in the image below. The only difference is that the user can select among the different CC.AA. in the new box of the control panel."),
             img(src="region_tab.JPG", height = 330,width = 839)
             )
           )
))
    
        
        # Define server logic required to draw a histogram
        server <- function(input, output) {
          library("ggplot2")
          library("ggpubr")
          
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
            dataset <- data.frame(data$casos, data$fecha)
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
            dataset <- data.frame(data$casos, data$fecha)
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
            dataset <- data.frame(data$casos, data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Sick vs date"
            ylab <- "New cases (absolute)"
            xlab <- "Date"
            ggplotly(plot_new_cases_abs_func(date_range, dataset, title, ylab, xlab))
          })
          plot_sick_4 <- reactive({
            if (!input$check_sick_4)
              return(NULL)
            date_range <-
              c(input$dates_sick[1], input$dates_sick[2])
            dataset <- data.frame(data$casos, data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Sick vs date"
            ylab <- "New cases (+%)"
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
          
          ######################################################################
          # Dead
          ######################################################################
          plot_dead_1 <- reactive({
            if (!input$check_dead_1)
              return(NULL)
            date_range <-
              c(input$dates_dead[1], input$dates_dead[2])
            dataset <- data.frame(data$fallecimientos, data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Dead vs date"
            ylab <- "Dead (linear)"
            xlab <- "Date"
            ggplotly(plot_total_linear_func(date_range, dataset, title, ylab, xlab))
          })
          plot_dead_2 <- reactive({
            if (!input$check_dead_2)
              return(NULL)
            date_range <-
              c(input$dates_dead[1], input$dates_dead[2])
            dataset <- data.frame(data$fallecimientos, data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Dead vs date"
            ylab <- "Dead (log)"
            xlab <- "Date"
            ggplotly(plot_total_log_func(date_range, dataset, title, ylab, xlab))
          })
          plot_dead_3 <- reactive({
            if (!input$check_dead_3)
              return(NULL)
            date_range <-
              c(input$dates_dead[1], input$dates_dead[2])
            dataset <- data.frame(data$fallecimientos, data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Dead vs date"
            ylab <- "New dead (absolute)"
            xlab <- "Date"
            ggplotly(plot_new_cases_abs_func(date_range, dataset, title, ylab, xlab))
          })
          plot_dead_4 <- reactive({
            if (!input$check_dead_4)
              return(NULL)
            date_range <-
              c(input$dates_dead[1], input$dates_dead[2])
            dataset <- data.frame(data$fallecimientos, data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Dead vs date"
            ylab <- "New dead (+%)"
            xlab <- "Date"
            ggplotly(plot_new_cases_perc_func(date_range, dataset, title, ylab, xlab))
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
          # CC.AA.
          ######################################################################
          
          region_data <- reactive({
            return(data_2[, input$select_reg])
          })
          
          plot_reg_1 <- reactive({
            if (!input$check_reg_1)
              return(NULL)
            date_range <- c(input$dates_reg[1], input$dates_reg[2])
            dataset <- data.frame(region_data(), data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Sick vs date"
            ylab <- "Cases (linear)"
            xlab <- "Date"
            ggplotly(plot_total_linear_func(date_range, dataset, title, ylab, xlab))
          })
          plot_reg_2 <- reactive({
            if (!input$check_reg_2)
              return(NULL)
            date_range <- c(input$dates_reg[1], input$dates_reg[2])
            dataset <- data.frame(region_data(), data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Sick vs date"
            ylab <- "Cases (log)"
            xlab <- "Date"
            ggplotly(plot_total_log_func(date_range, dataset, title, ylab, xlab))
          })
          plot_reg_3 <- reactive({
            if (!input$check_reg_3)
              return(NULL)
            date_range <- c(input$dates_reg[1], input$dates_reg[2])
            dataset <- data.frame(region_data(), data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Sick vs date"
            ylab <- "New cases (absolute)"
            xlab <- "Date"
            ggplotly(plot_new_cases_abs_func(date_range, dataset, title, ylab, xlab))
          })
          plot_reg_4 <- reactive({
            if (!input$check_reg_4)
              return(NULL)
            date_range <- c(input$dates_reg[1], input$dates_reg[2])
            dataset <- data.frame(region_data(), data$fecha)
            colnames(dataset)[1] <- "values"
            colnames(dataset)[2] <- "dates"
            title <- "Sick vs date"
            ylab <- "New cases (+%)"
            xlab <- "Date"
            ggplotly(plot_new_cases_perc_func(date_range, dataset, title, ylab, xlab))
          })
          
          output$plot_reg = renderPlotly({
            ptlist <-
              list(plot_reg_1(), plot_reg_2(), plot_reg_3(), plot_reg_4())
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
        