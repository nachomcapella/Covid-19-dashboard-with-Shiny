library("ggplot2")
library("ggpubr")

#Reading the data:
data <- read.csv(file = "./datasets/COVID 19/nacional_covid19.csv")
colnames(data)[1] <- "fecha"
data$fallecimientos[is.na(data$fallecimientos)] <- 0

#Working with the data:
get_daily_increment_absolute <- function(cases) {
  number_days <- length(cases)
  print(number_days)
  daily_increment = c()
  daily_increment[1] = 0
  
  for (i in c(1:number_days)) {
    if (i == number_days) {
      daily_increment[i] <- cases[i] - cases[i - 1]
      return(daily_increment)
    }
    present_day <- cases[i + 1]
    previous_day <- cases[i]
    increment <- (present_day - previous_day)
    daily_increment[i + 1] <- increment
  }
  
  return(daily_increment)
}

get_daily_increment_percentage <- function(cases) {
  number_days <- length(cases)
  print(number_days)
  daily_increment = c()
  daily_increment[1] = 0
  
  for (i in c(1:number_days)) {
    if (i == number_days) {
      daily_increment[i] <- cases[i] / cases[i - 1] * 100 - 100
      return(daily_increment)
    }
    present_day <- cases[i + 1]
    previous_day <- cases[i]
    if (previous_day == 0) {
      increment <- 0
    } else{
      increment <- present_day / previous_day * 100 - 100
    }
    daily_increment[i + 1] <- increment
  }
  
  return(daily_increment)
}

#
plot_number_cases <-
  ggplot(data = data, aes(x = fecha, y = casos, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  scale_y_continuous(trans='log10') +
  ggtitle("Número de casos vs fecha") +
  ylab("Número de casos") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_increment_absolute <-
  ggplot(data = data,
         aes(x = fecha, y = infectados_incremento_absoluto, group = 1)) +
  geom_line(color = "green") +
  geom_point() +
  ggtitle("Casos nuevos vs fecha") +
  ylab("Número de casos") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_increment_percertange <-
  ggplot(data = data,
         aes(x = fecha, y = infectados_incremento_porcentual, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Variación porcentual de casos nuevos") +
  ylab("Incremento (+%)") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_list <-
  c(plot_number_cases,
    plot_increment_absolute,
    plot_increment_percertange)

plot_casos_nuevos <-
  ggarrange(
    plot_number_cases,
    plot_increment_absolute,
    plot_increment_percertange,
    ncol = 1,
    nrow = 3
  )
annotate_figure(plot_casos_nuevos,
                top = text_grob("Análisis contagiados", face = "bold", size = 14))

daily_increment_absolute <- get_daily_increment_absolute(data$casos)
data["infectados_incremento_absoluto"] <- daily_increment_absolute
daily_increment_percentage <-
  get_daily_increment_percentage(data$infectados_incremento_absoluto)
data["infectados_incremento_porcentual"] <-
  daily_increment_percentage

#Fallecidos:
daily_increment_absolute <-
  get_daily_increment_absolute(data$fallecimientos)
data["fallecidos_incremento_absoluto"] <- daily_increment_absolute
daily_increment_percentage <-
  get_daily_increment_percentage(data$fallecidos_incremento_absoluto)
data["fallecidos_incremento_porcentual"] <-
  daily_increment_percentage

plot_number_cases <-
  ggplot(data = data, aes(x = fecha, y = fallecimientos, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  scale_y_continuous(trans='log10') +
  ggtitle("Número de fallecidos vs fecha") +
  ylab("Número de fallecidos") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_increment_absolute <-
  ggplot(data = data,
         aes(x = fecha, y = fallecidos_incremento_absoluto, group = 1)) +
  geom_line(color = "green") +
  geom_point() +
  ggtitle("Fallecidos nuevos vs fecha") +
  ylab("Número de fallecidos") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_increment_percertange <-
  ggplot(data = data,
         aes(x = fecha, y = fallecidos_incremento_porcentual, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Variación porcentual de falecidos nuevos") +
  ylab("Incremento (+%)") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_list <-
  c(plot_number_cases,
    plot_increment_absolute,
    plot_increment_percertange)

plot_fallecidos_nuevos <-
  ggarrange(
    plot_number_cases,
    plot_increment_absolute,
    plot_increment_percertange,
    ncol = 1,
    nrow = 3
  )
annotate_figure(plot_fallecidos_nuevos,
                top = text_grob("Análisis fallecidos", face = "bold", size = 14))


##################################################################################
#Aragón
##################################################################################
data_2 <- read.csv(file = "./datasets/COVID 19/ccaa_covid19_casos.csv")
casos_aragon <- t(data_2[2,3:length(data_2)]) #Transpose desired data.
data["infectados_aragon"] <- c(0,0,casos_aragon)
  
daily_increment_absolute <- get_daily_increment_absolute(data$infectados_aragon)
data["infectados_aragon_incremento_absoluto"] <- daily_increment_absolute
daily_increment_percentage <-
  get_daily_increment_percentage(data$infectados_aragon_incremento_absoluto)
data["infectados_aragon_incremento_porcentual"] <-
  daily_increment_percentage

plot_number_cases <-
  ggplot(data = data, aes(x = fecha, y = infectados_aragon, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  scale_y_continuous(trans='log10') +
  ggtitle("Número de casos vs fecha") +
  ylab("Número de casos") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_increment_absolute <-
  ggplot(data = data,
         aes(x = fecha, y = infectados_aragon_incremento_absoluto, group = 1)) +
  geom_line(color = "green") +
  geom_point() +
  ggtitle("Casos nuevos vs fecha") +
  ylab("Número de casos") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_increment_percertange <-
  ggplot(data = data,
         aes(x = fecha, y = infectados_aragon_incremento_porcentual, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Variación porcentual de casos nuevos") +
  ylab("Incremento (+%)") +
  xlab("Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_list <-
  c(plot_number_cases,
    plot_increment_absolute,
    plot_increment_percertange)

plot_casos_nuevos <-
  ggarrange(
    plot_number_cases,
    plot_increment_absolute,
    plot_increment_percertange,
    ncol = 1,
    nrow = 3
  )
annotate_figure(plot_casos_nuevos,
                top = text_grob("Análisis contagiados en Aragón", face = "bold", size = 14))



