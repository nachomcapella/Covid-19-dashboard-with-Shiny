#Reading the data:
get_data <- function() {
  print("Reading the data...")
  data_national_and_dates <- get_data_national_and_dates()
  data_national <- data_national_and_dates[[1]]
  dates <- data_national_and_dates[[2]]
  
  data_ccaa_sick <- get_data_ccaa_sick(dates)
  data_ccaa_sick_pcr <- get_data_ccaa_sick_pcr(dates)
  data_ccaa_sick_test <- get_data_ccaa_sick_test(dates)
  data_ccaa_dead <- get_data_ccaa_dead(dates)
  data_ccaa_hospital <- get_data_ccaa_hospital(dates)
  data_ccaa_icu <- get_data_ccaa_icu(dates)
  data_ccaa_discharge <- get_data_ccaa_discharge(dates)
  
  print("Data read!")
  print("Data formatted!")
  print("NA values changed to 0!")
  
  return(list(data_national,data_ccaa_sick,data_ccaa_sick_pcr,data_ccaa_sick_test,data_ccaa_dead,data_ccaa_hospital,data_ccaa_icu,data_ccaa_discharge))
}

get_data_national_and_dates <- function(){
  print("- National data...")
  data <- read.csv(file = "../data/nacional_covid19.csv")
  #data<-read.csv(text=GET("https://raw.github.com/datadista/datasets/blob/master/COVID%2019/nacional_covid19.csv"))
  #data <- read.csv(data)
  colnames(data)[1] <- "fecha"
  data$fecha <- as.Date(data$fecha)
  data[is.na(data)] = 0
  return(list(data,data$fecha))
}

get_data_ccaa_sick <- function(fecha){
  print("- Sick data CCAA...")
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data <- read.csv(file="../data/ccaa_covid19_casos.csv", header = T)
  
  
  data <- data[1:19, 3:dim(data)[2]]
  data <-
    t(data)
  
  names <-
    c(
      "andalucia",
      "aragon",
      "asturias",
      "baleares",
      "canarias",
      "cantabria",
      "castillalamancha",
      "castillayleon",
      "cataluna",
      "ceuta",
      "cvalenciana",
      "extremadura",
      "galicia",
      "cdemadrid",
      "melilla",
      "murcia",
      "navarra",
      "paisvasco",
      "larioja"
    )
  colnames(data) <- names
  
 
  data <- as.data.frame(data)
  data <- as.data.frame(cbind(fecha, data))
  data[is.na(data)] = 0
  return(data)
}

get_data_ccaa_sick_pcr <- function(fecha){
  print("- Sick data CCAA (PCR)...")
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data <- read.csv(file="../data/ccaa_covid19_confirmados_pcr.csv", header = T)
  
  
  data <- data[1:19, 3:dim(data)[2]]
  data <-
    t(data)
  
  names <-
    c(
      "andalucia",
      "aragon",
      "asturias",
      "baleares",
      "canarias",
      "cantabria",
      "castillalamancha",
      "castillayleon",
      "cataluna",
      "ceuta",
      "cvalenciana",
      "extremadura",
      "galicia",
      "cdemadrid",
      "melilla",
      "murcia",
      "navarra",
      "paisvasco",
      "larioja"
    )
  colnames(data) <- names
  
  
  data <- as.data.frame(data)
  data <- as.data.frame(cbind(fecha, data))
  data[is.na(data)] = 0
  return(data)
}

get_data_ccaa_sick_test <- function(fecha){
  print("- Sick data CCAA (antiboby test)...")
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data <- read.csv(file="../data/ccaa_covid19_confirmados_test.csv", header = T)
  
  
  data <- data[1:19, 3:dim(data)[2]]
  data <-
    t(data)
  
  names <-
    c(
      "andalucia",
      "aragon",
      "asturias",
      "baleares",
      "canarias",
      "cantabria",
      "castillalamancha",
      "castillayleon",
      "cataluna",
      "ceuta",
      "cvalenciana",
      "extremadura",
      "galicia",
      "cdemadrid",
      "melilla",
      "murcia",
      "navarra",
      "paisvasco",
      "larioja"
    )
  colnames(data) <- names
  
  x <- rep(0, ncol(data))
  data <- rbind(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x, data)
  data <- as.data.frame(cbind(fecha, data))
  
  data <- as.data.frame(data)
  data <- as.data.frame(cbind(fecha, data))
  data[is.na(data)] = 0
  return(data)
}

get_data_ccaa_dead <- function(fecha){
  print("- Dead data CCAA...")
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data <- read.csv(file="../data/ccaa_covid19_fallecidos.csv", header = T)
  data <- data[1:19, 3:dim(data)[2]]
  data <-
    t(data)
  
  names <-
    c(
      "andalucia",
      "aragon",
      "asturias",
      "baleares",
      "canarias",
      "cantabria",
      "castillalamancha",
      "castillayleon",
      "cataluna",
      "ceuta",
      "cvalenciana",
      "extremadura",
      "galicia",
      "cdemadrid",
      "melilla",
      "murcia",
      "navarra",
      "paisvasco",
      "larioja"
    )
  colnames(data) <- names
  
  
  data <- as.data.frame(data)
  x <- rep(0, ncol(data))
  data <- rbind(x,x,x,x,x,x,x,x,x,x,x, data)
  data <- as.data.frame(cbind(fecha, data))
  data[is.na(data)] = 0
  return(data)
}

get_data_ccaa_hospital <- function(fecha){
  print("- Hospital data CCAA...")
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data <- read.csv(file="../data/ccaa_covid19_hospitalizados.csv", header = T)
  data <- data[1:19, 3:dim(data)[2]]
  data <-
    t(data)
  
  names <-
    c(
      "andalucia",
      "aragon",
      "asturias",
      "baleares",
      "canarias",
      "cantabria",
      "castillalamancha",
      "castillayleon",
      "cataluna",
      "ceuta",
      "cvalenciana",
      "extremadura",
      "galicia",
      "cdemadrid",
      "melilla",
      "murcia",
      "navarra",
      "paisvasco",
      "larioja"
    )
  colnames(data) <- names
  
  data <- as.data.frame(data)
  data <- as.data.frame(cbind(fecha, data))
  data[is.na(data)] = 0
  return(data)
}

get_data_ccaa_icu <- function(fecha){
  print("- ICU data CCAA...")
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data <- read.csv(file="../data/ccaa_covid19_uci.csv", header = T)
  data <- data[1:19, 3:dim(data)[2]]
  data <-
    t(data)
  
  names <-
    c(
      "andalucia",
      "aragon",
      "asturias",
      "baleares",
      "canarias",
      "cantabria",
      "castillalamancha",
      "castillayleon",
      "cataluna",
      "ceuta",
      "cvalenciana",
      "extremadura",
      "galicia",
      "cdemadrid",
      "melilla",
      "murcia",
      "navarra",
      "paisvasco",
      "larioja"
    )
  colnames(data) <- names
  
  data <- as.data.frame(data)
  data <- as.data.frame(cbind(fecha, data))
  data[is.na(data)] = 0
  return(data)
}

get_data_ccaa_discharge <- function(fecha){
  print("- Hospital discharge data CCAA...")
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data <- read.csv(file="../data/ccaa_covid19_altas.csv", header = T)
  data <- data[1:19, 3:dim(data)[2]]
  data <-
    t(data)
  
  names <-
    c(
      "andalucia",
      "aragon",
      "asturias",
      "baleares",
      "canarias",
      "cantabria",
      "castillalamancha",
      "castillayleon",
      "cataluna",
      "ceuta",
      "cvalenciana",
      "extremadura",
      "galicia",
      "cdemadrid",
      "melilla",
      "murcia",
      "navarra",
      "paisvasco",
      "larioja"
    )
  colnames(data) <- names
  
  data <- as.data.frame(data)
  
  x <- rep(0, ncol(data))
  data <- rbind(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x, data)
  data <- as.data.frame(cbind(fecha, data))
  
  data <- as.data.frame(cbind(fecha, data))
  data[is.na(data)] = 0
  return(data)
}

