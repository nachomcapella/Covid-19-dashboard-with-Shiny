#Reading the data:
library(httr)
get_data <- function() {
  print("Reading the data...")
  data <- read.csv(file = "./data/nacional_covid19.csv")
  #data<-read.csv(text=GET("https://raw.github.com/datadista/datasets/blob/master/COVID%2019/nacional_covid19.csv"))
  #data <- read.csv(data)
  colnames(data)[1] <- "fecha"
  data$fecha <- as.Date(data$fecha)
  data$fallecimientos[is.na(data$fallecimientos)] <- 0
  
  #data_2 <-"https://raw.github.com/datadista/datasets/blob/master/COVID%2019/ccaa_covid19_casos.csv"
  data_2 <- read.csv(file="./data/ccaa_covid19_casos.csv", header = T)
  data_2 <- data_2[1:19, 3:dim(data_2)[2]]
  data_2 <-
    t(data_2)

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
  colnames(data_2) <- names
  data_2 <- as.data.frame(rbind(rep(0, 18), data_2))
  
  fecha <- data$fecha
  data_2 <- as.data.frame(cbind(fecha, data_2))
  print("Data modified!")
  return(list(data, data_2))
}


#Working with the data:
get_daily_increment_absolute <- function(cases) {
  number_days <- length(cases)
  daily_increment = c()
  daily_increment[1] = 0
  
  for (i in c(1:number_days)) {
    if (i == number_days) {
      daily_increment[i] <- (cases[i] - cases[i - 1])
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
  cases <- get_daily_increment_absolute(cases)
  
  number_days <- length(cases)
  daily_increment = c()
  daily_increment[1] = 0
  
  for (i in c(1:number_days)) {
    
    
    if (i==number_days) {
      present_day <- cases[i]
      previous_day <- cases[i-1]
    }else{
      present_day <- cases[i + 1]
      previous_day <- cases[i]
    }
    
    
    if (previous_day == 0 &  present_day!=0) {
      increment <- NaN
    }
    if
    (previous_day == 0 &  present_day==0){
      increment<-0
    }
     if(previous_day !=0){
      increment <- (present_day  - previous_day)/ previous_day * 100 
    }
    
    if(previous_day<0){
      increment<-increment*(-1)
    }
    
    if(i==number_days){
      daily_increment[i]<-increment
      return(daily_increment)
    }
    daily_increment[i + 1] <- increment
  }
  
  return(daily_increment)
}

get_table_example <- function(){
 a <- c(3,10,16,32,44,66,114, 135, 198)
 a<- c(10,10,100,200,100,75)
 df<-cbind(a,get_daily_increment_absolute(a),get_daily_increment_percentage(a))
 colnames(df)<-c("Total cases","New cases", "New cases variation (+%)")
 return(df)
}


