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


