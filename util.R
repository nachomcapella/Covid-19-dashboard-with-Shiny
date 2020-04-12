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
  
  cases<-get_daily_increment_absolute(cases)
  
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
