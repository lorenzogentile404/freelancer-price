# Params
max_hourly_rate = 600
min_hourly_rate = 400
daily_hours = 7
min_days = 1
max_days = 40
min_advance_payment_percentage = 10
max_advance_payment_percentage = 35
currency = "DKK"
title = "Prices Lorenzo Gentile September 2019"

# Generate file
pdf(file=paste('./',title,'.pdf',sep=''))
  
max_daily_rate = max_hourly_rate*daily_hours
min_daily_rate = min_hourly_rate*daily_hours

price <- function(x) {
  a = max_daily_rate - min_daily_rate 
  b = 1 
  c = 13.3569
  return(a*exp(-(x-b)^2/c^2) + min_daily_rate)
}

plot(price, min_days, max_days, type='l', xlab = 'Days of consultancy purchased', ylab=paste('Daily (', daily_hours,' h) rate (',currency,')',sep=''), main = title, col='blue')
text(min_days + 5,max_daily_rate, labels=c(paste(max_hourly_rate,paste(currency,'/h',sep=''))))
text(max_days - 2, min_daily_rate + 60, labels=c(paste(min_hourly_rate,paste(currency,'/h',sep=''))))
points(c(min_days,max_days),c(max_daily_rate,min_daily_rate), col = "blue", pch = 16)

before <- function(x) {
  a = max_advance_payment_percentage - min_advance_payment_percentage
  b = 1 
  c = 9.402122
  return(-a*exp(-(x-b)^2/c^2) + max_advance_payment_percentage)
}

plot(before, min_days, max_days, type='l', xlab = 'Days of consultancy purchased', ylab='Advance payment (%)', main = '', col='green')

dev.off() 

# priceOld <- function(x) {
#   min = 300
#   max = 360
#   power = 2
#   return(((x-1)^power*min+max)/(1+(x-1)^power))
# }