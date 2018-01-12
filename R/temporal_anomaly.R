temporal_anomaly <- function(id = '74DA3895C392' , time = '2017-02-24 10:10:30')
{
  id <- '74DA3895C392'
  time <- '2017-02-24 10:10:30'

  #check for necessary packages

  #library necessary packages

  #ensure the id and time input

  id <- as.character(id)
  time <- as.character(time)
  time <- as.POSIXlt(time , format = '%Y-%m-%d %H:%M:%S')

  #pick the prior data

  wholedata.id <- wholedata[which(wholedata$device_id == id) , ]

  if(length(wholedata.id[ , 1]) == 0)
  {
    stop('There is no data for this id')
  }

  wholedata.id$Time_1 <- as.POSIXlt(wholedata.id$Time_1 , format = '%Y-%m-%d %H:%M:%S')

  if(length(wholedata.id[which(as.character(wholedata.id$Time_1) == as.character(time)) , 1]) == 0)
  {
    difftime.aprox <- min(abs(as.numeric(difftime(wholedata.id$Time_1 , time))))
    time <- wholedata.id[which(abs(as.numeric(difftime(wholedata.id$Time_1 , time))) == difftime.aprox) , 'Time_1']
    time <- as.POSIXlt(time , format = '%Y-%m-%d %H:%M:%S')
    warning('Time has been adjusted to the most approximate one due to non-existence of specific time data')
    print(paste0('Adjusted time :' , as.character(time)))
  }

  wholedata.prior <- wholedata.id[which(wholedata.id$Time_1 <= time) , ]

  if(length(wholedata.prior[ , 1]) == 1 )
  {
    stop('There is no prior data exist for this device at this moment')
  }

  wholedata.prior$difftime <- as.numeric(difftime(wholedata.prior$Time_1 , time))
  wholedata.prior.1 <- wholedata.prior[-which(wholedata.prior$difftime == 0) , ]
  if(max(wholedata.prior.1$difftime) <= -900)
  {
    warning('the most approximately prior record exceeds 15 minutes')
  }


  difftime.max <- max(wholedata.prior.1$difftime)
  pm25.approx <- as.numeric(wholedata.prior[which(wholedata.prior$difftime == difftime.max) , 'PM2.5'])

  pm25.time <- as.numeric(wholedata.prior[which(wholedata.prior$Time_1 == time) , 'PM2.5'])

  threshold <- ifelse(pm25.time <= 30 , 5 , pm25.time/6)

  pm25.disparity <- abs(pm25.approx - pm25.time)
  malfunction <- ifelse(pm25.disparity >= threshold , 1 , 0)

  if(malfunction == 1)
  {
    return(TRUE)
  }

  if(malfunction == 0)
  {
    return(FALSE)
  }

}
