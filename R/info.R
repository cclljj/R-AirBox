info <- function(mydata)
{
  #Check for necessary packages
  if("qboxplot" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages('qboxplot')
  }


  #plot Distinct Online Airbox Devices on Each Day
  Sys.setlocale("LC_TIME", "C")
  d <- as.factor(as.character(mydata$Date))
  data <- data.frame(mydata$device_id, d)
  colnames(data) <- c("device_id","Date")
  cdate <- table(data)
  mdate <- apply(cdate , 2 , max)
  mdate <- as.vector(mdate)
  fdate <- c()

  for(i in 1: length(mdate))
  {
    fdate[i] <- length(as.vector(which(cdate[,i]>(mdate[i]*0.5))))
  }

  airboxdate <- colnames(cdate)
  wd <- as.character(weekdays(as.Date(airboxdate)))
  xname <- c(paste(airboxdate , wd , sep = " "))

  #factoring the device id
  mydata$device_id <- factor(mydata$device_id)

  #create a new dataset knowing the information of each device
  devicecode <- c(levels(mydata$device_id))
  devicematrix <- matrix(nrow = length(devicecode) , ncol = 12)
  k <- 1
  for(i in devicecode)
  {
    codelength <- length(mydata[which(mydata$device_id == i) , 1])
    codedata <- mydata[which(mydata$device_id == i) , ]
    pm25mean <- mean(eval(parse(text = paste0('codedata' , '$PM2.5'))))
    pm25median <- median(eval(parse(text = paste0('codedata' , '$PM2.5'))))
    pm10mean <- mean(eval(parse(text = 'codedata'))[which(eval(parse(text =
                                                                       paste0('codedata' , '$type'))) == 0) , 'PM10'])
    pm10median <- median(eval(parse(text = 'codedata'))[which(eval(parse(text =
                                                                           paste0('codedata' , '$type'))) == 0) , 'PM10'])
    pm1mean <- mean(eval(parse(text = 'codedata'))[which(eval(parse(text =
                                                                      paste0('codedata' , '$type'))) == 0) , 'PM1'])
    pm1median <- median(eval(parse(text = 'codedata'))[which(eval(parse(text =
                                                                          paste0('codedata' , '$type'))) == 0) , 'PM1'])
    tempmean <- mean(eval(parse(text = paste0('codedata' , '$Temperature'))))
    humidmean <- mean(eval(parse(text = paste0('codedata' , '$Humidity'))))
    latitute <- codedata[1 , 'lat']
    longitude <- codedata[1 , 'lon']

    devicematrix[k , 1] <- i
    devicematrix[k , 2] <- codelength
    devicematrix[k , 3] <- tempmean
    devicematrix[k , 4] <- humidmean
    devicematrix[k , 5] <- pm25mean
    devicematrix[k , 6] <- pm25median
    devicematrix[k , 7] <- pm10mean
    devicematrix[k , 8] <- pm10median
    devicematrix[k , 9] <- pm1mean
    devicematrix[k , 10] <- pm1median
    devicematrix[k , 11] <- latitute
    devicematrix[k , 12] <- longitude

    toprint <- c(k , i , codelength , latitute, longitude)
    k <- k+1
  }

  devicedata <- as.data.frame(devicematrix)
  donotreplace <- devicedata
  colnames(devicedata) <- c('airboxID' , 'contribution' , 'temperaturemean' , 'humiditymean' ,
                            'pm25mean' , 'pm25median' , 'pm10mean' , 'pm10median' ,
                            'pm1mean' , 'pm1median' , 'latitute' , 'longitude')
  devicedata$contribution <- as.numeric(as.character(devicedata$contribution))
  devicedata$temperaturemean <- as.numeric(as.character(devicedata$temperaturemean))
  devicedata$humiditymean <- as.numeric(as.character(devicedata$humiditymean))
  devicedata$pm25mean <- as.numeric(as.character(devicedata$pm25mean))
  devicedata$pm25median <- as.numeric(as.character(devicedata$pm25median))
  devicedata$pm10mean <- ifelse(is.na(devicedata$pm10mean) == TRUE ,
                                NA , as.numeric(as.character(devicedata$pm10mean)))
  devicedata$pm10median <- ifelse(is.na(devicedata$pm10mean) == TRUE ,
                                  NA , as.numeric(as.character(devicedata$pm10median)))
  devicedata$pm1mean <- ifelse(is.na(devicedata$pm10median) == TRUE ,
                               NA , as.numeric(as.character(devicedata$pm1mean)))
  devicedata$pm1median <- ifelse(is.na(devicedata$pm1mean) == TRUE ,
                                 NA , as.numeric(as.character(devicedata$pm1median)))
  devicedata$latitute <- as.numeric(as.character(devicedata$latitute))
  devicedata$longitude <- as.numeric(as.character(devicedata$longitude))

  #plot the contribution for each device
  devicedata_1 <- devicedata[order(devicedata$contribution) , ]

  close.screen(all = TRUE)
  par(mfrow = c(2,1))
  par(las = 2 , cex.axis = 1, mar = c(4,4,1.5,0.5),mgp = c(1.8,0.5,0),
      cex.main = 0.8 , cex.lab = 1)
  barplot(fdate , border = NA  , names.arg = xname
          , yaxs = "i" , ylim = c(0,max(fdate)+100) , xpd = F
          , ylab = "# of distinct online devices" , cex.axis = 0.5 ,
          cex.names = 0.6)

  title("Distinct Online Airbox Devices on Each Day" , line = 0)


  par(las = 1 , mar = c(2,4,4,1),mgp = c(1.8,0.5,0) )

  plot(1:length(devicedata[ , 1]) , devicedata_1$contribution ,
       type = 'l' , xlab = 'Device sequence number' ,
       ylab = 'contribution of measurement data' ,
       lwd = 3 , cex.main = 0.8 , cex.axis = 0.5 , cex.lab = 1)
  title("Contribution of Devices Numerically sorted",line = 0.5)

}
