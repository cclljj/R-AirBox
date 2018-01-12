spatial_anomaly <- function(id = '74DA3895C392' , time = '2017-02-24 10:10:30' , dist = 3000 , unit = 'm' , zoom = 13)
{
  #Check for necessary package

  if("geosphere" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages('geosphere')
  }

  if("ggmap" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages('ggmap')
  }

  #library necessary packages
  library(geosphere)
  library(ggmap)

  #reset variables into suitable classes
  id <- as.character(id)
  time <- as.POSIXlt(time , format = '%Y-%m-%d %H:%M:%S')
  dist <- as.numeric(dist)
  unit <- as.character(unit)

  #check for input
  if(unit != 'm' & unit != 'km')
  {
    stop('the input of unit should be either "m" or "km"')
  }

  if(dist <= 0)
  {
    stop('dist should be bigger than zero')
  }

  #change unit if unit is kilometer
  if(unit == 'km')
  {
    dist <- 1000*dist
  }

  #check if there is exact data for the inputed time, if not, approximate it
  iddata <- wholedata[which(wholedata$device_id == id) , ]

  if(length(iddata[ , 1]) == 0)
  {
    stop('There is no data for this id')
  }

  if(length(iddata[which(iddata$device_id == id &
                         as.character(iddata$Time_1) == time) , 1]) == 0)
  {
    iddata$difftime <- as.numeric(difftime(time , iddata$Time_1 , unit = 'secs'))
    iddata$difftime <- abs(iddata$difftime)
    mintime <- min(iddata$difftime)
    time <- as.POSIXlt(iddata[which(iddata$difftime == mintime) , 'Time_1'])
    warning('Time has been adjusted to the most approximate one due to non-existence of specific time data')
    print(paste0('Adjusted time : ' , as.character(time)))
  }

  #set base longitude and latitude according to the input of id, and make a matrix so that it fits distm
  base.lon <- as.numeric(unique(wholedata[wholedata$device_id == id , 'lon']))
  base.lat <- as.numeric(unique(wholedata[wholedata$device_id == id , 'lat']))
  matrix_base <- matrix(ncol = 2 , nrow = 1 , c(base.lon , base.lat))

  #create specialized dataframe to improve effiency of analyzing
  distancedata <- wholedata[ , c('device_id' , 'lat' , 'lon')]

  #reset variables in distancedata into suitable classes
  distancedata[ , 'device_id'] <- as.factor(as.character(distancedata[ , 'device_id']))
  distancedata$lat <- as.numeric(distancedata$lat)
  distancedata$lon <- as.numeric(distancedata$lon)

  #decrease the amount of device_id data by evaluating distance input
  distancedata <- unique(distancedata)

  if(dist <= 55500)
  {
    upperlon <- base.lon + 0.5
    lowerlon <- base.lon - 0.5
    upperlat <- base.lat + 0.5
    lowerlat <- base.lat - 0.5

    distancedata <- distancedata[which(distancedata$lon >= lowerlon &
                                         distancedata$lon <= upperlon &
                                         distancedata$lat >= lowerlat &
                                         distancedata$lat <= upperlat
    ) , ]
  }

  #turn distancedata into matrix so it can fit in distm
  matrix_distancedata <- as.matrix(distancedata[ , c('lon' , 'lat')])

  #calculate distance with distm
  distance <- distm(matrix_distancedata , matrix_base , fun = distHaversine)

  #change class of distance from matrix to numeric
  distancedata[ , 'distance'] <- as.numeric(distance)

  #find the targets we want
  distancedata[ , 'target'] <- ifelse(distancedata[ , 'distance'] <= dist , 1 , 0)
  distancedata[ , 'device_id'] <- as.factor(as.character(distancedata[ , 'device_id']))
  targetdata <- distancedata[which(distancedata$target == 1) , ]
  targetid <- targetdata[ , 'device_id']
  targetid.1 <- as.vector(targetid)
  timedata <- wholedata[which(wholedata$device_id %in% targetid.1) , ]
  print(as.character(targetid))
  print(c(paste0('latitude of id:' , base.lon) , paste0('longitude of id:' , base.lat)))

  #set timedata (15 minutes prior and after
  timedata$Time_1 <- as.POSIXlt(timedata$Time_1 , format = '%Y-%m-%d %H:%M:%S')
  time_up <- time + 900
  time_down <- time - 900
  timedata.1 <- timedata[which(timedata$Time_1 <= time_up &
                                 timedata$Time_1 >= time_down) , ]

  #decide threshold
  idmedian <- as.numeric(median(timedata.1[which(timedata.1$device_id == id) , 'PM2.5']))
  threshold <- ifelse(idmedian >= 0 & idmedian < 12 , 3 ,
                      ifelse(idmedian >= 12 & idmedian < 24 , 6.6 ,
                             ifelse(idmedian >= 24 & idmedian < 36 , 9.35 ,
                                    ifelse(idmedian >= 36 & idmedian < 42 , 13.5 ,
                                           ifelse(idmedian >= 42 & idmedian < 48 , 17 ,
                                                  ifelse(idmedian >= 48 & idmedian < 54 , 23 ,
                                                         ifelse(idmedian >= 54 & idmedian < 59 , 27.5 ,
                                                                ifelse(idmedian >= 59 & idmedian < 65 , 33.5 ,
                                                                       ifelse(idmedian >= 65 & idmedian < 70 , 40.5 ,
                                                                              ifelse(idmedian >= 70 , 91.5 , stop('something went wrong')))))))))))

  #run median data
  spatial.matrix <- matrix(data = NA , nrow = length(targetid.1) , ncol = 4)
  colnames(spatial.matrix) <- c('device_id' , 'median' , 'lon' , 'lat')
  k <- 1
  for(i in targetid.1)
  {
    spatial.matrix[k , 1] <- i
    spatial.matrix[k , 2] <- median(timedata.1[which(timedata.1$device_id == as.character(i)) , 'PM2.5'])
    spatial.matrix[k , 3] <- unique(distancedata[which(distancedata$device_id == as.character(i)) , 'lon'])[[1]]
    spatial.matrix[k , 4] <- unique(distancedata[which(distancedata$device_id == as.character(i)) , 'lat'])[[1]]

    k <- k+1
  }

  #idenfying the malfunctions
  spatial.data <- as.data.frame(spatial.matrix)
  spatial.data$device_id <- as.factor(spatial.data$device_id)
  spatial.data$median <- as.numeric(as.character(spatial.data$median))
  spatial.data$diff <- spatial.data$median - idmedian
  spatial.data$diff <- abs(spatial.data$diff)
  spatial.data$malfunction <- ifelse(is.na(spatial.data$diff) == TRUE , NA ,
                                     ifelse(spatial.data$diff >= threshold , 1 , 0))
  spatial.data$lon <- as.numeric(as.character(spatial.data$lon))
  spatial.data$lat <- as.numeric(as.character(spatial.data$lat))

  spatial.data.1 <- spatial.data[-which(is.na(spatial.data$median) == TRUE) , ]
  neighbormedian <- median(timedata.1[which(timedata.1$device_id != id) , 'PM2.5'])
  print(paste0("Median of PM2.5 concentration in devices in selected area is : " , neighbormedian))
  differ <- abs(neighbormedian - idmedian)
  idcolor <- ifelse(differ < threshold , 'burlywood4' , 'orangered')
  if(idcolor == 'burlywood4')
  {
    print('There is no spatial anomaly in chosen id')
  }

  if(idcolor == 'orangered')
  {
    print('There is spatial anomaly in choson id')
  }

  #draw map

  condition.1 <- spatial.data[which(spatial.data$malfunction == 1) , ]
  condition.2 <- spatial.data[which(spatial.data$malfunction == 0) , ]
  condition.3 <- spatial.data[which(is.na(spatial.data$malfunction) == TRUE) , ]

  text1 <- paste0('id : ' , as.character(id) , ', time : ' , as.character(time))

  map <- get_map(location = c(lon = base.lon , lat = base.lat) , zoom = zoom)
  ggmap(map) +
    geom_point(aes(x = lon , y = lat ) , data = condition.1 , col = 'dodgerblue4' , pch = 15 , cex = 2) +
    geom_point(aes(x = lon , y = lat ) , data = condition.2 , col = 'dodgerblue4' , pch = 15 , cex = 2) +
    geom_point(aes(x = lon , y = lat ) , data = condition.3 , col = 'azure4' , pch = 15 , cex = 2) +
    geom_point(aes_string(x = base.lon , y = base.lat) , col = idcolor , pch = 16 , cex = 6) +
    geom_text(aes(x = lon , y = lat , label = median) , data = condition.1 , hjust = -0.2 , vjust = -0.2 , color = 'dodgerblue4') +
    geom_text(aes(x = lon , y = lat , label = median) , data = condition.2 , hjust = -0.2 , vjust = -0.2 , color = 'dodgerblue4') +
    geom_text(aes_string(x = base.lon , y = base.lat , label = as.character(idmedian)) , hjust = -0.5 , vjust = -0.5 , color = idcolor) +
    labs(title = text1) +
    theme(plot.title = element_text(size = 9 , face = 'bold'))

}

