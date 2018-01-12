neighbor <- function(mydata = getData('2017-01') , id = '74DA388FF60A' , dist = 3000 , unit = 'm' , zoom = 13)
{
  #Check for necessary packages
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
  dist <- as.numeric(dist)

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

  #set base longitude and latitude according to the input of id, and make a matrix so that it fits distm
  base.lon <- as.numeric(unique(mydata[mydata$device_id == id , 'lon']))
  base.lat <- as.numeric(unique(mydata[mydata$device_id == id , 'lat']))
  matrix_base <- matrix(ncol = 2 , nrow = 1 , c(base.lon , base.lat))

  #create specialized dataframe to improve effiency of analyzing
  distancedata <- mydata[ , c('device_id' , 'lat' , 'lon')]

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
  print(as.character(targetid))
  print(c(base.lon , base.lat))

  text1 <- paste0('id : ' , as.character(id))

  map <- get_map(location = c(lon = base.lon , lat = base.lat) , zoom = zoom)
  ggmap(map) +
    geom_point(aes(x = lon , y = lat ) , data = targetdata , col = 'dodgerblue4' , pch = 15 , cex = 2) +
    geom_point(aes_string(x = base.lon , y = base.lat) , col = 'firebrick3' , pch = 16 , cex = 6) +
    labs(title = text1 , size = 0.8) +
    theme(plot.title = element_text(size = 10 , face = 'bold'))
}
