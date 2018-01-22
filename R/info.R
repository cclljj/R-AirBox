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
  contribution <- tapply(mydata$device_id ,mydata$device_id , length)
  contribution <- sort(contribution)

  #plot the contribution for each device

  close.screen(all = TRUE)
  split.screen(figs = c(2, 1))
  screen(n = 1)
  par(las = 2 , cex.axis = 1, mar = c(6,4,1.5,0.5),mgp = c(1.8,0.5,0),
      cex.main = 0.8 , cex.lab = 1)
  barplot(fdate , border = NA  , names.arg = xname
          , yaxs = "i" , ylim = c(0,max(fdate)+100) , xpd = F
          , ylab = "# of distinct online devices" , cex.axis = 0.4 ,
          cex.names = 0.6)

  title("Distinct Online Airbox Devices on Each Day" , line = 0)

  screen(n = 2)
  par(las = 1 , mar = c(2,4,3,1),mgp = c(1.8,0.5,0) )

  plot(1:length(contribution) , contribution,
       type = 'l' , xlab = 'Device sequence number' ,
       ylab = 'contribution of measurement data' ,
       lwd = 3 , cex.main = 0.8 , cex.axis = 0.5 , cex.lab = 1)
  title("Contribution of Devices Numerically sorted",line = 0.5)

}
