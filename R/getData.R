getData <- function(data)
{
  #Check for necessary packages
  if("purrr" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages('purrr')
  }

  #checking if the input is a proper character
  type <- class(data)
  if(type != 'character')
  {
    stop('data should be an input of time seperating by dash')
  }
  datalength <- nchar(data)
  if(datalength != 7)
  {
    stop('data input should be a combine of year and month seperated by dash')
  }

  #change factor to date format
  wholedata$date <- as.POSIXlt(paste(wholedata$Date,wholedata$Time , sep = " "))

  #split character of input to year and month
  word <- strsplit(data , '-')

  datayear <- purrr::map_chr(word , 1)
  datamonth <- as.character(as.numeric(purrr::map_chr(word , 2)))
  datadate <- wholedata$date


  wholedatayear <- as.character(datadate$year + 1900)
  wholedatamonth <- as.character(datadate$mon +1)

  result1 <- wholedata[which(wholedatayear == datayear &
                               wholedatamonth == datamonth) , ]

  #output
  result1$type <- NULL
  result1$Time_1 <- NULL
  print(length(result1[ , 1]))
  return(result1)

}
