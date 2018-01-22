getData <- function(data)
{
  #Check for necessary packages
  if("purrr" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages('purrr')
  }
  #Check for necessary packages
  if("lubridate" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages('lubridate')
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

  #downloading data from internet database
  URL <- 'https://pm25.lass-net.org/Rpackage/wholedata.rda'
  User <- as.list(Sys.info())
  username <- User$user

  if(Sys.info()['sysname'] == 'Darwin')
  {
    pathway <- as.character(paste0('/Users/' , username , '/wholedata.rda'))
    pathway0 <- as.character(paste0('/Users/' , username))
    download.file(URL , pathway , method = 'curl' )
  }

  if(Sys.info()['sysname'] == 'Windows')
  {
    pathway <- as.character(paste0('C:/Users/' , username , '/wholedata.rda'))
    pathway0 <- as.character(paste0('C:/Users/' , username))
    download.file(URL , pathway , mode = "wb")
  }

  setwd(pathway0)
  load(file = pathway)

  #split character of input to year and month
  word <- strsplit(data , '-')

  datayear <- purrr::map_chr(word , 1)
  datamonth <- as.character(as.numeric(purrr::map_chr(word , 2)))
  datadate <- wholedata$Date


  wholedatayear <- as.character(lubridate::year(datadate))
  wholedatamonth <- as.character(lubridate::month(datadate))

  result1 <- wholedata[which(wholedatayear == datayear &
                               wholedatamonth == datamonth) , ]
  print(length(result1[ , 1]))
  return(result1)

}
