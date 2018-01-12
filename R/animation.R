animation <- function(w = 450 , h = 300)
{

  if("magick" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages('magick')
  }

  library('magick')

  URL <- "https://data.lass-net.org/LASS/assets/IDW_gif/Taiwan_latest_last_24h.gif"
  User <- as.list(Sys.info())
  username <- User$user

  if(Sys.info()['sysname'] == 'Darwin')
  {
    pathway <- as.character(paste0('/Users/' , username , '/taiwan24.gif'))
    download.file(URL , pathway , method = 'curl' )
  }

  if(Sys.info()['sysname'] == 'Windows')
  {
    pathway <- as.character(paste0('C:/Users/' , username , '/taiwan24.gif'))
    download.file(URL , pathway , mode = "wb")
  }



  k <- paste0(w , 'x' , h , '!')

  gif_taiwan24 <- image_read(path = pathway)
  gif_taiwan24 <- image_scale(gif_taiwan24 , as.character(k))
  print('gif file for the pm2.5 distribution in past 24 hours in Taiwan
        has be downloaded in user file, named as "taiwan24.gif"')
  gif_taiwan24


}
