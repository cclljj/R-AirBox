\name{animation}
\alias{animation}
\title{
Download the gif file of the PM2.5 distribution in Taiwan.
}
\description{
\code{animation} is used to download the animated distribution chart from https://data.lass-net.org/GIS/IDW/. The chart shows the concentration of PM2.5 , which is reported from each airbox device in the last 24 hours.
}
\usage{
animation(w = 450, h = 300)
}
\arguments{
  \item{w}{
The value determines the width of the picture. The default value is 450.
}
  \item{h}{
The value determines the height of the picture. The default value is 300.
}
}
\details{
The gif picture downlaoded by \code{animation} is stored in user file and the picture is named as "taiwan24.gif".
}
\value{
A gif file of the PM2.5 distribution in Taiwan.
}
\examples{
animation(w = 450 , h = 300)
}
