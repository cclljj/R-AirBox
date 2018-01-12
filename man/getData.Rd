\name{getData}
\alias{getData}
\title{Selecting airbox data}
\description{
\code{getData} can select the airbox data for a specified period of time.
}
\usage{
getData(data)
}
\arguments{
\item{data}{A character implies the month of the airbox data which is going to be selected. The character should has 7 digits with the form of \strong{"YYYY-MM"}.}
}
\value{
\code{getData} returns a data frame of the airbox data for the selected period of time.
}
\note{
 In this airbox package, only 3 months of airbox data, which includes the information from 2017-JAN to 2017-MAR, was provided.
 Also, the timezone of the time of airbox data is in GMT+8.
}
\examples{
mydata <- getData(data="2017-01")
}
