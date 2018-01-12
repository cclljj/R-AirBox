\name{week_day}
\alias{week_day}
\title{Present three plots indicating the average concentration of PM2.5, using weekday and hour as variables individually}
\description{
\code{week_day} is a function analyzing PM2.5 data that is input by using temporal factors. Boxplot for PM2.5 Measurement (factored by days in a week) that is returned uses the days in a week as factor, which are Sunday, Monday, Tuesday, Wednesday, Thrusday, Friday, and Saturday. Boxplot for PM2.5 Measurement (factored by hours in a day) that is returned uses the hours in a day as factor, which are the numbers between 0-23. PM2.5 concentration by day of the week is a line chart that is returned uses the hours in a week as factor, which are 24 hours in each day of the week.
}
\usage{
week_day(mydata)
}
\arguments{
\item{mydata}{A dataset that is either a subset of \code{wholedata}, or a dataset that has the construcion needed for analyze. A dataset that is able to analyze with \code{week_day} should contain a POSIXlt form of time with a certain format (please see the Time_1 column in wholedata), and PM2.5 concentration in specific time named PM2.5}
}
\value{
\code{week_day} returns three plot. The plot on the bottom left is titled "Boxplot for PM2.5 Measurement", with a subtitle of "(factored by days in a week)", which is a plot indicating the concentration of PM2.5 during a week using days as variables. The plot on the bottom right is titled "Boxplot for PM2.5 Measurement" with a subtitle of "(factored by hours in a day", which is a plot indicating the concentration of PM2.5 during a day using hour as variables. The plot on the top is titled "PM2.5 concentration by day of the week" is a line graph indicating the concentration of PM2.5 during a week using hours of each day as varibles.
}
\examples{
week_day(getData('2017-02'))
}
