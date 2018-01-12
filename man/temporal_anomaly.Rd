\name{temporal_anomaly}
\alias{temporal_anomaly}
\title{Determine if there is temporal anomaly for the input device}
\description{
\code{temporal_anomaly} trace back, and compare with the closet prior data for the chosen device at the certain time entered. A threshold would be determined by the chosen device's PM2.5 concentration at the certain time entered. The absolute difference between the PM2.5 concentration at the entered time and the closest prior concentration for the chosen device would be compared with the threshold. If the threshold is exceeded, a TRUE representing temporal anomaly would be returned. In contrast, if the threshold is not exceeded, a FALSE representing no temporal anomaly would be exceeded.
}
\usage{
\code{temporal_anomaly(id = '74DA3895C392' , time = '2017-02-24 10:10:30')}
}
\arguments{
\item{id}{A character implies the device's id that is interested for spatial-anomaly-testing. If the id that is entered does not contain data, the function will stop. The default setting for id is '74DA3895C392'.}
\item{time}{A string of characters that defines the specific time that is interested for spatial-anomaly-testing. The character string should follow the format:"Year(four digits)-month(two digits)-day(two digits) Hour(two digits):Minute(2 digits):Seconds(2 digits)". For instance, "2017-01-25 18:24:56". There should be remark that there is a space between the date and the time, and format in under an 24-hour-setting. }
}

\value{
    \code{temporal_anomaly} returns either TRUE or FALSE. If TRUE is returned, there is temporal anomaly existence for the chosen device. If FALSE is returned, there is no temporal anomaly existence. If there is no recorded PM2.5 concentration at the entered time for the chosen device, an adjusted time will be set by tracking a closest time that contain a record for PM2.5 concentration. The adjusted time may be either prior or later than the time entered, only if it is the nearest time.
}

\seealso{
\code{\link{getData}} to select the airbox data.
\code{\link{week_day}} to show the boxplot for the concentration of PM2.5.
\code{\link{spatial_anomaly}} to examine for spatial anomaly in PM2.5 concentration.
}

\examples{
#example for the input time being adjusted (there is no specific PM2.5 data under the entered time)
temporal_anomaly(id = '74DA3895C392' , time = '2017-02-24 10:10:30')
temporal_anomaly()

#example for the input time without being adjusted
temporal_anomaly(id = '74DA3895C392' , time = '2017-02-24 10:08:31')
}

\details{
1. If the entered argument for time does not access to a specific data in the chosen device, the time will be adjusted to the closest time either prior or later to the entered time. The adjusted time will be printed out in the console section.
2. The threshold would be determined by the PM2.5 concentrtion at the entered time for the chosen device. If the PM2.5 concentration is below 30, the threshold would be set as 5; if the PM2.5 concentration exceeds 30, the threshold would be one six of the PM2.5 concentration at the entered time for the chosen device. For example, if the PM2.5 concentration at the entered time for the id entered is 72, the threshold would be 72/6 = 12.
}
