\name{spatial_anomaly}
\alias{spatial_anomaly}
\title{Determine if there is spatial anomaly for the input device}
\description{
\code{spatial_anomaly}  calculates the median of the input device's PM2.5 concentration in the between 15 minutes prior or after the input time. If there is no specific PM2.5 data under the entered time, an adjusted time will replace the entered time by tracing the time with the least difference either prior or after. The chosen device's median concentration will be compared with the cross-sectional data within all devices appearing in selected distance. A threshold will be determined in the chosen device's median PM2.5 concentration. There is description of how the threshold is decided in the "remark" section below. If the absolute difference between the chosen device's PM2.5 concentration and the median within neighbor devices in entered distance is larger than the threshold level, an anomaly is determined for the chosen device; and will return a stuffed red circle in a map. In contrast, if the difference of PM2.5 median concentration between chosen device and device within entered distance, there is no abnomaly, which returns with a stuffed brown circle in a map.
}
\usage{
spatial_anomaly(id , time , distance , unit , zoom)
}
\arguments{
\item{id}{A character implies the device's id that is interested for spatial-anomaly-testing. If the id that is entered does not contain data, the function will stop. The default setting for id is '74DA3895C392'.}
\item{time}{A string of characters that defines the specific time that is interested for spatial-anomaly-testing. The character string should follow the format:"Year(four digits)-month(two digits)-day(two digits) Hour(two digits):Minute(2 digits):Seconds(2 digits)". For instance, "2017-01-25 18:24:56". There should be remark that there is a space between the date and the time, and format in under an 24-hour-setting. }
\item{distance}{A numeric implies the distance that is interested to be included in for spatial-anomaly-testing. An unit can be chose for the "unit" argument, and the default setting is a meter measurement.}
\item{unit}{A character that implies the unit that should be attached to the numeric distance. "unit" can be either "m" or "km", which represents meter or kilometer. Other inputs are not available for "unit". The default setting is "m", which represents a meter measuring unit.}
\item{zoom}{map zoom, an integer from 3 (continent) to 21 (building), default value 13. openstreetmaps limits a zoom of 18, and the limit on stamen maps depends on the maptype. "auto" automatically determines the zoom for bounding box specifications, and is defaulted to 10 with center/zoom specifications. maps of the whole world currently not supported. The argument is directly referenced from [Package ggmap version 2.6.1]}
}
\value{
\code{sptial_anomaly} returns in a map. If a spatial anomaly exists for the chosen device at the entered or adjusted time, the device will imply a brown stuffed circle in its latitude and longitude; if a spatial anomaly does not exist for the chosen device at the entered or adjusted time, the device will imply a red stuffed circle in its latitude and longitude. There will be a number indicating the median PM2.5 concentration between the prior and later 15 minutes of the entered/adjusted time on the upper-right of the circle. The squares in the maps imply the neighbor devices. Blue squares represents those with data recorded between the prior and later 15 minutes of the entered/adjusted time, and the median for the individual device is presented at the upper-right of the squares, as well. The gray squares represent those does not exist recorded data between the prior and later 15 minutes of the entered/adjusted time.
}
\examples{
#example for the input time being adjusted (there is no specific PM2.5 data under the entered time)
spatial_anomaly(id = '74DA3895C392' , time = '2017-02-24 10:10:30' , dist = 3000 , unit = 'm' , zoom = 13)
spatial_anomaly()

#example for the input time without being adjusted
spatial_anomaly(id = '74DA3895C392' , time = '2017-02-24 10:08:31' , dist = 3000 , unit = 'm' , zoom = 13)
}
\seealso{
\code{\link{getData}} to select the airbox data.
\code{\link{week_day}} to show the boxplot for the concentration of PM2.5.
\code{\link{temporal_anomaly}} to examine for temporal anomaly in PM2.5 concentration.
}
\details{
1. If the entered argument for time does not access to a specific data in the chosen device, the time will be adjusted to the closest time either prior or later to the entered time. The adjusted time will be printed out in the console section.
2. Beside the map returned, there will be several things printed out. The adjusted time will be printed out if there is a time adjustment, which happened when there is no directly specific data for the chosen device at the entered time. If the time entered does not need to be adjusted, there will be no "Adjusted time" printed out. In addition, a character list of neighbor device's id will be printed out. Then, latitude and longitude of chosen device will be printed out as well. The median of PM2.5 concentration in the neighbor area (which is the area in the distance entered) will be printed. Last, there will be either information for "There is no spatial anomaly in chosen id." or "There is spatial anomaly in choson id" being printed.
3. The determination for threshold depends on the chosen device's PM2.5 concentration. When 0 <= PM2.5 concentration < 12, the threshold would be 3.0; when 12 <= PM2.5 concentration < 24, the threshold would be 6.6; when 24 <= PM2.5 concentration < 36, the threshold would be 9.35; when 36 <= PM2.5 concentration < 42, the threshold would be 13.5; when 42 <= PM2.5 concentration < 48, the threshold would be 17.0; when 48 <= PM2.5 concentration < 54, the threshold would be 23.0; when 54 <= PM2.5 concentration < 59, the threshold would be 27.5; when 59 <= PM2.5 concentration < 65, the threshold would be 33.5; when 65 <= PM2.5 concentration < 70, the threshold would be 40.5; when PM2.5 concentration >= 70, the threshold would be 91.5.
}
