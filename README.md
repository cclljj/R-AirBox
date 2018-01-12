# R-AirBox
R package for AirBox data analysis

## Motivation

Air pollution has become an deteriorating environmental problem in the past decades, when the world focus its development on industry without enough environmental protection. Hence, for efficiently monitoring nearby air quality, LASS (Local Aware Sensing System) developed a low-cost air quality monitoring sensor with small volume, but with proper accuracy and reliability. Since a package to analyze datasets collected by the low-cost air quality sensors, we started to write the package to further interpret the meanings behind the collected data.

## Installation

Installation of airbox from GitHub is easy using the devtools package. <br>
`require(devtools) <br>
install_github(„cclljj/R-AirBox“)`<br>

## Description

 airbox is designed to analyze air quality data from low cost air quality sensors mainly as “airbox”. The main pollution that could be analyzed is the particle matters having aerodynamic diameters less than 2.5 μm, aka, PM2.5. <br>
airbox can get sample data of airbox, analyze temporal distribution of PM2.5, find neighbor devices of selected device, discern anomaly within selected area, etc. 

## Prerequisites

Here are the packages users will need to launch functions of airbox: <br>
purr qboxplot magick geosphere ggmap<br>

## Code Example

*To get sample data within the package<br>
`mydata <- getData(data = "2017-01")` <br>
*To know basic information of the dataset (including the states and the measurement length of online devices)<br>
`mydata <- getData(data = "2017-01")<br>
info(mydata = mydata)`<br>
*To know the distribution of PM2.5 distribution during days of week and hours in a day<br>
`mydata <- getData(data = ‘2017-01’)<br>
week_day(mydata = mydata)`<br>
*To draw a map indicating the neighbor devices within the selected distance of an entered device<br>
`mydata <- getData(data = ‘2017-01’)<br>
neighbor(mydata = mydata, id = '74DA388FF60A', dist = 3000, unit = 'm', zoom = 13)`<br>
*To download the animated distribution chart that shows the report of each airbox devices’ PM2.5 concentration from the past 24 hours from the internet. <br>
`animation(w = 450, h = 300)`<br>
*To know whether there is a spatial anomaly in the entered device within the certain distance and at the certain time. <br>
`spatial_anomaly(id = '74DA3895C392', time = '2017-02-24 10:10:30', dist = 3000 , unit = 'm', zoom = 13)`<br>
*To know whether there is a temporal anomaly in the entered device in the certain time entered. <br>
`temporal_anomaly(id = '74DA3895C392', time = '2017-02-24 10:10:30')`<br>

## License
This package is licensed under MIT.

