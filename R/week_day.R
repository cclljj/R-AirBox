week_day <- function (mydata = getData("2017-01"))
{
  if ("qboxplot" %in% rownames(installed.packages()) == FALSE) {
    install.packages("qboxplot")
  }
  library(qboxplot)
  mydata$Time_1 <- paste0(mydata$Date, "-", mydata$Time)
  mydata$Time_1 <- as.POSIXlt(mydata$Time_1, format = "%Y-%m-%d-%H:%M:%S")
  mydata$weekday <- as.factor((mydata$Time_1)$wday)
  mean0 <- mean(mydata[which(mydata$weekday == 0), "PM2.5"])
  mean1 <- mean(mydata[which(mydata$weekday == 1), "PM2.5"])
  mean2 <- mean(mydata[which(mydata$weekday == 2), "PM2.5"])
  mean3 <- mean(mydata[which(mydata$weekday == 3), "PM2.5"])
  mean4 <- mean(mydata[which(mydata$weekday == 4), "PM2.5"])
  mean5 <- mean(mydata[which(mydata$weekday == 5), "PM2.5"])
  mean6 <- mean(mydata[which(mydata$weekday == 6), "PM2.5"])
  close.screen(all = TRUE)
  split.screen(figs = c(2, 1))
  split.screen(figs = c(1, 2), screen = 2)
  screen(3)
  par(mar = c(3, 3, 1.3, 0.2))
  par(cex.main = 0.7, cex.sub = 0.7, cex.axis = 0.5, cex.lab = 0.7,
      font.main = 2)
  par(mgp = c(2, 1, 0))
  qboxplot(PM2.5 ~ weekday, data = mydata, probs = c(0.05,
                                                     0.5, 0.95), ylim = c(0, 100), ylab = expression(paste("PM"[2.5],
                                                                                                           mu, "/", "g"^3)), main = expression(paste("Boxplot for PM"[2.5],
                                                                                                                                                     " Measurement by day of week")), col = "lavenderblush",
           whiskcol = "gray86", las = 2, names = c("Sun.", "Mon.",
                                                   "Tues.", "Wed.", "Thurs.", "Fri.", "Sat."), sub = "(factored by days in a week)",
           cex.axis = 0.8)
  points(c(mean0, mean1, mean2, mean3, mean4, mean5, mean6),
         type = "o", col = "skyblue1", lwd = 1.5, pch = 20)
  mydata$hours <- (mydata$Time_1)$h
  mydata$hours <- as.factor(mydata$hours)
  mean00 <- mean(mydata[which(mydata$hours == 0), "PM2.5"])
  mean01 <- mean(mydata[which(mydata$hours == 1), "PM2.5"])
  mean02 <- mean(mydata[which(mydata$hours == 2), "PM2.5"])
  mean03 <- mean(mydata[which(mydata$hours == 3), "PM2.5"])
  mean04 <- mean(mydata[which(mydata$hours == 4), "PM2.5"])
  mean05 <- mean(mydata[which(mydata$hours == 5), "PM2.5"])
  mean06 <- mean(mydata[which(mydata$hours == 6), "PM2.5"])
  mean07 <- mean(mydata[which(mydata$hours == 7), "PM2.5"])
  mean08 <- mean(mydata[which(mydata$hours == 8), "PM2.5"])
  mean09 <- mean(mydata[which(mydata$hours == 9), "PM2.5"])
  mean10 <- mean(mydata[which(mydata$hours == 10), "PM2.5"])
  mean11 <- mean(mydata[which(mydata$hours == 11), "PM2.5"])
  mean12 <- mean(mydata[which(mydata$hours == 12), "PM2.5"])
  mean13 <- mean(mydata[which(mydata$hours == 13), "PM2.5"])
  mean14 <- mean(mydata[which(mydata$hours == 14), "PM2.5"])
  mean15 <- mean(mydata[which(mydata$hours == 15), "PM2.5"])
  mean16 <- mean(mydata[which(mydata$hours == 16), "PM2.5"])
  mean17 <- mean(mydata[which(mydata$hours == 17), "PM2.5"])
  mean18 <- mean(mydata[which(mydata$hours == 18), "PM2.5"])
  mean19 <- mean(mydata[which(mydata$hours == 19), "PM2.5"])
  mean20 <- mean(mydata[which(mydata$hours == 20), "PM2.5"])
  mean21 <- mean(mydata[which(mydata$hours == 21), "PM2.5"])
  mean22 <- mean(mydata[which(mydata$hours == 22), "PM2.5"])
  mean23 <- mean(mydata[which(mydata$hours == 23), "PM2.5"])
  screen(4)
  par(mar = c(3, 3, 1.3, 0.2))
  par(cex.main = 0.7, cex.sub = 0.7, cex.axis = 0.7, cex.lab = 0.7,
      font.main = 2)
  par(mgp = c(2, 1, 0))
  qboxplot(PM2.5 ~ hours, data = mydata, probs = c(0.05, 0.5,
                                                   0.95), ylim = c(0, 100), ylab = expression(paste("PM"[2.5],
                                                                                                    mu, "/", "g"^3)), main = expression(paste("Boxplot for PM"[2.5],
                                                                                                                                              " Measurement by hour of day")), col = "lavenderblush",
           whiskcol = "gray86", las = 1, xlab = "hours", sub = "(factored by hours in a day)")
  points(c(mean00, mean01, mean02, mean03, mean04, mean05,
           mean06, mean07, mean08, mean09, mean10, mean11, mean12,
           mean13, mean14, mean15, mean16, mean17, mean18, mean19,
           mean20, mean21, mean22, mean23), type = "o", col = "skyblue1",
         lwd = 1.5, pch = 20)
  week_hour_matrix <- matrix(nrow = 168, ncol = 5)
  colnames(week_hour_matrix) <- c("name", "weekday", "hour",
                                  "PMmean", "num")
  x <- 1
  for (i in 0:6) {
    a <- as.character(i)
    for (k in 0:23) {
      b <- as.character(k)
      c <- paste0("mean_", a, "_", b)
      assign(c, mean(mydata[which(mydata$weekday == i &
                                    mydata$hours == k), "PM2.5"]))
      d <- c(c, eval(parse(text = paste0("mean_", a, "_",
                                         b))))
      week_hour_matrix[x, 1] <- paste0("mean_", a, "_",
                                       b)
      week_hour_matrix[x, 2] <- i
      week_hour_matrix[x, 3] <- k
      week_hour_matrix[x, 4] <- eval(parse(text = paste0("mean_",
                                                         a, "_", b)))
      week_hour_matrix[x, 5] <- x
      x <- x + 1
    }
  }
  screen(1)
  par(mar = c(3, 4.5, 1.3, 1.3))
  par(cex.main = 0.7, cex.sub = 0.7, cex.axis = 0.7, cex.lab = 0.7,
      font.main = 2)
  plot(week_hour_matrix[, 5], week_hour_matrix[, 4], type = "l",
       xaxt = "n", ylab = "PM2.5 concentration (AQI)", xlab = "day of week",
       main = "PM2.5 concentration by day of the week", col = "orangered",
       lwd = 2)
  axis(1, at = seq(0, 168, 24), labels = FALSE)
  days <- c("Sun.", "Mon.", "Tues.", "Wed.", "Thur.", "Fri.",
            "Sat.")
  loc.days <- seq(13, 157, 24)
  mtext(days, side = 1, line = 1, at = loc.days, cex = 0.7)
  abline(v = seq(0, 168, 24), col = "grey85", lwd = 1.5, lty = 2)
}
