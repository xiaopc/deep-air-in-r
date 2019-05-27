library(readr)
library(tidyr)
library(reshape2)
library(zoo)

startdate = strptime("2016-01-02", format = "%Y-%m-%d") # + 1 day
enddate = strptime("2017-01-01", format = "%Y-%m-%d")   # + 1 day
path = "data/cities/china_cities_%s.csv"

df.total = data.frame()

load.data <- function (df, dat) {
  df$hour <- sprintf("%02d", df$hour)
  df <- unite(df, "datetime", date, hour, sep = "")
  df <- melt(df, id.vars = c('datetime','type'))
  names(df)[3] = "city"
  df <- dcast(df, datetime + city ~ type)
  df <- df[, c("datetime", "city", "CO", "NO2", "O3", "PM2.5", "SO2")]
}

for (d in seq(startdate, enddate, by = "day")) {
  dat <- as.Date.POSIXct(d)
  df <- read_csv(sprintf(path, as.character.Date(dat, format = "%Y%m%d")))
  df.total <- rbind(df.total, load.data(df, dat))
}

df.total$datetime <- as.POSIXct(strptime(df.total$datetime, format = "%Y%m%d%H"))
time.list <- data.frame(datetime = seq(as.POSIXct(startdate - 86400), as.POSIXct(enddate - 1), by = "hour"))
time.site.list <- merge(time.list, unique(df.total$city), all = TRUE)
names(time.site.list)[2] <- "city"
time.site.list$city <- as.character(time.site.list$city)
df.total.all <- merge(time.site.list, df.total, all.x = TRUE, by.x = c("datetime", "city"), by.y = c("datetime", "city"))

write.csv(df.total.all, "city-2016.csv", row.names = FALSE)

df.total.all.split <- split(df.total.all, df.total.all$city)
df.single <- df.total.all.split$成都[, c("datetime", "CO", "NO2", "O3", "PM2.5", "SO2")]
write.csv(df.single, "city-2016-chengdu.csv", row.names = FALSE)
