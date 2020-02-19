library(data.table)
library(lubridate)

x <- fread("data/H08_20200101_0000_1MWLFbet_FLDK.06001_06001.csv", quote = "'")

head(x)
setnames(x, "#obstime", "obstime")

# Convert observation time to datetime object
x[, obstime := as_datetime(obstime)]

# Fire flag is all 1's -- no additional info
x[, .N, fire]

# Need a threshold for firepower - check quantile values
quantile(x[, firepower], seq(0, 1, .1)) # 90th percentile is 390.62

# Latitude and Longitude for French Island - known fires on January 18/19 2020
# -38.353688, 145.353799

# Calulate distance from hotspots to French Island
x[, dist := sqrt((-38.353688-lat)^2+(145.353799-lon)^2)]

x[order(dist), .(dist, firepower, obstime, lat, lon)]
