# This is a generic explorer file for the bitcoin exchange data.

lapply(c("data.table", "ggplot2", "dplyr", "gridExtra", 
         "lubridate"), 
       require, character.only=T)

source("import_data.R")
if (!exists("df.1coin")) { # so don't keep reading in data repeatedly
  df.1coin <- import_data('.1coinUSD.csv')
}
if (!exists("df.bitfinex")) {
  df.bitfinex <- import_data('.bitfinexUSD.csv')
}

# Impute w/average between existing previous and next prices, if
# in either one you find you're missing an hour timestamp
source("impute_utils.R")
df.1coin.mini <- impute_average(df.1coin.mini)
df.bitfinex.mini <- impute_average(df.bitfinex.mini)

stop()

# Use only part of each
df.1coin.mini <- df.1coin[df.1coin$datehr < ymd_hms("2014-10-01 00:00:00"),]
df.1coin.mini <- df.1coin.mini[df.1coin.mini$datehr > 
                                 ymd_hms("2014-04-01 00:00:00"),]
df.bitfinex.mini <- df.bitfinex[df.bitfinex$datehr <
                                  ymd_hms("2014-10-01 00:00:00"),]
df.bitfinex.mini <- df.bitfinex.mini[df.bitfinex.mini$datehr >
                                       ymd_hms("2014-04-01 00:00:00"),]

# add hour columns
df.1coin.mini$hr <- hour(df.1coin.mini$datehr)
df.1coin.mini <- df.1coin.mini[,c(1,3,2)]
df.bitfinex.mini$hr <- hour(df.bitfinex.mini$datehr)
df.bitfinex.mini <- df.bitfinex.mini[,c(1,3,2)]

# Start GARCH analysis
require(rmgarch)



