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
if (!exists("df.lake")) {
  df.lake <- import_data('.lakeUSD.csv')
}

# Impute w/average between existing previous and next prices, if
# in either one you find you're missing an hour timestamp
source("impute_utils.R")
df.1coin.imputed <- impute_average(df.1coin)
df.bitfinex.imputed <- impute_average(df.bitfinex)
df.lake.imputed <- impute_average(df.lake)

# Use only part of each
df.1coin.mini <- df.1coin.imputed[df.1coin.imputed$datehr < ymd_hms("2014-10-01 00:00:00"),]
df.1coin.mini <- df.1coin.mini[df.1coin.mini$datehr > 
                                 ymd_hms("2014-04-01 00:00:00"),]
df.bitfinex.mini <- df.bitfinex.imputed[df.bitfinex.imputed$datehr <
                                  ymd_hms("2014-10-01 00:00:00"),]
df.bitfinex.mini <- df.bitfinex.mini[df.bitfinex.mini$datehr >
                                       ymd_hms("2014-04-01 00:00:00"),]
df.lake.mini <- df.lake.imputed[df.lake.imputed$datehr < ymd_hms("2014-10-01 00:00:00"),]
df.lake.mini <- df.lake.mini[df.lake.mini$datehr >
                               ymd_hms("2014-04-01 00:00:00"),]

# add hour columns
df.1coin.mini$hr <- hour(df.1coin.mini$datehr)
df.1coin.mini <- df.1coin.mini[,c(1,3,2)]
df.bitfinex.mini$hr <- hour(df.bitfinex.mini$datehr)
df.bitfinex.mini <- df.bitfinex.mini[,c(1,3,2)]
df.lake.mini$hr <- hour(df.lake.mini$datehr)
df.lake.mini <- df.lake.mini[,c(1,3,2)]

# differencing removes the trend which is good. however, you do
# lose one date at the beginning, so remove that one.
date.labels <- df.1coin.mini$datehr[2:nrow(df.1coin.mini)]
diff.1coin <- diff(df.1coin.mini$price)
diff.bitfinex <- diff(df.bitfinex.mini$price)
diff.lake <- diff(df.lake.mini$price)

# # plot!
# plt.1coin.data <- diff.1coin[1:50]
# plt.bitfinex.data <- diff.bitfinex[1:50]
# plt.lake.data <- diff.lake[1:50]
# 
# plt.1coin <- qplot(1:length(plt.1coin.data), plt.1coin.data, geom="line") +
#   ggtitle("1coin") +
#   ylab("Bitcoin price in USD")
# plt.bitfinex <- qplot(1:length(plt.bitfinex.data), plt.bitfinex.data,
#                       geom="line") + 
#   ggtitle("Bitfinex") +
#   ylab("Bitcoin price in USD")
# plt.lake <- qplot(1:length(plt.lake.data), plt.lake.data, geom="line") +
#   ggtitle("Lake") +
#   ylab("Bitcoin price in USD")
# 
# grid.arrange(plt.1coin, plt.bitfinex, plt.lake)

# Start GARCH analysis
# Remember you need a first difference to remove the trend (mean) which
# is similar for all coins, and you may need to use a second difference too.
require(rmgarch)
cluster <- NULL

# load example data
print("Fitting DCC to the example data ...")
data(dji30retw)
dat = dji30retw[, 1:3, drop = FALSE]
uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(3,1), model = "sGARCH"), 
                   distribution.model = "norm")
spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1),  distribution = "mvnorm")
fit1 = dccfit(spec1, data = dat, fit.control = list(eval.se=FALSE))
print("Done fitting DCC to the example data!")

# Create a similar dataframe (date-indexed) for our bitcoin
# USD price time series (price differences from interval to interval)
df.diff <- data.frame(
  onecoin=diff.1coin,
  bitfinex=diff.bitfinex,
  lake=diff.lake
  )
row.names(df.diff) <- date.labels

print("Fitting DCC to bitcoin data ...")
uspec.btc = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(3,1), model = "sGARCH"), 
                   distribution.model = "norm")
spec.btc = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1),  distribution = "mvnorm")
fit.btc <- dccfit(spec.btc, data=df.diff, fit.control=list(eval.se=FALSE))
print("Done fitting DCC to bitcoin data!")
# yay this works!
# now for DCC just figure out how to get the data.
# useful link w/matlab code:
# https://sites.google.com/site/garthmortensenthesis/









