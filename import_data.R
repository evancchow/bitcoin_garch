# Script to import data

lapply(c("data.table", "ggplot2", "dplyr", "gridExtra", 
         "lubridate"), 
       require, character.only=T)

import_data <- function(filename="none_provided") {
  if (!exists("round.time", mode="function")) source("roundtime.R")
  
  # Import data from market of your choice.
  if (filename=="none_provided") {
    filepath <- file.choose() 
  }
  
  df.btc <- as.data.frame(fread(filename))
  colnames(df.btc) <- c("time", "price", "amount")
  # To filter by date:
  # df.btc[df.btc$time < as.POSIXct("2014-02-01 00:00:00"),]
  df.btc$time <- as.POSIXlt(df.btc$time,
                            origin="1970-01-01 00:00:00",
                            tz="America/New_York")
  df.btc$date <- as.Date(df.btc$time)
  df.btc$hr <- round.time(df.btc$time)
  df.btc$datehr <- ymd_hms(sprintf("%s %s:00:00", df.btc$date, df.btc$hr))
  # Group by date / hr and average
  # lookup: R dplyr tutorial, @:
  # http://rpubs.com/justmarkham/dplyr-tutorial
  df.compress <- df.btc[,c("datehr", "price")]
  df.avged <- as.data.frame(df.compress %>% 
                              group_by(datehr) %>% 
                              summarise_each(funs(mean)))
  return(df.avged)
}