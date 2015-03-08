# Some utility functions to take in a time series and impute stuff as needed,
# say missing values.

# Helper function to insert a new row into a dataframe at index r
# (shift row at index r down one to make room). From SO:
# http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)
}

impute_average <- function(df.btc) {
  
  # loop over, imputing where necessary
  hour.vals <- c(0, 3, 6, 9, 12, 15, 18, 21, 24)
  
  i <- 1
  while (i < nrow(df.btc)) {
    curr.row <- df.btc[i,]
    next.row <- df.btc[i+1,]
    hr.diff <- as.numeric(difftime(next.row$datehr,
                                   curr.row$datehr,
                                   units="hours"))
    if (hr.diff > 3) {
#     if ((hours(next.row$datehr) - hours(curr.row$datehr)) > 3) {
      start.row <- curr.row
      curr.time <- start.row$datehr
      curr.price <- start.row$price
      total.price.diff <- next.row$price - curr.row$price
      
      # find out how many rows ahead to add
      # This is equal to the (hour difference between 
      # curr and next) / 3 - 1
      numNewRows <- ((hr.diff) / 3) - 1
      
      # This means you want to add ((next.price - curr.price) / 3) to
      # each new row
      for (j in 1:numNewRows) {
        # increment new time and price
        curr.time <- curr.time + hours(3)
        curr.price <- curr.price + (total.price.diff) / (hr.diff / 3)
        
        # create a new row and insert it
        new.row <- data.frame(
          datehr=curr.time, 
          price=curr.price)
        df.btc <- insertRow(df.btc, new.row, i+j)
      }
      i <- i + j
    } else {
      i <- i + 1
    }
  }
  
  return(df.btc)
}