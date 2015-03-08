# A script with some function(s) to round data to the nearest time points,
# for example, every few hours. This is for dealing with irregular
# time series.

# Given a specific time, round it to the nearest 1/8 of a day
round.time <- function(time) {
  # IMPORTANT NOTE: you can apply this directly to a vector 
  # of POSIXlt things, and you will find
  # it will round all the values for you.
  # Milestones are at 0, 3, 6, 9, 12, 15, 18, 21 hours
  vect <- (3 * round(hour(time) / 3))
  # do NOT convert 24 to 0 because then it resets to beginning of day,
  # not the start of the next one.
  return(vect)
}
