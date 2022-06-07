#' Force data into predefined time intervals
#'
#' The measurements are fitted into timesteps defined be the first point of
#' time and a temporal resolution
#'
#' @param time_vector A POSIXct vector
#' @param data_vector A numeric vector, with data corresponding to the time_vector
#' @param res Temporal resolution in minutes
#' @param first_pointOfTime Starting point (POSIXct) of the newly defined time series.
#' By default the minimum of the time_vector
#' @param last_pointOfTime End point (POSIXct) of the newly defined time series.
#' By default the maximum of the time_vector
#'
#' @details In a first step a vactor is generated with continuous timesteps,
#' starting at first_pointOfTime by a defined time interval. Subsequently, the
#' measured data is forced into timesteps with a similar time interval. Here,
#' the measurements are assigned to the timestep that is closest to the actual
#' time of measurements. If more than one measurement are assigned to one
#' timestep, the average is used. If there is no measurement, NA is used.
#'
#' @return Dataframe with POSIX column "t" and data column "d"
#'
#' @export
#'
continuousTimeIntervals <- function(
  time_vector,
  data_vector,
  res = 15,
  first_pointOfTime = min(time_vector, na.rm = TRUE),
  last_pointOfTime = max(time_vector, na.rm = TRUE)
){
  new_timestamps <- seq(from = first_pointOfTime,
                        to = last_pointOfTime,
                        by = 60 * res)

  df_out <- aggregate_measurements(time_vector = time_vector,
                                   data_vector = data_vector,
                                   time_interval = 60 * res)

  # merging by the complete new_timestamps vector (missing values --> NA)
  merge(x = data.frame("t" = new_timestamps),
        y = df_out,
        by = "t",
        all.x = TRUE)
}

#' Average data per predefined timesteps
#'
#' Creates a data frame with fitted time column and corresponding average
#' data values.
#'
#' @param time_vector A POSIXct vector
#' @param data_vector A numeric vector, with data corresponding to the time_vector
#' @param time_interval Temporal resolution in seconds
#'
#' @return
#' Data frame with POSIX time column "t" and data column "d
#' @export
#'
aggregate_measurements <- function(
  time_vector,
  data_vector,
  time_interval = 60 * 15){

  new_time <- adjust_time(time_vector = time_vector,
                          time_interval = time_interval)

  # find all duplicated time steps --> used to get average parameter values
  forward <- which(duplicated(new_time, fromLast = FALSE))
  backward <- which(duplicated(new_time, fromLast = TRUE))
  v <- sort(unique(x = c(forward, backward)))
  if(length(v) == 0){
    # all the unique values --> no action necessary
    looking_good <- data.frame("t" = new_time, "d" = data_vector)
  } else{
    looking_good <- data.frame("t" = new_time, "d" = data_vector)[-v, ]
    # need some preparation
    # duplicated values --> get average values
    timesteps <- unique(new_time[v])
    prepared <- data.frame(
      "t" = timesteps,
      "d" = sapply(timesteps, function(t){
        mean(data_vector[new_time == t], na.rm = TRUE)
      })
    )
    # rbind and sort
    looking_good <- rbind(looking_good, prepared)
  }
  looking_good[order(looking_good[["t"]]), ]
}

# Version by Hauke Sonnenberg:
# using existing function hsGroupByInterval() in kwb.base
aggregate_measurements_hauke <- function(
  time_vector, data_vector, time_interval
)
{
  # Create a data frame as required by hsGroupByInterval()
  data <- data.frame(t = time_vector, d = data_vector)

  # My function seems to require that the data frame is sorted by time!
  data <- kwb.utils::orderBy(data, "t")

  kwb.base::hsGroupByInterval(
    data,
    interval = time_interval,
    FUN = mean,
    # Use the first timestamps of the time intervals in the output
    offset2 = 0L
  )
}

#' Equal time intervals
#'
#' Transforms the time vector, so that the every timestep is shifted to equal
#' equally distanced points of time
#'
#' @param time_vector A POSIXct vector
#' @param time_interval Temporal resolution in seconds
#'
#' @return A POSIX vector with equally distanced points of time. The new
#' timesteps start on the hour and
#'
#' @export
#'
adjust_time <- function(time_vector, time_interval)
{
  as.POSIXct(
    round(as.numeric(time_vector) / time_interval) * time_interval,
    origin = "1970-01-01"
  )
}
