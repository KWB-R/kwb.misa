#' Filter MiSa Dataframe
#'
#' @description
#' Filters the loaded data frame by sites and time
#'
#' @param dataFrame Data frame loaded by a MiSa function (see details)
#' @param sites Names of considered sites, written in the site column of a MiSa
#' Dataframe
#' @param tBeg POSIX-Value with a start time of the observeration interval
#' @param tEnd POSIX-Value with an end time of the observeration interval
#'
#' @details
#' The name of the site column must be "site", the name of the timestamp column
#' must be "posixDateTime".
#' The best way is to load the oxygen data  with one of the following
#' functions: [read_misa_oneSite()],[read_misa_multipleSites()] or
#' [read_misa_files()].
#'
#' @return
#' A filtered data frame with the same columns as the input data frame
#'
#' @export
#'
misa_filter_data <- function(
  dataFrame, sites = "", tBeg = min(dataFrame$posixDateTime, na.rm = TRUE),
  tEnd = max(dataFrame$posixDateTime, na.rm = TRUE)
){
  if(!("" %in% sites)){
    dataFrame <- dataFrame[dataFrame$site %in% sites, ]
  }
  if(tBeg != ""){
    dataFrame <- dataFrame[dataFrame$posixDateTime >= tBeg, ]
    first_row <- data.frame(matrix(data = NA, nrow = 1, ncol = ncol(dataFrame),
                                   dimnames = list(NULL, colnames(dataFrame))))
    first_row$posixDateTime <- as.POSIXct(tBeg)
    dataFrame <- rbind(first_row, dataFrame)
  }
  if(tEnd != ""){
    dataFrame <- dataFrame[dataFrame$posixDateTime <= tEnd, ]
    last_row <- data.frame(matrix(data = NA, nrow = 1, ncol = ncol(dataFrame),
                                   dimnames = list(NULL, colnames(dataFrame))))
    last_row$posixDateTime <- as.POSIXct(tEnd)
    dataFrame <- rbind(dataFrame, last_row)
  }
  dataFrame
}

#' Breaking down POSIX into months and year and filter by month
#'
#' Adds month and year column to data frame and filters by month. The addition
#' of the year column is important for the following MiSa Assessment
#'
#' @param df data frame with a POSIX time column
#' @param time_column Name or number of the time column
#' @param months the number of the months that should be kept. The default is
#' 5:9 which are the important months for oxygen deficits in Berlin caused by
#' CSOs
#'
#' @return The filteterd input data frame with months and year column
#'
#' @export
#'
SummerMonths <- function(
  df, # list with sites
  time_column = "t",
  months = 5:9
){
    df$month <- as.numeric(strftime(x = df[[time_column]],format = "%m"))
    df$year <- as.numeric(strftime(x = df[[time_column]],format = "%Y"))
    df[df$month %in% months, ]
}
