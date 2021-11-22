#' Filter MiSa Dataframe
#'
#' @description
#' Filters the loaded data frame by sites and time
#'
#' @param dataFrame Data frame loaded by a MiSa function (see details)
#' @param site Names of considered sites, written in the site columne of a MiSa
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
filter_data <- function(
  dataFrame,sites = "", tBeg = min(dataFrame$posixDateTime, na.rm = TRUE),
  tEnd = max(dataFrame$posixDateTime, na.rm = TRUE)
){
  if(!("" %in% sites)){
    dataFrame <- dataFrame[dataFrame$site %in% site, ]
  }
  if(tBeg != ""){
    dataFrame <- dataFrame[dataFrame$posixDateTime >= tBeg, ]
  }
  if(tEnd != ""){
    dataFrame <- dataFrame[dataFrame$posixDateTime <= tEnd, ]
  }
  dataFrame
}
