#' MiSa Assessment: Yearly hours of deficits
#'
#' Counting the hours on a yearly basis below threshold values
#'
#' @param dataFrame MiSa Dataframe: with columne "d": data, "year": year
#' @param res Temporal resolution of oxygen data in minutes
#' @param thresholds Oxygen threshold values used for the assessment in mg/L
#' @param max_missing The maximal allowed percent of missing oxygen data. If NA
#' Values exceed this number, hours below thresholds are set to NA
#'
#' @return Data frane with rows per year and columns per threshold as well es
#' for missing data
#'
#' @export
#'
yearly_deficiency_time <- function(
  dataFrame, # df with oxygen data in 15 min intervals and with year column
  res = 15, # minutes
  thresholds = c(0.5, 1, 1.5, 2, 5),
  max_missing = 25 # in % of all potential measurements
){

  years <- min(dataFrame$year):max(dataFrame$year)

  group_list <- lapply(years, function(x)
  {dataFrame$d[dataFrame$year == x]})

  group_df <- as.data.frame(t(sapply(group_list, function(x){
    sapply(thresholds, count_def_hours, data_vector = x, res = res)
  })))

  group_df$unknown <- sapply(group_list, function(x) {
    round(sum(is.na(x)) / length(x) * 100, 0)
  })

  group_df[group_df$unknown > max_missing,
           -which(colnames(group_df) =="unknown")] <- NA

  group_df$year <- years
  rownames(group_df) <- NULL
  colnames(group_df) <- c(paste0("below_", thresholds), "missing_data", "year")

  group_df
}


#' MiSa Assessment: Yearly Numbers of deficits
#'
#' Counting the events below threshold values on a yearly basis
#'
#' @param dataFrame MiSa Dataframe: with columne "d": oxygen data, "year": year
#' @param res Temporal resolution of oxygen data in minutes
#' @param seperating_hours TODO: describe (also: should be "separating_hours"
#'   with "a", not "e")
#' @param deficiency_hours TODO: describe
#' @param thresholds Oxygen threshold values used for the assessment in mg/L
#' @param max_missing The maximal allowed percent of missing oxygen data. If NA
#'   Values exceed this number, hours below thresholds are set to NA
#' @param use_recovery_value TODO: describe
#' @param recovery_value TODO: describe
#'
#' @return Data frane with rows per year and columns per threshold as well es
#' for missing data
#'
#' @export
#'
yearly_crit_Events <- function(
  dataFrame, # df with oxygen data in 15 min intervals and with year column
  res = 15,
  seperating_hours = 5 * 24,
  deficiency_hours = 0.25,
  thresholds = 1.5,
  max_missing = 25, # in % of all potential measurements
  use_recovery_value = FALSE,
  recovery_value = NULL){

  years <- min(dataFrame$year):max(dataFrame$year)

  group_list <- lapply(years, function(x)
  {dataFrame$d[dataFrame$year == x]})

  group_df <- as.data.frame(t(sapply(group_list, function(x){
    sapply(thresholds, count_def_events,
           data_vector = x,
           starting_data_points = round(1 + deficiency_hours / (res / 60), 0),
           separating_data_points = seperating_hours / (res / 60))
    })))

  if(nrow(group_df) == 1){
    group_df <- as.data.frame(t(group_df))
  }

  group_df$unknown <- sapply(group_list, function(x) {
    round(sum(is.na(x))/length(x) * 100, 0)
  })

  group_df[group_df$unknown > max_missing,
           -which(colnames(group_df) == "unknown")] <- NA

  group_df$year <- years
  rownames(group_df) <- NULL
  colnames(group_df) <- c(paste0("below_", thresholds), "missing_data", "year")
  group_df
}

#' Negative deviation from a reference site
#'
#' Functions cumulates the negative deviation (lower O2-Concentrations) compared
#' to a reference site without (significant) urban pollution
#'
#' @param dataFrame MiSa Dataframe: with columne "d": oxygen data, "year": year
#' @param oxygen_ref The course of oxygen of the reference
#' @param max_missing The maximal allowed percent of missing oxygen data. If NA
#' Values exceed this number, hours below thresholds are set to NA
#'
#' @return
#' Data frane with negative deviation per year
#'
#' @export
yearly_negative_deviation <- function(
  dataFrame, # df with oxygen data in 15 min intervals and with year column
  oxygen_ref,
  max_missing = 25
){
  years <- min(dataFrame$year):max(dataFrame$year)

  # add MUE oxygen to data frame
  dataFrame$oxygen_ref <- oxygen_ref

  # per site data frames with oxygen in MUE and its difference
  group_list <- lapply(years, function(x){
    data.frame("ox_ref" = dataFrame$oxygen_ref[dataFrame$year == x],
               "ox" = dataFrame$d[dataFrame$year == x])})

  group_df <- as.data.frame(sapply(group_list, function(x){
    negative_deviation(data_vector = x$ox, reference_vector = x$ox_ref)
  }))

  group_df$unknown <- sapply(group_list, function(x) {
    round(sum(is.na(x[["ox_ref"]]) | is.na(x[, "ox"])) /
                       nrow(x) * 100,
                     digits = 0)
  })

  group_df[group_df$unknown > max_missing,
           -which(colnames(group_df) == "unknown")] <- NA

  group_df$year <- years
  rownames(group_df) <- NULL
  colnames(group_df) <- c("neg_deviation_relative", "missing_data", "year")

  group_df
}



