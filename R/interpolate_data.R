#' Linear interpolation for one or more missing values
#'
#' All sections of NA values that are smaller or equal as a defined maximal
#' number of NA's are interpolated
#'
#' @param data_vector Numeric vector of measurements (including NA values)
#' @param max_na the maximal number of NA values in a row to be interpolated
#'
#' @return
#' A list containing the data vector with interpolated NA value as well as an
#' information about the amount of NA's interpolated
#'
#' @export
#'
interpolate_multipleNA <- function(
  data_vector,
  max_na
){
  # find NA data
  nas <- same_inarow(v = is.na(data_vector),
                     NA_treatment = "NA")

  nas <- nas[nas$Value, ]

  # if the first or last is NA, interpolation is not possible
  rfi <- nas[nas$repeats <= max_na &
               nas$starts_at != 1 &
               nas$ends_at != length(data_vector), ]

  if(nrow(rfi) > 0){
    for(i in 1:nrow(rfi)){
      before <- data_vector[(rfi$starts_at[i] - 1)]
      after <- data_vector[(rfi$ends_at[i] + 1)]

      # interpolated values
      new_values <- seq(before, after, length.out = rfi$repeats[i] + 2)
      new_values <- new_values[-c(1, length(new_values))]

      # Replace NAs
      data_vector[rfi$starts_at[i]:rfi$ends_at[i]] <- new_values
    }
  }
  list(data_vector,
       c("NA's interpolated" = sum(rfi$repeats),
         "NA's in total" =  sum(nas$repeats)))
}
