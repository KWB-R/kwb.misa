#' Linear interpolation for one or more missing values
#'
#' All sections of NA values that are smaller or equal as a defined maximal
#' number of NA's are interpolated
#'
#' @param data_vector Numeric vector of measurements (including NA values)
#' @param max_na the maximal number of NA values in a row to be interpolated
#' @param diff_x (optional) Numeric vector with x value difference correspoding
#' to the data_vector. Only needed if the difference is not constant.
#'
#' @return
#' A list containing the data vector with interpolated NA value as well as an
#' information about the amount of NA's interpolated
#'
#' @export
#'
interpolate_multipleNA <- function(
  data_vector,
  max_na,
  diff_x = NULL
){
  # find NA data
  nas <- same_inarow(v = is.na(data_vector))

  nas <- nas[nas$Value, ]

  # if the first or last is NA, interpolation is not possible
  rfi <- nas[nas$repeats <= max_na &
               nas$starts_at != 1 &
               nas$ends_at != length(data_vector), ]

  if(nrow(rfi) > 0){
    for(i in 1:nrow(rfi)){
      beg_i <- rfi$starts_at[i] - 1
      end_i <- rfi$ends_at[i] + 1
      before <- data_vector[beg_i]
      after <- data_vector[end_i]

      new_values <-  if(is.null(diff_x)){
        # interpolated values
        seq(before, aftder, length.out = rfi$repeats[i] + 2)

      } else {
        x <- cumsum(diff_x[beg_i:end_i])
        new_values <- (x - min(x)) / diff(range(x)) * (before - after) + after
      }

      new_values <- new_values[-c(1, length(new_values))]


      # Replace NAs
      data_vector[rfi$starts_at[i]:rfi$ends_at[i]] <- new_values
    }
  }
  list(data_vector,
       c("NA's interpolated" = sum(rfi$repeats),
         "NA's in total" =  sum(nas$repeats)))
}
