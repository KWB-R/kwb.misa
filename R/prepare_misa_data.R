#' Prepare MiSa Data for MiSa Assessment
#'
#' Timestamps are adapted, oxygen data is interpolated, it is filtered for
#' summmer months
#'
#' @param df_MiSa Data frame loaded with one of the MiSa Load functions
#' @param res Temporal resolution in minutes
#' @param max_na_interpolation Maximal numbers of NA values in a row to be
#' interpolated
#'
#' @return
#' List with data frames per site, that is ready for MiSa Assessment
#'
#' @export
#'
prepare_misa_data <- function(
  df_MiSa, res = 15, max_na_interpolation = 4
){
  dl <- split(x = data_comp_f, f = data_comp_f$site)

  dl <- lapply(dl, function(df){
    site <- df$site[1]
    df_pro <- continuousTimeIntervals(
      time_vector = df$posixDateTime,
      data_vector = df$oxygen,
      res = res)

    interpolated_data <- interpolate_multipleNA(
      data_vector = df_pro$d,
      max_na = max_na_interpolation)
    df_pro$d <- interpolated_data[[1]]

    df_pro$site <- site
    print(site)
    print(interpolated_data[[2]])

    SummerMonths(df = df_pro, time_column = "t")
  })
}
