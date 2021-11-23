#' Prepare MiSa Data for MiSa Assessment
#'
#' Timestamps are adapted, oxygen data is interpolated, it is filtered for
#' summmer months
#'
#' @param df_MiSa Data frame loaded with one of the MiSa Load functions
#' @param res Temporal resolution in minutes
#' @param max_na_interpolation Maximal numbers of NA values in a row to be
#' interpolated. The default is one hour without measurements. Number of NA
#' depneds on the temporal resolution (60 / res)
#'
#' @return
#' List with data frames per site, that is ready for MiSa Assessmen. Additional
#' information is printed about the number of interpolated NA values. If there
#' are many NA values that are not interpolated it is probably due to the fact
#' of no measurements during winter.
#'
#' @export
#'
misa_prepare_data <- function(
  df_MiSa, res = 15, max_na_interpolation = 60/res
){
  dl <- split(x = data_comp_f, f = data_comp_f$site)

  dl <- lapply(dl, function(df){
    site <- df$site[1]
    df_pro <- continuousTimeIntervals(
      time_vector = df$posixDateTime,
      data_vector = df$oxygen,
      first_pointOfTime = min(df_MiSa$posixDateTime, na.rm = T),
      last_pointOfTime = max(df_MiSa$posixDateTime, na.rm = T),
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
