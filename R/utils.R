#' Repeating values in a row within a Vector
#'
#' Describes the Values of a vector the times they are repeated and the
#' start and end position of those values
#'
#' @param v A character, factor or numeric vector
#'
#' @return
#' A data frame with four columns: Value (-> listed value of the input vector),
#' Repeats (times it is repeated in a row), starts_at (start position),
#' ends_at (end position).
#'
#' @export
#'
#'@importFrom kwb.utils findChanges
#'
same_inarow <- function(v){

  stopifnot(!anyNA(v))

  result <- kwb.utils::findChanges(v)

  data.frame(
    Value = result$value,
    repeats = result$ends_at - result$starts_at + 1L,
    starts_at = result$starts_at,
    ends_at = result$ends_at
  )
}
