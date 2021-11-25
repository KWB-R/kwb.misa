#' Repeating values in a row within a Vector
#'
#' Describes the Values of a vector the times they are repeated and the
#' start and end position of those values
#'
#' @param v A character, factor or numeric vector
#' @param NA_treatment How to treat NA values within the vector. Defaul is NA,
#' It can also be "0" (FALSE) or "1" (TRUE)
#'
#' @return
#' A data frame with four columns: Value (-> listed value of the input vector),
#' Repeats (times it is repeated in a row), starts_at (start position),
#' ends_at (end position).
#'
#' @export
#'
same_inarow <- function(
  v,
  NA_treatment = "NA"
){
  v_extended <- as.character(c(v, 0))
  v_extended[is.na(v_extended)] <- NA_treatment

  # supporting function --------------------------------------------------------
  count_repeats <- function(
    v, # Vector
    beg
  ){
    n <- 0
    for(i in beg:length(v)){
      if(v[i] == v[beg]){
        n <- n + 1
      } else {
        break
      }
    }
    data.frame("Value" = v[beg], "repeats" = n,
               "starts_at" = beg, "ends_at" = i - 1)
  }
  # ----------------------------------------------------------------------------

  rep_list <- list()
  beg <- 1
  list_entry <- 1
  while(beg < length(v_extended)){
    rep_list[[list_entry]] <- count_repeats(v = v_extended, beg = beg)
    beg <- rep_list[[list_entry]]$ends_at + 1
    list_entry <- list_entry + 1
  }

  finalFrame <- do.call(rbind, rep_list)
  finalFrame$Value <- as.character(finalFrame$Value)
  finalFrame
}
