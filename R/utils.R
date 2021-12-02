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
  t1 <- system.time(result_1 <- same_inarow_v1(v, NA_treatment))
  t2 <- system.time(result_2 <- same_inarow_v2(v, NA_treatment))

  cat("Runtime same_inarow_v1:\n")
  print(t1)

  cat("Runtime same_inarow_v2:\n")
  print(t2)

  if (! identical(result_1, result_2)) {
    warning("same_inarow_v1() and same_inarow_v2() return different results!")
  }

  result_1
}

same_inarow_v1 <- function(v, NA_treatment = "NA"){
  v_extended <- as.character(c(v, 0))
  v_extended[is.na(v_extended)] <- NA_treatment

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

#' @importFrom kwb.utils findChanges
same_inarow_v2 <- function(v, NA_treatment = "NA"){

  v[is.na(v)] <- NA_treatment

  result <- kwb.utils::findChanges(as.character(v))

  data.frame(
    Value = result$value,
    repeats = result$ends_at - result$starts_at + 1,
    starts_at = as.numeric(result$starts_at),
    ends_at = as.numeric(result$ends_at)
  )
}

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
  data.frame(
    Value = v[beg],
    repeats = n,
    starts_at = beg,
    # Not correct but required by same_inarow_v1(), I assume
    ends_at = i - 1
    # This is correct, I think
    #ends_at = beg + n - 1L
  )
}
