#' Count Events of deficits
#'
#' Counts the Number of intervals where x number of data points in a row are below
#' a predifined threshold value. Events are separated by a specified
#' number of data points above that threshod value. Furthermore, the exceedance
#' of a value can also seperation critirion.
#'
#' @param data_vector Numeric vector (with data in the same unit as the tjreshold)
#' @param starting_data_points Minimal number of data points to define the
#' beginning of an deficiency event
#' @param threshold Numeric in the same unit as the data vector
#' @param separating_data_points Minimal number of data points to seperate two
#' events
#' @param use_recovery_value If TRUE a recovery, two events are only separated
#' if a revocvery value is exceeded between two deficits
#' @param recovery_value Numeric in the same unit as the data vector. Only used
#' if use_recovery_value = TRUE.
#' @param return_event_positions Instead the number of events, the events
#' starting and endpositions are returned, correspoding to the data vector
#'
#' @return Either a number of events or a data frame with event start and end
#' position
#'
#' @export
#'
#' @examples
#' data_vector <- sin(x = seq(0,50,0.5)) * 1:101/20
#'
#' a <- count_def_events(
#' data_vector = data_vector,
#' starting_data_points = 2,
#' threshold = 0,
#' separating_data_points = 4,
#' use_recovery_value = FALSE,
#' recovery_value = 7,
#' return_event_positions = TRUE)
#'
#' plot(data_vector, pch = 20, type = "b")
#' rect(xleft = a$tBeg, xright = a$tEnd, ybottom = -10, ytop = 10,
#'  col = "red", density = 4)
#'
#' recovery_value <- 3
#' a <- count_def_events(
#' data_vector = data_vector,
#' starting_data_points = 2,
#' threshold = 0,
#' separating_data_points = 4,
#' use_recovery_value = TRUE,
#' recovery_value = recovery_value,
#' return_event_positions = TRUE)
#'
#' plot(data_vector, pch = 20, type = "b")
#' rect(xleft = a$tBeg[a$start],
#'  xright = a$tEnd[a$end],
#'  ybottom = -10, ytop = 10,
#'  col = "red", density = 4)
#'  abline(h = recovery_value, col = "blue")
#'
#'
count_def_events <- function(
  data_vector,
  starting_data_points,
  threshold,
  separating_data_points,
  use_recovery_value = FALSE,
  recovery_value = NULL,
  return_event_positions = FALSE
){
  possible_starts <- which(data_vector < threshold)

  # criteria 1: the following value needs to be below threshold, too
  following <- sapply(1:(starting_data_points - 1), function(i){
    (data_vector[possible_starts + i] < threshold)
  })

  events <- if(length(unlist(following))){
    if(sum(unlist(following), na.rm = T)){
      possible_starts[apply(following, MARGIN = 1, all)]
    }
  }


  if(length(events) > 0){
    # criteria 2: events within a defined seperation step are aggregated as one
    event_end <- c(which(diff(events) > separating_data_points))
    event_start <- c(1, event_end + 1)
    event_end <- c(event_end, length(events))
    a <- data.frame("tBeg" = events[event_start], "tEnd" = events[event_end])

    if(use_recovery_value){
      # possible criteria 3: minimum reached O2 concentration between events
      separated <- c(TRUE)
      if(nrow(a) > 1){
        for(r in 2:nrow(a)){
          separated <- c(
            separated,
            sum(
              data_vector[a$tEnd[r - 1]:a$tBeg[r]] >=
                recovery_value,
              na.rm = TRUE) > 0)
        }
      }
      a$start <- separated
      a$end <- FALSE
      a$end[c((which(a$start) - 1)[2:sum(a$start)], nrow(a))] <- T
      if(return_event_positions){
        return(a)
      }  else {
        return(sum(a$event))
      }
    } else {
      if(return_event_positions){
        return(a)
      } else {
        return(length(event_end))
      }
    }
  } else {0}
}


#' Count hours of deficits
#'
#'
#' @param data_vector Numeric vector (with data in the same unit as the threshold)
#' @param threshold Numeric in the same unit as the data vector
#' @param res Temporal resolution of data in minutes
#'
#' @return
#' A single Value (hours of deficits)
#'
#' @export
#'
count_def_hours <- function(
  data_vector,
  threshold,
  res){
  sum(data_vector <= threshold, na.rm = TRUE) / (60 / res)
}

#' Relative Negative Deviation from a reference
#'
#' The cumulative sum of all negative deviations.
#'
#' @param data_vector Numeric data vector
#' @param reference_vector Corresponding data of the reference
#'
#' @details
#' First the similarity the data vector and the reference vector is calculated.
#' Only complete pairs (no NA values) are used. For each data pair the
#' quotient between data and reference is calcutaled. If data > reference the
#' value is set to 1. All quotients  are cumulated (-> absolute similarity).
#' This can be maximum the number of data pairs. When deviding by the number of
#' data pairs, the relative similarity is obtained. One minus the relative
#' similarity is the negative deviation.
#'
#' @return
#' Numeric value between 0 and 1
#'
#'
#'
negative_deviation <- function(
  data_vector,
  reference_vector
){

  def <- which((data_vector - reference_vector) <= 0)
  # available data
  av_data <- sum(!is.na(data_vector) & !is.na(reference_vector))

  # similarity to reference (--> Maximal possible score  = 1)
  res <- (sum((data_vector/ reference_vector)[def])  + # resilience of values below MUE
            (av_data - length(def))) / # res = 1 for every value above MUE
    av_data # devided by the amount of measurements (that are not NA)
  # 1 - similarity = negative deviation
  round((1 - res), 2)
}


