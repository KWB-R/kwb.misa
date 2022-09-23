#' Read Qsim output and save in required format for misa assessment tool
#'
#' This function combines the Qsim site information of the first 3 columns
#' to unique sitenames and spreads the Qsim output table into a table with one
#' time column and several oxygen columns (one per site)
#'
#' @param qsim_output_path path where the output file from Gsim is stored
#' @param qsim_fileName Filename of the Qsim output file (inclusive .csv)
#' @param misa_tool_input_path input Folder for Misa Tool (see details)
#' @param output_fileName Filename of the created table (including.csv)
#' @param return_table if TRUE, the saved table is returned
#'
#' @details
#' The Misa Tool input folder as defined in parameter 'misa_tool_input_path' must
#' contain the two folder "files_per_site" and "site_per_file". The output
#' table will be stored in the latter.
#' Saves table as CSV File ";"-separated if parameter return_table is set to TRUE
#'
#' @return
#' The saved table contains a timestamp column in Central European Time and
#' several oxygen columns in mg/L
#'
#' @export
#' @importFrom utils read.table write.table
#' @importFrom tidyr spread
#' @importFrom rlang .data
QSIM_prepare_for_tool <-function(
  qsim_output_path,
  qsim_fileName,
  misa_tool_input_path,
  output_fileName,
  return_table = FALSE
){

  df_in <- read.table(file = file.path(qsim_output_path, qsim_fileName),
                      header = T, sep = ";", dec = ",")

  df_in$site <- paste(df_in[[1]], df_in[[2]], df_in[[3]], sep = "_")

  df_out <- df_in[,c("Datum", "site", "VO2")]

  del <- which(duplicated(df_out[,1:2]))
  if(length(del)){
    df_out <- df_out[-del,]
  }

  df_out <- df_out %>% tidyr::spread(.data$site, .data$VO2)
  df_out$Datum <- as.POSIXct(df_out$Datum, format = "%d.%m.%Y %H:%M")
  df_out <- df_out[order(df_out$Datum),]

  utils::write.table(x = df_out,
                     file = file.path(misa_tool_input_path,
                                      "sites_per_file",
                                      output_fileName),
                     sep = ";",
                     dec = ".",
                     row.names = F)
}


#' Read Qsim output and saves the flow at designated river sites
#'
#' @param qsim_output_path path where the output file from Gsim is stored
#' @param qsim_fileName Filename of the Qsim output file (including .csv)
#' @param save_path path where flow table is saved
#'
#' @export
#' @importFrom utils read.table write.table
#' @importFrom tidyr spread
#' @importFrom rlang .data
QSIM_get_flow <-function(
    qsim_output_path,
    qsim_fileName,
    save_path
){

  df_in <- read.table(file = file.path(qsim_output_path, qsim_fileName),
                      header = T, sep = ";", dec = ",")

  df_in$site <- paste(df_in[[1]], df_in[[2]], df_in[[3]], sep = "_")

  df_out <- df_in[,c("Datum", "site", "Q")]

  del <- which(duplicated(df_out[,1:2]))
  if(length(del)){
    df_out <- df_out[-del,]
  }

  df_out <- df_out %>% tidyr::spread(.data$site, .data$Q)
  df_out$Datum <- as.POSIXct(df_out$Datum, format = "%d.%m.%Y %H:%M")
  df_out <- df_out[order(df_out$Datum),]

  flowSite <- c("Panke" = "Pa_Panke-1_0.126", "Spree1" = "SOW_S501-SOW_6.4",
                "Spree0" = "SOW_S106-SOW_18.1", "LWK" = "Lwk_S203-Lwk_1.67",
                "Havel" = "HOW_S530-HOW_0.1")

  cNames <- c("Datum" = "Datum", flowSite)
  df_out <- df_out[,cNames]
  colnames(df_out) <- names(cNames)

  output_fileName <- paste0("q_", qsim_fileName)
  utils::write.table(x = df_out,
                     file = file.path(save_path,output_fileName),
                     sep = ";",
                     dec = ".",
                     row.names = F)
}

#' Get River, Section and location information from Qsim site names
#'
#' @param misa_assessment The assessment of a Qsim simulation
#'
siteInfo_from_QsimName <- function(misa_assessment){
  lapply(misa_assessment, function(x){
    if(is.data.frame((x))){
      x[["river"]] <-  sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][1]})

      x[["section"]] <-  sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][2]})

      x[["km"]] <-  as.numeric(sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][3]}))
      x[order(x$km, decreasing = T),]
    }
  })
}


#' Aggregates the MiSa assessment of complete event series
#'
#' @param dl_misa List of events
#'
df_aggr <- aggregate_eventSeries <- function(dl_misa){
  indices <- seq_along(dl_misa)
  df_aggr <- data.frame(
    "hours" = sapply(paste0("below_", c(0.5, 1, 1.5, 2, 3)), function(x){
      rowSums(data.frame(sapply(X = indices, function(i){
        v <- dl_misa[[i]]$hours[[x]]
        if(is.null(v)){
          NA
        } else {v}
      })), na.rm = T)
    }),
    "events" = rowSums(data.frame(sapply(X = indices, function(i){
      v <- dl_misa[[i]]$events[[paste0("below_", 1.5)]]
      if(is.null(v)){
        NA
      } else {v}
    })), na.rm = T),
    "neg_dev" = rowMeans(data.frame(sapply(X = indices, function(i){
      v <- dl_misa[[i]]$neg_dev$neg_deviation_relative
      if(is.null(v)){
        NA
      } else {v}
    })), na.rm = T)
  )

  df_aggr$qsim_site <- rownames(dl_misa[[2]]$hours)
  df_aggr
}
