#' Read Qsim output and save in required format for misa assessment tool
#'
#' This function combines the Qsim site information of the first 3 columns
#' to unique sitenames and spreads the Qsim output table into a table with one
#' time column and several oxygen columns (one per site)
#'
#' @param qsim_output_path path where the output file from Gsim is stored
#' @param qsim_fileName Filename of the Qsim output file (inclusive .csv)
#' @param misa_tool_input_path input Folder for Misa Tool (see details)
#' @param output_fileName Filename of the created table (inclusive .csv)
#' @param return_table if TRUE, the saved table is returned
#'
#' @details
#' The Misa Tool input folder as definied in [misa_tool_input_path] must contain
#' the two folder "files_per_site" and "site_per_file". The output Table will be
#' stored in the latter.
#' Saves table as CSV File ";"-seperated.
#' [return_table = TRUE]
#'
#' @return
#' The saved table contains a timestamp column in Central European Time and
#' several oxygen columns in mg/L
#'
#' @export
#' @importfrom utils read.table write.table
#' @importfrom tidyr spread
#'
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

  df_out <- df_out %>% tidyr::spread(site, VO2)
  df_out$Datum <- as.POSIXct(df_out$Datum, format = "%d.%m.%Y %H:%M")
  df_out <- df_out[order(df_out$Datum),]

  write.table(x = df_out,  file = file.path(misa_tool_input_path,
                                            "sites_per_file",
                                            output_fileName),
              sep = ";", dec = ".", row.names = F)
}
