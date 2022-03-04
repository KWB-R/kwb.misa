#' Read MiSa Files
#'
#' This function combines the functions read_misa_oneSite and
#' read_misa_multipleSites and is strictly bound the the misa folder structure
#'
#' @param input_path This is the directory where the two folders "files_per_site"
#' and "sites_per_file" are located
#'
#' @details
#' All csv files from both folders will be loaded. They must contain a timestamp
#' column and an oxygen column. The timestamp column is identified
#' automatically, by looking for a column where the entries contain ":" and one
#' of the date separating symbols ".", "/" or "-".. The oxygen column is
#' found by its colname. It should conatin "O2", "o2", "Oxygen", "oxygen", "ox",
#' "Ox", "Sauerstoff" or "sauerstoff".
#'
#' For "file_per_site" files: all letters in the filename before the first "_" are
#' used for the sitename.
#' For "sites_per_file" files: all column names will be used for site names.
#' Thus, all columns except the timestamp must be oxygan concentrations at
#' different sites
#'
#' @return
#' A Data frame with 3 columen: Timestamp, Oxygen data and Site name
#'
#' @export
#'
read_misa_files <- function(input_path){
  # load all files with one site (continous quality parameter measurements) ----
  # look for all files stored in "files_per_site" folder and convert to list
  # containing one data frame per file
  files_per_site_path <- file.path(input_path, "files_per_site")

  files1 <- dir(files_per_site_path, pattern = "\\.csv$")

  siteData <- if(length(files1)){

    # Site Ids are sought from file name, all symbols before first "_"
    siteIDs <- sapply(files1, function(x){
      strsplit(x = x, split = "_")[[1]][1]
    })

    # combination of name of files and site-ID
    fileData <- data.frame(
      fileDir = files_per_site_path,
      fileName = files1,
      siteIDs = siteIDs)

    do.call(rbind, apply(fileData, 1, function(rawdata){
      read_misa_oneSite(
        path = rawdata[[1]], file = rawdata[[2]], siteID = rawdata[[3]])
    }))

  } # else NULL implicitly

  # load all files with more than one site (model results) ---------------------
  sites_per_file_path <- file.path(input_path, "sites_per_file")

  files2 <- dir(sites_per_file_path, pattern = "\\.csv$")

  fileData <- if(length(files2)){

    do.call(rbind, lapply(
      files2, read_misa_multipleSites, path = sites_per_file_path
    ))

  } # else NULL implicitly

  # combine all loaded data in one data frame, resetting the row names
  kwb.utils::resetRowNames(rbind(siteData, fileData))
}

#' read_misa_oneSite
#'
#' This function reads csv tables with one timestamp column and one
#' oxygen data column
#'
#' @param path The path to the file
#' @param file Filename (including ".csv" Ending)
#' @param siteID a character vector specifying the site name
#'
#' @return
#' A Data frame with 3 columen: Timestamp, Oxygen data and Site name
#'
#' @export
#'
read_misa_oneSite <- function(path, file, siteID){

  data <- read_csv(file.path(path, file))

  print(paste0(siteID, " - data loaded"))

  dateCol <- finding_timestampColumns(dataFrame = data)
  o2col <- finding_o2Column(dataFrame = data)

  if(length(o2col) == 0L){
    stop("No oxygen column found - please rename the column to one of:",
         "'O2', 'o2', 'Oxygen', 'oxygen', 'ox', 'Ox', 'Sauerstoff', 'sauerstoff'")
  }

  if(length(dateCol) == 0L){
    stop("no date column found")
  }

  if(length(dateCol) > 1L){
    stop("More than 1 date column found")
  }

  # data$posixDateTime <- as.POSIXct(x = data[,dateCol],
  #                                  tryFormats = c("%Y-%m-%d %H:%M:%S",
  #                                                 "%d.%m.Y% %H:%M:%S",
  #                                                 "%Y/%m/%d %H:%M:%S",
  #                                                 "%Y-%m-%d %H:%M",
  #                                                 "%Y/%m/%d %H:%M"))
  data.frame(
    posixDateTime = to_posix(data[[dateCol]]),
    oxygen = data[[o2col]],
    site = siteID,
    stringsAsFactors = FALSE)
}

#' read_misa_multipleSites
#'
#' This function reads csv tables with one timestamp column and several
#' oxygen data columns. Where the colnames refer to the sites of measurements
#'
#' @param path The path to the file
#' @param file Filename (including ".csv" Ending)
#'
#' @return
#' A Data frame with 3 columen: Timestamp, Oxygen data and Site name
#'
#' @export
#'
read_misa_multipleSites <- function(path, file){

  data <- read_csv(file.path(path, file))

  dateCol <- finding_timestampColumns(dataFrame = data)
  siteIDs <- colnames(data)[-dateCol]

  if(length(dateCol) == 0L){
    stop("no date column found")
  }

  if(length(dateCol) > 1L){
    stop("More than 1 date column found")
  }

  data$posixDateTime <- to_posix(data[[dateCol]])

  data <- lapply(siteIDs, function(x){
    data.frame(
      posixDateTime = data[,"posixDateTime",],
      oxygen = data[,x],
      site = x, stringsAsFactors = FALSE)
  })

  print(paste0(file, " - POSIXDateTime column created"))

  data.frame(do.call(rbind, data))
}

# Read csv file with default settings
read_csv <- function(file){

  kwb.utils::catAndRun(paste("Loading data from", file), {
    utils::read.table(
      file, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE
    )
  })
}

# Convert text time stamps to POSIXct, assuming different possible formats
to_posix <- function(x){
  as.POSIXct(x, tryFormats = c(
    "%Y-%m-%d %H:%M:%S",
    "%d.%m.Y% %H:%M:%S",
    "%Y/%m/%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M"
  ))
}

#' finding_timestampColumns
#'
#' This function looks for the timestamp column in a data frame by typical
#' timestamp symbols
#'
#' @param dataFrame The data frame where the column is searched
#'
#' @return
#' A vector with column numbers of timestamp columns
#'
#' @export
finding_timestampColumns <- function(
  dataFrame
){
  which(sapply(dataFrame, function(v){
    length(intersect(x = grep(pattern = "(\\:)", x = v),
                     y = grep(pattern = "(\\.|\\-|/)", x = v))) /
      length(v)
  }) > 0.5)
}


#' finding_o2Column
#'
#' This function looks for the oxygen data column in a data frame by column
#' name
#'
#' @param dataFrame The data frame where the column is searched
#' @param tryO2 A vector with patterns possible patterns of columnnames with
#' oxygen data (not case sensitive)
#'
#' @return
#' A vector with column numbers of oxygen columns
#'
#' @export
finding_o2Column <- function(
  dataFrame, tryO2 = c("o2", "oxygen", "ox", "sauerstoff")
){
  unlist(sapply(X = tryO2, grep, x = tolower(colnames(dataFrame))))
}
