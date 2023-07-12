# Those steps are built on each other (run within the function
# "iw_gerris_interface"). Only the first loading function can be used
# seperately and is thus exported to NAMESPACE

#' 1. loading infoworks Output
#' @param iw_output_folder Folder with infoworks output files
#' @param simulation_name Unique part of the simulation name
#' @param flow_only If TRUE only flow data frame is loaded
#' @param parameter_conversion The parameter conversion table from the package
#' can be loaded with [paraIDs()]
#' @param outlet_conversion The outlet conversion table from the package
#' can be loaded with [outletIDs()]
#'
#' @importFrom utils read.csv
#' @export
load_infoworks_output <- function(
    iw_output_folder, simulation_name, flow_only, parameter_conversion,
    outlet_conversion
){
  # output parameters from parameter_conversion table
  output_paras <- if(flow_only){
    parameter_conversion$id_infoworks[
      parameter_conversion$parameter == "Durchfluss"]
  } else {
    parameter_conversion[
      !is.na(parameter_conversion[,"id_infoworks"]),"id_infoworks"]
  }

  # filter for the right files
  output_files <- list.files(path = iw_output_folder)
  output_files <- grep(pattern = simulation_name, x = output_files, value = T)
  if(length(output_files) == 0){
    stop(paste("no Infoworks data named", simulation_name))
  }
  output_files <- grep(pattern = paste0("\\.csv$"), x = output_files, value = T)
  output_files <- grep(pattern = paste0("^Link"), x = output_files, value = T)
  output_files <- grep(pattern = paste0("_ds_"), x = output_files, value = T)

  # load infoworks output data
  iw_out <- list()
  missing_for_any <- c()
  for(output_para in output_paras){

    this_file <- grep(pattern = output_para, x = output_files, value = T)
    if(length(this_file) == 0){
      cat("- Warning: No data-file for parameter:", output_para, "\n")

    } else {
      iw_out[[output_para]] <-
        read.csv(file = file.path(iw_output_folder, this_file),
                 header = T, sep = ",", dec = ".", stringsAsFactors = F)

      # only keep links listed in the outlet_conversion table
      ids <- ids_from_colnames(COLnames = colnames(iw_out[[output_para]]))
      missing_outlets <- which(!(outlet_conversion$upstream_link_id %in% ids))
      missing_outlets <- outlet_conversion$upstream_link_id[missing_outlets]
      keep_those <- which(ids %in% outlet_conversion$upstream_link_id)
      iw_out[[output_para]] <- iw_out[[output_para]][c(1,2,keep_those)]

      for(i in seq_along(missing_outlets)){
        iw_out[[output_para]][paste0("X", missing_outlets[i])] <- 0
      }

      missing_for_any <- c(missing_for_any, missing_outlets)
      message(output_para, " loaded")
    }
  }

  if(length(missing_for_any) > 0L){
  cat("- Warning: the following links are listed in the conversion table\n",
      "  but not found in the input data of at least one parameter:\n",
      "  ", unique(missing_for_any), "\n",
      "  Outlets have been integrated with 0 flow for Gerris ",
      "readability purposes.\n", sep = "")
  }

  iw_out
}

#' 2. Flow correction
#' @param input_data input
#' @param data_columns columns data (no time etc.)
#' @param flow_threshold minimum flow in m3/s
#'
correct_flow <- function(
    input_data,
    data_columns = 3:ncol(input_data),
    flow_threshold = 0.003
){
  # splitting time columns and flow data
  time_cols <- input_data[-data_columns]
  flow_mat <- as.matrix(input_data[,data_columns])

  # correct the flow
  flow_mat[flow_mat < flow_threshold] <- 0

  # joining time columns and corrected data
  cbind(time_cols, as.data.frame(flow_mat))
}

keep_overflows <- function(df_in, flowID_name){

  nRow <- nrow(df_in)
  cso <- which(df_in[[flowID_name]] != 0)
  beforeCso<- cso - 1L
  afterCso <- cso + 1L
  rows_to_keep <- sort(unique(c(1, cso, beforeCso, afterCso, nRow)))
  df_out <- df_in[rows_to_keep[rows_to_keep <= nRow],]

  # force first and last row to be 0 even if there is a CSO
  df_out[c(1, nrow(df_out)),
         which(!(colnames(df_out) %in% c("Zeit", "RbId")))] <- 0
  df_out
}

#' 3. Change of Timesteps
#' @param data_in Input
#' @param time_format Time format according to strptime
#' @param time_column Number of the time column
#' @param skip_hours Hours to skip at the beginning
#' @param data_columns Columns with data (no time etc.)
#' @param timestep_out Timestep in minutes
#'
adapt_timestep <- function(
    data_in, time_format = "%d/%m/%Y %H:%M:%S", time_column = 1,
    skip_hours = 24, data_columns = 3:ncol(data_in), timestep_out = 15
){
  # get time step information of infoworks simulation
  sim_beg <- as.POSIXct(data_in[1:2,time_column], format = time_format)
  timestep_in <- as.numeric(difftime(sim_beg[2],sim_beg[1], units = "mins"))

  # build table with average values in new time steps
  table_out <-
    as.data.frame(sapply(X = data_in[,data_columns], ts_ruleOf3,
                         timestep_in = timestep_in,
                         timestep_out = timestep_out))

  # create new time column
  time_table <-
    data.frame("Zeit" =
                 0:(nrow(table_out)-1) * timestep_out * 60  + sim_beg[1])

  # combine new time column with new average data
  df_out <- cbind(time_table, table_out)

  # cut rows of defined skipped time interval
  rm_rows <- which(df_out$Zeit <  sim_beg + 60 * 60 * skip_hours)
  if(length(rm_rows) > 0){
    df_out[-rm_rows,]
  } else {
    df_out
  }
}

#' Adjust temperal resolution of data
#'
#' Using a rule of three, the data is first scaled to a timestep of 1 minute and
#' then averaged for the required interval
#'
#' @param values Vector of timeseries data
#' @param timestep_in,timestep_out Timesteps in the same unit
#'
#' @export
#'
ts_ruleOf3 <- function(
    values,
    timestep_in,
    timestep_out){
  # one-minute-vector
  v_1 <- rep(values, each = timestep_in)

  # averaged vector in new time steps
  sapply(1:floor(length(v_1) / timestep_out), function(x){
    mean(v_1[(1:timestep_out) + timestep_out * (x-1)], na.rm = T)
  })
}

#' 4. Merge outlets that are not defined in Gerris with those that are
#' @param data_input Input
#' @param outlet_conversion The outlet conversion table from the package
#' can be loaded with [outletIDs()]
#'
integrate_missing_outlets <- function(
    data_input, outlet_conversion
){
  # all outlets with missing ID for Gerris
  missing <- outlet_conversion$upstream_link_id[
    which(is.na(outlet_conversion$gerris_id))]
  # pairing with the closest available outlet
  add_to <- lapply(missing,
                   closest_available_outlet,
                   outlet_conversion = outlet_conversion)

  # Adding the missing outlets mass flow to the closest available outlet
  # and deleting the missing outlet column
  for(x in add_to){
    data_input <- lapply(data_input, function(df){
      real_names <- ids_from_colnames(COLnames = colnames(df))
      df[,real_names == x["to"]] <- rowSums(df[,real_names %in% x])
      df[-which(real_names == x["from"])]
    })
    cat(x["from"], " (",
        outlet_conversion$surface_water[
          outlet_conversion$upstream_link_id == x["from"]],
        " at km ",
        outlet_conversion$water_body_km[
          outlet_conversion$upstream_link_id == x["from"]],
        ") is now part of ",
        x["to"], " (",
        outlet_conversion$surface_water[
          outlet_conversion$upstream_link_id == x["to"]],
        " at km ",
        outlet_conversion$water_body_km[
          outlet_conversion$upstream_link_id == x["to"]],
        ")\n", sep = "")
  }
  data_input
}

#' 4a. Find closest outlet to the missing one
#' @param missing_outlet Infoworks upstream link ID of the Missing outlet
#' @param outlet_conversion The outlet conversion table from the package
#' can be loaded with [outletIDs()]
#'
closest_available_outlet <- function(
    missing_outlet,
    outlet_conversion
){
  # missing outlet inforamtion
  moi <- outlet_conversion[outlet_conversion$upstream_link_id == missing_outlet,]

  # filter for surface water and available outlets
  sw <- moi$surface_water
  av_outlets <- outlet_conversion[outlet_conversion$surface_water == sw &
                            !is.na(outlet_conversion$gerris_id),]

  # minimum distance between missing outlet and available outlets
  all_dist <- abs(av_outlets$water_body_km - moi$water_body_km)
  min_dist <- min(all_dist)

  # select outlet with minimal distance (if more than one outlet are equally
  # far away, the first one is selected)
  c("from" = missing_outlet,
    "to" = av_outlets$upstream_link_id[which(all_dist == min_dist)[1]])
}

#' 5. calculating concentration from mass flows
#' @param input_data Input
#' @param flowID_infoworks Infoworks ID of flow
#'
get_concentrations <- function(
    input_data, flowID_infoworks
){
  flow <- which(names(input_data) == flowID_infoworks)

  flow_table <- input_data[[flow]]
  input_data <- input_data[-flow]

  # return list with flow and concentrations
  c("flow" = list(flow_table),
    lapply(input_data, function(table_in){
      # concentration is mass flow / flow * 1000 (from kg/s with m?/s to ng/L)
      concentration_mat <-
        as.matrix(table_in[,-1] / flow_table[,-1] * 1000) # without time columns
      concentration_mat[!is.finite(concentration_mat)] <- 0

      cbind("Zeit" = table_in[,1],
            as.data.frame(concentration_mat)) # add time colums
    }))
}

#' 6. correction of Kjeldahl Nitrogen
#' @param input_data Input
#'
kn_correction <- function(
    input_data
){
  tkn <- grep(pattern = "tkn",x = tolower(names(input_data)))
  nh4 <- grep(pattern = "nh4",x = tolower(names(input_data)))
  mat1 <- as.matrix(input_data[[tkn]][,-1])
  mat2 <- as.matrix(input_data[[nh4]][,-1])
  too_small <- which(mat1 < mat2)
  mat1[too_small] <- mat2[too_small]
  cbind("DatumUhrzei" = input_data$mftkntot[,1], as.data.frame(mat1))
}

#' 8. Renaming parameter IDs
#' @param input_data Input
#' @param parameter_conversion The outlet conversion table from the package
#' can be loaded with [paraIDs()]
rename_IDs <- function(
    input_data, parameter_conversion
){
  id_old <- parameter_conversion[,"id_infoworks"]
  id_new <- parameter_conversion[,"id_gerris"]
  # parameters that are not defined in the id_gerris column are not to be used
  remove_parameters <- id_old[is.na(id_new)]
  to_change <- which(names(input_data) %in% id_old &
                       !(names(input_data) %in% remove_parameters))

  for(changing in to_change){
    names(input_data)[changing] <-
      id_new[which(id_old == names(input_data)[changing])]
  }

  rm_entry <- which(names(input_data) %in% remove_parameters)
  if(length(rm_entry > 0)){
    input_data <- input_data[-rm_entry]
    message("Parameters: ",  remove_parameters,
            " deleted --> not defined in gerris ID column")
  }
  input_data
}
#' 9. Add constant values
#' @param input_data Input
#' @param parameter_conversion The outlet conversion table from the package
#' can be loaded with [paraIDs()]
#'
add_constant_values <- function(
    input_data, parameter_conversion
){
  rows_constant <- which(!is.na(parameter_conversion$constant_value))
  constant_parameters <- parameter_conversion$id_gerris[rows_constant]
  constant_value <- parameter_conversion$constant_value[rows_constant]
  time_column <- input_data[[1]][,1]
  flow_matrix <- as.matrix(input_data$Q[,-1] > 0)

  for(i in 1:length(constant_value)){
    df_process <-
      matrix(data = constant_value[i],
             nrow = nrow(input_data[[1]]),
             ncol = ncol(input_data[[1]])-1)
    df_process <- as.data.frame(df_process * flow_matrix)
    df_process <- cbind(time_column, df_process)
    colnames(df_process) <- colnames(input_data[[1]])

    input_data[[constant_parameters[i]]] <- df_process
  }
  input_data
}

#' 10. Checking if the concentration lies within the gerris range
#' @param input_data Input
#' @param parameter_conversion The outlet conversion table from the package
#' can be loaded with [paraIDs()]
#'
check_parameter_range <- function(
    input_data, parameter_conversion
){
  para_names <- names(input_data)

  for(i in 1:length(para_names)){
    # extract data matrix
    mat_process <- as.matrix(input_data[[i]][-1])
    # Min and Max values from parameter conversion table (if value is NA it
    # means there was no limit defined --> set to infinite)
    min_allowed <- parameter_conversion$min_gerris[
      which(parameter_conversion$id_gerris == para_names[i])]
    if(is.na(min_allowed)){min_allowed <- -Inf}
    max_allowed <- parameter_conversion$max_gerris[
      which(parameter_conversion$id_gerris == para_names[i])]
    if(is.na(max_allowed)){max_allowed <- Inf}

    # change values that are not within the Gerris limit
    to_small <- which(mat_process < min_allowed)
    to_high <- which(mat_process > max_allowed)
    if(length(to_small) > 0){
      mat_process[to_small] <- min_allowed
      cat(para_names[i], ": ", length(to_small),
              " values below Gerris limit and set to ", min_allowed)
    }
    if(length(to_high) > 0){
      mat_process[to_high] <- max_allowed
      cat(para_names[i], ": ", length(to_high),
              " values above Gerris limit and set to ", max_allowed)
    }

    # round to 3 significant digits
    mat_process <- signif(x = mat_process, digits = 3)

    # combine again
    input_data[[i]] <-  cbind("Zeit" = input_data[[i]][,1],
                              as.data.frame(mat_process))
  }
  input_data
}

#' 11. Reshaping from parameter-tables to outlet-tables
#' @param input_data Input
#'
data_per_outlet <- function(
    input_data
){
  outlet_names <- unique(unlist(lapply(input_data, colnames)))[-1]

  identical_outlets <- lapply(outlet_names, function(outlet){
    sapply(input_data, function(df){
      which(colnames(df) == outlet)
    })
  })

  names(identical_outlets) <- outlet_names

  lapply(identical_outlets, function(outlet){

    # per outlet look for the correct column in each parameter table
    df_out <- data.frame(mapply(function(n_para, n_out){
      input_data[[n_para]][[n_out]]
    },
    n_para = names(outlet),
    n_out = outlet))

    df_out[df_out$Q == 0L,] <- 0
    cbind("Zeit" = input_data[[1]][,1], df_out)


  })
}
#' 12. small function to help creating a column with gerris outlet ID
#' @param v_original Vector in old order
#' @param v_new Vector in new order
find_positions <- function(v_original, v_new){
  sapply(v_original, function(x){
    which(v_new == x)
  })
}

#' 14. Reshaping to Gerris format
#' @param data_input Input
#'
#' @importFrom tidyr gather
#'
build_gerris_table <- function(
    data_input
){
  ParamId <- Wert <- all_of <- NULL
  function_list <- lapply(data_input, function(x){
    all_parameters <-
      colnames(x)[which(!(colnames(x) %in% c("Zeit", "RbId")))]
    # reshape table
    function_table <- tidyr::gather(x, ParamId, Wert, all_of(all_parameters))
    # order by time
    function_table[order(function_table[,"Zeit"]),
                   c("RbId", "Zeit", "ParamId", "Wert")]
  })
  do.call(rbind, function_list)
}
#' 15. Change time to gerris format
#' @param input_data Input
#' @param new_format Time format in strptime format
#'
change_time_format <- function(
    input_data, new_format = "%d.%m.%Y %H:%M"
){
  input_data[,"Zeit"] <-
    format(input_data[,"Zeit"], new_format)
  input_data
}
#' 16. Save interface output
#' @param input_data Input
#' @param gerris_input_folder Output folder where data is saved
#' @param output_filename Filename
#'
save_gerris_input <- function(
    input_data, gerris_input_folder, output_filename
){
  write.table(
    x = input_data,
    file = file.path(gerris_input_folder, paste0(output_filename, ".csv")),
    sep = ";",
    dec = ",",
    row.names = FALSE,
    col.names = c(colnames(input_data)[1:3], ""), quote = F
  )
}

#' 16b. Save interface output (stats)
#' @param input_data Stats
#' @param gerris_input_folder Output folder where data is saved
#' @param output_filename Filename
#'
save_cso_stats <- function(
    input_data, gerris_input_folder, output_filename
){
  write.table(
    x = input_data,
    file = file.path(gerris_input_folder,paste0(output_filename, "_stats.csv")),
    sep = ";",
    dec = ".",
    row.names = FALSE
  )
}

#' ID from column name
#'
#' @param COLnames Names of Columns (starting with "X" followed by the ID)
#'
ids_from_colnames <- function(
    COLnames
){
  link_ids <- COLnames
  rm_x <- grep(pattern = "^X[0-9]", link_ids)
  link_ids[rm_x] <- substr(
    link_ids[rm_x],
    start = 2,
    stop = nchar(link_ids[rm_x])
  )
  link_ids
}

#' Function to get statatistic per outlet
#' @param input_data Input
#' @param timestep_out Timestep in strptime format
#' @param flow_only If TRUE BOB is set to 0
#' @param outlet_conversion The outlet conversion table from the package
#' can be loaded with [outletIDs()]
#'
get_cso_stats <- function(
    input_data, timestep_out, flow_only, outlet_conversion
){
  df_out <- do.call(rbind, lapply(input_data, function(df){
    x <- df[,"Q"]
    y <- if(flow_only){
      0
    } else {
      df[,"OBSB"]
    }

    cso <- which(x > 0)
    if(length(cso) > 0){
      data.frame(
        "RbId" = df[1, "RbId"],
        "tBeg" = df[min(cso), 1],
        "tEnd" = df[max(cso), 1],
        "tVol_m3" = sum(x) * timestep_out * 60,
        "maxIntens_m3_s" = max(x),
        "tBSB_g" = round(sum(x * y) * timestep_out * 60 / 1000, 2))
    } else {
      data.frame(
        "RbId" = df[1, "RbId"],
        "tBeg" = as.POSIXct(NA),
        "tEnd" = as.POSIXct(NA),
        "tVol_m3" = 0,
        "maxIntens_m3_s" = 0,
        "tBSB_g" = 0)
    }
  }))

  # combine stats with outlet information
  rownames(df_out) <- df_out[,"RbId"]
  gerris_outs <- which(outlet_conversion$gerris_id %in% df_out[,"RbId"])

  cbind(df_out[outlet_conversion[gerris_outs, "gerris_id"], ],
        outlet_conversion[gerris_outs, ])
}

#' Text written before data process step
#'
#' @param x Character describing the process step
#'
preText <- function(x){
  text(x = 0, y = 9.5, labels = paste0(x, " ..."), pos = 4)
}

#' Text written and status bar drawn after data process step
#'
#' @param x Character describing the process step
#' @param step Number of process step
#'
postText <- function(x, step){
  rect(xleft = 0, xright = 16, ybottom = 9, ytop = 10,
       col = "white", border = NA)
  rect(xleft = step - 1, xright = step, ybottom = 4.5, ytop = 5.5,
       col = "steelblue")
  text(x = step - 1, y = 5.7, labels = paste0(step,". ", x), srt = 45, pos = 4)
}

