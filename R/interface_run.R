#' Convert data from infoworks output to gerris input format
#'
#' @param interface_input_folder Folder in which the inforowks output files are
#' located
#' @param interface_output_folder Folder where the gerris input files are saved
#' @param simulation_name Either the whole name of the simulation or a unique
#' part regarding all simulations in the defined folder
#' @param return_after if a number is given, the function returns the data after
#' thata data processing step. For more information see details
#' @param return_output_table If TRUE the output table is returned, otherwise it
#' is only saved in the speficied table
#' @param infoworks_time_format The Inforworks time format default is
#' "%d/%m/%Y %H:%M:%S". For manual adaption see details of [strptime()]
#' @param gerris_time_format  The Gerris time format default is
#' "%d.%m.%Y %H:%M". For manual adaption see details of [strptime()]
#' @param timestep_out The timesteps needed for Gerris in minutes
#' (default is  15)
#' @param skip_hours The hours to skip before Gerris input is needed. This is
#' especially imnportant if infoworks needs a long start-up phase that should
#' not be passed on the gerris
#' @param flow_threshold A threshold underneath which the flow is set to 0 to
#' avoid negative flows (in m3/s)
#' @param flow_only If TRUE, a flow file is sufficient as interface input.
#' All other available input files will also be processed but no total nitrogen
#' is calculated that might lead to an error due to missing nitrogen input files
#' @param outlets_to_suppress Character vector defining Infoworks outlet
#' IDs for all outlets to be suppressed. For those outlets the flow will be
#' constantly set to 0.
#'
#' @details These are the steps within the function:
#' 1. load data
#' 2. set flow to 0 below threshold value
#' 3. change timesteps
#' 4. outlets that do not have an ID in Gerris are integrated into the closest
#' vailable outlet in the model
#' 5. calculating concentration by parameters mass flow and water flow
#' 6. if TKN < NH4-N --> TKN = NH4-N (not done if floe_only = TRUE)
#' 7. new table for total Nitrogen (sum of TKN NO2, and NO3)
#' (not done if floe_only = TRUE)
#' 8. Rename Parameters
#' 9. Add all Parameters with constant values
#' 10. Check if all parameters are within the gerris range between min and max
#' and round to 3 significant digits
#' 11. reshape the data from data per parameter to data per outlet
#' 12. add a column with gerris id ("Randbedingung")
#' 13. delete all rows without flow (except for the very first time step, and
#' the event surrounding timesteps)
#' After that data is aggregated for the "stats" output files
#' 14. collect all data in one single table
#' 15. change time format
#' 16. Save table for use in gerris
#'
#' @return
#' One table containing all water quality and overflow data per outlet and
#' timestamp at which an overflow occurred plus there very first timestemp of
#' the simulation. One table with statistics per outlet over the whole
#' simulation time. A log file with messages of the data processing.
#'
#' @export
#' @import graphics
iw_gerris_interface <- function(
    interface_input_folder, interface_output_folder,simulation_name,
    return_after = "", return_output_table = FALSE,
    infoworks_time_format = "%d/%m/%Y %H:%M:%S",
    gerris_time_format = "%d.%m.%Y %H:%M",
    timestep_out = 15, # minutes
    skip_hours = 0,
    flow_threshold = 0.003,
    flow_only = FALSE,
    outlets_to_suppress = NULL
){
  parameter_conversion <- paraIDs()
  outlet_conversion <- outletIDs()
  flowID_infoworks <- parameter_conversion$id_infoworks[
    parameter_conversion$parameter == "Durchfluss"]

  time_suffix <- format(Sys.time(), "_%Y%m%d_%H-%M")

  con <- file(file.path(
    interface_output_folder,
    paste0(simulation_name , time_suffix,".log")))
  sink(con, append = TRUE)

  cat("Transformation of the Infoworks Output: ", simulation_name, "\n",
      "days skipped in the Beginning: ", skip_hours / 24, "\n",
      "1. data loaded from: ", interface_input_folder, " --- ",
      as.character(Sys.time()), "\n", sep = "")

  {
    # dev.new(noRStudioGD = TRUE, width = 7, height = 5)
    par(mar = c(0,0,0,0), cex = 0.8)
    plot(x = 0, y = 0, type = "n", axes = F, xlab = "", ylab = "",
         xlim = c(0,19), ylim = c(4,10))
    rect(xleft = 0, xright = 16, ybottom = 4.5, ytop = 5.5,
         col = "white", lty = "dashed")
  }

  # 1. load data
  preText("loading data")
  iw_out <- load_infoworks_output(
    iw_output_folder = interface_input_folder,
    simulation_name = simulation_name,
    flow_only = flow_only,
    parameter_conversion = parameter_conversion,
    outlet_conversion = outlet_conversion
  )
  postText("data loaded", 1)
  if(isTRUE(all.equal(return_after, 1))){ return(iw_out) }

  # 2. set flow to 0 below threshold value
  preText("setting flow to 0 below threshold value")
  iw_out[[flowID_infoworks]] <- correct_flow(
    input_data = iw_out[[flowID_infoworks]],
    flow_threshold = flow_threshold
  )
  cat("2. flow rates below ", flow_threshold, " m3/s set to 0.\n")
  postText("flow corrected", 2)

  # 2a. Suppress outlets if defined
  if(!is.null(outlets_to_suppress)){
    iw_out[[flowID_infoworks]] <- suppress_flow(
      input_data = iw_out[[flowID_infoworks]],
      outlets_to_suppress = outlets_to_suppress,
      outlet_conversion = outlet_conversion
    )
    cat("outlets:", paste0(outlets_to_suppress, collapse = ", "),
        "were constantly set to 0 flow\n")
  }
  if(isTRUE(all.equal(return_after, 2))){ return(iw_out) }


  # 3. change timesteps
  preText("changing timesteps")
  iw_out <- lapply(
    iw_out, adapt_timestep,
    time_format = infoworks_time_format,
    time_column = 1,
    skip_hours = skip_hours,
    timestep_out = timestep_out
  )
  cat("3. timesteps adapted to", timestep_out, "minutes.\n")
  postText("timesteps changed", 3)
  if(isTRUE(all.equal(return_after, 3))){ return(iw_out) }

  # 4. outlets that do not have an ID in Gerris are integrated into the closest
  # available outlet in the model
  preText("integrating outlets")
  iw_out <- integrate_missing_outlets(
    data_input = iw_out,
    outlet_conversion = outlet_conversion
  )
  postText("not defined outlets integrated", 4)
  if(isTRUE(all.equal(return_after, 4))){ return(iw_out) }

  # 5. calculating concentration by parameters mass flow and water flow
  preText("calculating concentrations")
  iw_out <- get_concentrations(
    input_data = iw_out,
    flowID_infoworks = flowID_infoworks
  )
  cat("5. calculated concentration [mg/L] from mass flow [kg/s] and water",
      "flow [m3/s]\n")
  postText("oncentration calculated", 5)
  if(isTRUE(all.equal(return_after, 5))){ return(iw_out) }

  if(!flow_only){
    # 6. if TKN < NH4-N --> TKN = NH4-N
    preText("correcting TKN")
    iw_out[["mftkntot"]] <- kn_correction(input_data = iw_out)
    cat("6. Kjeldahl-N less than NH4-N was increased to NH4-N concentration\n")
    postText("TKN corrected", 6)
    if(isTRUE(all.equal(return_after, 6))){ return(iw_out) }

    # 7. new table for total Nitrogen (sum of TKN NO2, and NO3)
    preText("adding total Nitrogen")
    tN <- parameter_conversion$id_gerris[
      parameter_conversion$parameter == "Gesamt-Stickstoff"]

    iw_out[[tN]] <- cbind(
      "Zeit" = iw_out[["mftkntot"]][,1],
      iw_out[["mftkntot"]][,-1] +
        sum(parameter_conversion$constant_value[
          parameter_conversion$parameter %in%
            c("Nitrit-Stickstoff", "Nitrat-Stickstoff")])
    )
    cat("7. total nitrogen calculated using Kjeldahl nitrogen, nitrate and",
        "nitrite\n")
    postText("Total Nitrogen added", 7)
    if(isTRUE(all.equal(return_after, 7))){ return(iw_out) }
  }

  # 8. Rename Parameters
  preText("renaming parameters")
  iw_out <- rename_IDs(
    input_data = iw_out,
    parameter_conversion = parameter_conversion
  )
  cat("8. parameter names converted from Infoworks ID to Gerris ID\n")
  postText("parameters renamed", 8)
  if(isTRUE(all.equal(return_after, 8))){ return(iw_out) }

  # 9. Add all Parameters with constant values
  preText("adding constant parameters")
  iw_out <- add_constant_values(
    input_data = iw_out,
    parameter_conversion = parameter_conversion
  )
  cat("9. all parameters with constant values added\n")
  postText("constant parameters added", 9)
  if(isTRUE(all.equal(return_after, 9))){ return(iw_out) }

  # 10. Check if all parameters are within the gerris range between min and max
  # and round to 3 significant digits
  preText("forcing parameters into Gerris parameter range")
  iw_out <- check_parameter_range(
    input_data = iw_out,
    parameter_conversion = parameter_conversion
  )
  cat("10. all parameters forced to be in Gerris value range\n")
  postText("all parameters in Gerris range", 10)
  if(isTRUE(all.equal(return_after, 10))){ return(iw_out) }

  # 11. reshape the data from data per parameter to data per outlet
  preText("reshaping data into tables per outlet")
  iw_out <- data_per_outlet(input_data = iw_out)
  cat("11. data reshaped form parameter tables to outlet tables\n")
  postText("data reshaped", 11)
  if(isTRUE(all.equal(return_after, 11))){ return(iw_out) }

  # 12. add a column with gerris id ("Randbedingung")
  preText("adding gerris ID")
  new_names <- outlet_conversion[["gerris_id"]][
    find_positions(v_original = ids_from_colnames(names(iw_out)),
                   v_new = outlet_conversion[["upstream_link_id"]])]
  names(iw_out) <- new_names
  iw_out <- lapply(new_names, function(x){
    iw_out[[which(names(iw_out) == x)]][["RbId"]] <- x
    iw_out[[which(names(iw_out) == x)]]
  })
  cat("12. Outlet names changed from Infoworks to Gerris ID\n")
  postText("Gerris ID added", 12)
  if(isTRUE(all.equal(return_after, 12))){ return(iw_out) }

  # 13. delete all rows without flow (except for the very first time step, and
  # the event surrounding timesteps)
  preText("deleting rows without flow (except)")
  iw_out <- lapply(
    iw_out,
    keep_overflows,
    flowID_name =
      parameter_conversion$id_gerris[
        which( parameter_conversion$id_infoworks == flowID_infoworks)]
  )

  cat("13. all timesteps without flow deleted (except for the very first",
  "timestep, and the event surrounding timesteps)\n")
  postText("rows without flow deleted", 13)
  if(isTRUE(all.equal(return_after, 13))){ return(iw_out) }

  # aggregate data per outlet
  cso_stats <- get_cso_stats(
    input_data = iw_out,
    timestep_out = timestep_out,
    flow_only = flow_only,
    outlet_conversion = outlet_conversion
  )

  # 14. collect all data in one single table
  preText("Creating single table")
  iw_out <- build_gerris_table(data_input = iw_out)
  postText("reshaped data into single table", 14)
  if(isTRUE(all.equal(return_after, 14))){ return(iw_out) }

  # 15. change time format
  preText("Changing time format")
  iw_out <- change_time_format(
    input_data = iw_out,
    new_format = gerris_time_format
  )
  cat("14. Time format changed\n")
  postText("time format changed", 15)

  # 16. Save table for use in gerris
  preText("Saving table")
  save_gerris_input(
    gerris_input_folder = interface_output_folder,
    output_filename = paste0(simulation_name, time_suffix),
    input_data = iw_out
  )
  save_cso_stats(
    input_data = cso_stats,
    gerris_input_folder = interface_output_folder,
    output_filename = paste0(simulation_name, time_suffix)
  )
  cat("15. Saved Gerris input csv-file in:", interface_output_folder, "---",
      as.character(Sys.time()), "\n")
  postText("output saved", 16)
  if(return_output_table){
    return(iw_out)
  }
  sink()
}

#' Detailed look on concentration over the Gerris limit
#' special functions for more information about concentrations above gerris
#' concentration limit --> see concentrations and the according flow
#' iw_gerris_interace needs to be stopped at "5" to obtain the results table
#'
#' @param result Table created with [iw_gerris_interface()], stopped at step 5.
#' @param parameterID_infoworks The parameter to be checked. ID according to
#' infoworks
#' @details
#' special functions for more information about concentrations above gerris
#' concentration limit --> see concentrations and the according flow
#' iw_gerris_interace needs to be stopped at "5" to obtain the results table
#'
#' @export
#' @importFrom readxl read_excel
check_high_concentration <- function(
    result,
    parameterID_infoworks
){
  parameter_conversion <- paraIDs()
  flowID_infoworks <- parameter_conversion$id_infoworks[
    parameter_conversion$parameter == "Durchfluss"]

  df <- result[[parameterID_infoworks]]
  gerris_limit <- parameter_conversion$max_gerris[which(
    parameter_conversion$id_infoworks == parameterID_infoworks)]
  if(length(gerris_limit) == 0){
    print("no limit found: check parameter name")
  }
  high <- lapply(2:ncol(df), function(x){
    here <- which(df[[x]] > gerris_limit )
    if(length(here) > 0){
      here
    } else {
      NA
    }
  })

  data.frame(
    "Parameter" =
      unlist(lapply(which(!is.na(high)), function(x){
        df[high[[x]], x+1]
      })),
    "flow" =
      unlist(lapply(which(!is.na(high)), function(x){
        result[[flowID_infoworks]][high[[x]], x+1]
      })),
    "outlet" =
      unlist(lapply(which(!is.na(high)), function(x){
        rep(colnames(df)[x+1], length(high[[x]]))
      }))
  )
}
