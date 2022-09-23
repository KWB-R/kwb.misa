#' Loads the parameter ID table
#'
#' @details
#' Columns:
#' parameter: full German Parameter Name
#' id_infoworks: parameter ID in Infoworks (mf = mass flow)
#' unit_infoworks: unit of the infoworks output (mass flow)
#' constant_value: parameters that are not simulated by infoworks and are set
#' to a constant value
#' unit is the same as in gerris (see column "unit_gerris")
#' id_gerris: parameter ID in Gerris
#' unit_gerris: unit needed for Gerris input (concentration)
#' min_gerris and max_gerris: tolerated range of parameter in Gerris model
#' comment: possibility to add comments (column not used by r-script)
#' @importFrom readxl read_excel
#' @export
#'
paraIDs <- function(){
  as.data.frame(
    readxl::read_excel(
      path = system.file(
        "extdata/interface/parameter_conversion.xlsx",
        package = "kwb.misa"
      )
    )
  )
}

#' Loads the package outlet ID table
#'
#' @details
#' Columns:
#' outlet_id: Node ID of the outlet within the infoworks model
#' upstream_link_id: Infoworks ID of the link leading to outlet (this must be
#' usedfor mass flow information)
#' gerris_id: "Randbedingung" used in gerris (provided by Schumacher,
#' Ingenieurbüro für Wasser und Umwelt)
#' surface_water: name of the surface water the outlet discharges into
#' water_body_km: the water body km where the outlet is located
#' y_coordinate and x_coordanite: outlet location in soldner coordinates for
#' Berlin
#' all lat and lon columns: coordinates transformed from Soldner to UTM-Format
#' (ETRS89 DREF91)
#' comment: possibility to add comments (column not used by r-script)
#' @importFrom readxl read_excel
#' @export
#'
outletIDs <- function(){
  as.data.frame(
    readxl::read_excel(
      path = system.file(
        "extdata/interface/outlet_conversion.xlsx",
        package = "kwb.misa"
      )
    )
  )
}

#' Loads river sites for comparing bar plots
#'
#' @return
#' Table as defined in 'extdata/misa_data' filtered for all rows
#' containing "y" in the "Auswahl"-column
#'
#' @export
#'
loadMisa_focus_sites <- function(){
  path <- file.path(system.file(package = "kwb.misa"), "extdata/misa_data")

  sid <- read.table(
    file = file.path(path, "focus_sites.csv"), header = T,sep = ";", dec = ".")

  sid[sid$use == "y",]
}

#' Loads critical rain events for scenario assessment
#'
#' @return
#' Table as defined in 'extdata/misa_data' filtered
#'
#' @export
#'
loadMisa_events <- function(){
  path <- file.path(system.file(package = "kwb.misa"), "extdata/misa_data")

  e_data <- read.table(file = file.path(path,"ereignisreihe.csv"),
                       header = T, sep = ";", dec = ".")

  e_data$tBeg <- as.POSIXct(e_data$tBeg, format = "%d.%m.%Y %H:%M")
  e_data$tEnd <- as.POSIXct(e_data$tEnd, format = "%d.%m.%Y %H:%M")

  e_data
}

#' Loads table with decoupling information
#'
#' @return
#' Table as defined in 'extdata/misa_data'
#'
#' @export
#'
loadMisa_decouplingInfo <- function(){
  path <- file.path(system.file(package = "kwb.misa"), "extdata/misa_data")
  read.table(
    file = file.path(path, "outlet_decoupling.csv"),
    header = T,sep = ";", dec = ".")
}

#' Dateiled information about catchments within one decoupling scenario
#'
#' @param decouplingScenario String as in colnames of the "outlet_decoupling.csv"
#' table, that can be loaded with [loadMisa_decouplingInfo()]
#'
#' @return
#' A list of 2: 1) Character vector of all considered outlets of the scenario
#' (outled ID as used in Qsim)
#' 2)Data frane with 5 columns: i) Catchment Names as defined in infoworks,
#' ii) Number of outlets per Catchment of all outlets, iii) number of outlets per
#' catchment of the decoupling scenario, iv) absolute decrease of outlets per
#' catchment compared to no decoupling, v) relative decrease of outlets
#' per catchment compared to no decoupling
#'
#' @export
#'
decoupledCatchments <- function(decouplingScenario){
  decoupled <- loadMisa_decouplingInfo()

  catchOfAll <- summary(as.factor(unlist(
    sapply(decoupled$catchment, strsplit, split = " \\+ "))))

  outletsKept <- decoupled$gerris_id[(as.logical(decoupled[[decouplingScenario]]))]
  if(length(outletsKept) > 0L){
    decoupled <- decoupled[decoupled$gerris_id %in% outletsKept,]
  } else {
    outletsKept <- decoupled$gerris_id
  }

  catchOfUsed <- summary(as.factor(unlist(
    sapply(decoupled$catchment, strsplit, split = " \\+ "))))

  catch_df <- merge(
    x = data.frame("ID" = names(catchOfAll), "nOutAll" = catchOfAll),
    y = data.frame("ID" = names(catchOfUsed), "nOutUsed" = catchOfUsed),
    by = "ID",
    all.x = TRUE,
    incomparables = 0
  )
  catch_df$nOutUsed[is.na(catch_df$nOutUsed)] <- 0
  catch_df$nOutDelta <- catch_df$nOutAll - catch_df$nOutUsed
  catch_df$nOutDecoupled <- round(catch_df$nOutDelta / catch_df$nOutAll, 2)

  list("Scenario_outlets" = outletsKept, "Catchments_included" = catch_df)
}

