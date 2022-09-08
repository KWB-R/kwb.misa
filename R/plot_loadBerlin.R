#' Reads package Data containing river courses of Berlin and river sites
#' simulated by the Qsim model
#'
#' @details
#' Geo-Data come from
#' Map data (c) OpenStreetMap contributors, https://www.openstreetmap.org
#' Extracts created by BBBike, https://extract.bbbike.org
#' osmium2shape by Geofabrik, https://geofabrik.de
#'
#' Please read the OSM wiki how to use shape files.
#'
#' https://wiki.openstreetmap.org/wiki/Shapefiles
#'
#'
#' shape file was created on: Fri 18 Jun 2021 03:58:25 PM UTC
#' GPS rectangle coordinates (lng,lat): 13.097,52.387 x 13.674,52.634
#' Script URL: https://extract.bbbike.org?sw_lng=13.097&sw_lat=52.387&ne_lng=13.674&ne_lat=52.634&format=shp.zip&city=Berlin&lang=de
#' Name of the area: Berlin
#'
#' @return
#' tables of all relevant rivers in misa combined in a list. Each table consists
#' of 6 columns: 1) longitued, 2) lattitude, 3) distance to fowollowing node in
#' km, 4) River km, 5) Corresponding Qsim output sitename, 6) Comment that can
#' be used for plots
#'
#' @export
#'
load_berlin_rivers <- function(){
  river_files <- dir(file.path(system.file(package = "kwb.misa"),
                               "extdata/berlin_rivers"), full.names = T)

  river_names <- dir(file.path(system.file(package = "kwb.misa"),
                               "extdata/berlin_rivers"), full.names = F)
  river_names <- substr( x =  river_names,
                         start = 7, stop = nchar(river_names) - 4)

  rivers <- lapply(river_files, function(file){
    river <- read.table(file = file, header = T, sep = ";", dec = ".")
  })

  names(rivers) <- river_names
  rivers
}

#' Loads river sites for comparing bar plots
#'
#' @return
#' Table as defined in 'extdata/focus_sites' filtered for all rows
#' containing "y" in the "Auswahl"-column
#'
#' @export
load_focus_sites <- function(){
  sid <- read.table(
    file = dir(
      file.path(
        system.file(package = "kwb.misa"),
        "extdata/focus_sites"),
      full.names = T),
    header = T,
    sep = ";",
    dec = ".")
  sid[sid$Auswahl == "y",]
}



