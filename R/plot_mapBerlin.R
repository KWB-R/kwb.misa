#' Creates and stores a map Plot of Oxygen deficiency during CSO Event
#'
#' @param BerlinRivers Berlin rivers loaded from package with
#' [load_berlin_rivers()]
#' @param statFilesPath Directory of all stat files
#' @param dl_misa Misa assessment list of all events
#' @param event Event number to be plotted
#' @param savingPath Directory where plot is going to be stored
#' @param sizeMax Maximum size of CSO volumne. Default 100000 is a good
#' compromise between large and small CSO events
#' @param below Oxygen concentration limit used for the plot (mg/L)
#' @param sixBreaks 6 low limits of water quality categories
#' @param decoupling Character vector defining a decoupling scenario. Default
#' is "" for no scenario. Possible scenarios can be obtained by the column names
#' of [loadMisa_decouplingInfo()].
#' @param dec What should be used a decimal character? Default is "," since
#' the plots are in German language
#'
#' @importFrom grDevices png dev.off
#'
#' @export
#'
mapPlot_EventTime <- function(
    BerlinRivers,
    statFilesPath,
    dl_misa,
    event,
    savingPath,
    sizeMax = 100000,
    below = 1.5,
    sixBreaks = c(0,0.5,2,4,10,20),
    decoupling = "",
    dec = ","
){
  options(OutDec = dec)

  p_title <- paste0("E", event)
  misaAssessment <- dl_misa[[p_title]]

  if(!is.null(misaAssessment$hours)){
    all_files <- dir(statFilesPath, full.names = TRUE)
    stat_files <- grep(pattern = "stats.csv$", all_files, value = TRUE)
    statFile <- grep(pattern = paste0(p_title, "_"), x = stat_files, value = TRUE)
    stat <- read.csv(file = statFile,header = TRUE, sep = ";", dec = ".")
    df_plot <- stat[!is.na(stat$tBeg),]

    decoupled <- loadMisa_decouplingInfo()
    rm_cso <- decoupled$gerris_id[!(as.logical(decoupled[[decoupling]]))]
    df_plot <- df_plot[!(df_plot$RbId %in% rm_cso),]

    prepared_rivers <- lapply(
      BerlinRivers, extend_riverTable,
      qsim_misa_table = misaAssessment$hours,
      varName = paste0("below_", below),
      sixBreaks = sixBreaks
    )

    # plot dimensions
    xlim <- c(13.18, 13.472)
    ylim <- c(52.46, 52.57)
    plotDim <- kwb.misa::getDimensions(xlim = xlim, ylim = ylim, width = 10)
    width_factor <- plotDim[1]/plotDim[2]

    # plot
    png(filename = paste0(savingPath, "/", p_title, "defTime.png"),
        height = 6, width = 6 * width_factor, units = "in", res = 300)
    xpdDim <- 6
    par(mar = c(xpdDim / 2, 0.2, xpdDim / 2 , xpdDim * width_factor - 0.2))
    plot(x = 0, y = 0,
         xaxt = "n", yaxt = "n", type = "n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = "",
         xlim = xlim, ylim = ylim,
         main = paste0("MW\u00dc und Gew\u00e4sserbelastung - ", p_title)
    )

    add_catchments()
    add_coloredRivers(
      ext_riversList = prepared_rivers,
      sixBreaks = sixBreaks,
      dataType = "time",
      LegendTitle = "Unterschreitungsdauer\n in Stunden (1,5 mg/L)",
      LegendLocation = "left")
    add_csoVol(
      eventStats = df_plot,
      sizeMax = sizeMax)

    abline(h = ylim)
    abline(v = xlim)

    dev.off()
  } else {
    message("No Assessment for Event ", event)
  }
}

#' Creates and stores a map Plot of all oxygen deficiencies Events
#'
#' @param BerlinRivers Berlin rivers loaded from package with
#' [load_berlin_rivers()]
#' @param df_aggr Aggregated misa assessment data frame
#' @param savingPath Directory where plot is going to be stored
#' @param varName Column name that is used for water quality categorisation
#' @param sixBreaks 6 low limits of water quality categories
#' @param dec What should be used a decimal character? Default is "," since
#' the plots are in German language
#'
#' @importFrom grDevices png dev.off
#'
#' @export
#'
mapPlot_EventsNumber <- function(
    BerlinRivers,
    df_aggr,
    savingPath,
    varName = "events",
    sixBreaks = c(-1,0,1,3,6,10),
    dec = ","
){
  options(OutDec = dec)

  prepared_rivers <- lapply(
    BerlinRivers, extend_riverTable,
    qsim_misa_table = df_aggr,
    varName = varName,
    sixBreaks = sixBreaks
  )

  # plot dimensions
  xlim <- c(13.18, 13.472)
  ylim <- c(52.46, 52.57)
  plotDim <- kwb.misa::getDimensions(xlim = xlim, ylim = ylim, width = 10)
  width_factor <- plotDim[1]/plotDim[2]

  # plot
  png(filename = paste0(savingPath, "/all_events_deficitNumber.png"),
      height = 6, width = 6 * width_factor, units = "in", res = 300)
  xpdDim <- 4
  par(mar = c(0.2,  xpdDim * width_factor / 2,
              xpdDim - 0.2, xpdDim * width_factor / 2))

  plot(
    x = 0, y = 0,
    xaxt = "n", yaxt = "n", type = "n",
    xaxs = "i", yaxs = "i",
    xlab = "", ylab = "",
    xlim = xlim, ylim = ylim)
  add_catchments()
  add_coloredRivers(
    ext_riversList = prepared_rivers,
    sixBreaks = sixBreaks,
    dataType = "number",
    LegendTitle = "Anzahl kritischer Ereignisse",
    LegendLocation = "top")
  abline(h = ylim)
  abline(v = xlim)

  dev.off()
}

#' Creates and stores a map Plot of Oxygen deficiency time of all CSO Events
#'
#' @param BerlinRivers Berlin rivers loaded from package with
#' [load_berlin_rivers()]
#' @param df_aggr Aggregated misa assessment data frame
#' @param savingPath Directory where plot is going to be stored
#' @param varName Column name that is used for water quality categorisation
#' @param sixBreaks 6 low limits of water quality categories
#' @param dec What should be used a decimal character? Default is "," since
#' the plots are in German language
#'
#' @importFrom grDevices png dev.off
#'
#' @export
#'
mapPlot_EventsTime <- function(
    BerlinRivers,
    df_aggr,
    savingPath,
    varName = "hours.below_1.5",
    sixBreaks = c(0,25,50,100,200,300),
    dec = ","
){
  options(OutDec = dec)

  prepared_rivers <- lapply(
    BerlinRivers, extend_riverTable,
    qsim_misa_table = df_aggr,
    varName = varName,
    sixBreaks = sixBreaks
  )

  # plot dimensions
  xlim <- c(13.18, 13.472)
  ylim <- c(52.46, 52.57)
  plotDim <- kwb.misa::getDimensions(xlim = xlim, ylim = ylim, width = 10)
  width_factor <- plotDim[1]/plotDim[2]

  # plot
  png(filename = paste0(savingPath, "/all_events_deficitTime.png"),
      height = 6, width = 6 * width_factor, units = "in", res = 300)
  xpdDim <- 4
  par(mar = c(0.2,  xpdDim * width_factor / 2,
              xpdDim - 0.2, xpdDim * width_factor / 2))
  plot(
    x = 0, y = 0,
    xaxt = "n", yaxt = "n", type = "n",
    xaxs = "i", yaxs = "i",
    xlab = "", ylab = "",
    xlim = xlim, ylim = ylim)
  add_catchments()
  add_coloredRivers(
    ext_riversList = prepared_rivers,
    sixBreaks = sixBreaks,
    dataType = "time",
    LegendTitle = "Unterschreitungsdauer in Stunden (1,5 mg/L)",
    LegendLocation = "top")
  abline(h = ylim)
  abline(v = xlim)

  dev.off()
}

#' Adds Qualty Categories to the river table
#'
#' As a result, each location is linked to a water quality value.
#'
#' @param river_table Table of one of the rivers loaded with
#' [load_berlin_rivers()]
#' @param qsim_misa_table Either the "hours", "events" or "neg_dev" data frame
#' of the mise assessment
#' @param varName One of the columns of the qsim_misa_table containing water
#' quality numeric information that should be plotted
#' @param sixBreaks Breaks defining the lower limits of the categories. The
#' values are included at the lower limit.
#'
#' @details
#' The qsim_misa_table does not provide information for every location within
#' the river, since the local resolution of the qsim output differs from the
#' river course data. Values between two known river sites are interpolated.
#' The color scale cannot be changed. Since in all misa assessment parameters
#' low values refer to a good water quality, the colors range from green for low
#' values to red for high values.
#'
#' @return
#' The input river_table extended by three columns: 1) "value" containing the
#' exact value, 2) "quality" containing 6 different quality categories based on
#' the defined breaks, 3) "color" assigning th color for plotting
#'
#' @importFrom utils data
#' @export
#'
extend_riverTable <- function(
    river_table, qsim_misa_table, varName, sixBreaks
){
  MisaColor <- NULL
  data("MisaColor", envir = environment())

  sixBreaks <- c(sixBreaks, Inf)
  river_table$value <- NA

  for(i in seq_len(nrow(qsim_misa_table))){
    found <- c(which(qsim_misa_table$qsim_site == river_table$qsim_id[i]),
               which(rownames(qsim_misa_table) == river_table$qsim_id[i]))
    if(length(found) > 0L){
      river_table$value[i] <- qsim_misa_table[[varName]][found]
    }
  }

  river_table$value <-
    round(kwb.misa::interpolate_multipleNA(
      data_vector = river_table$value,
      max_na = 1000,
      diff_x = river_table$distance_to_neighbour)[[1]], 1)

  river_table$quality <-
    cut(river_table$value, breaks = sixBreaks,
        include.lowest = TRUE, ordered_result = TRUE)

  river_table$color <- MisaColor[as.numeric(river_table$quality)]
  river_table
}

#' Adds the colored rivers to the Berlin Map
#'
#' @param ext_riversList The list of Berlin rivers loaded with
#' [load_berlin_rivers()], where every table is extended with
#' [extend_riverTable()]
#' @param sixBreaks Breaks defining the lower limits of the categories. Must be
#' the same that were used in [extend_riverTable()]
#' @param dataType Is needed for the Legend. If "time" is used, it is assumed
#' that the first water quality category is between 0 and a value above 0, while
#' for the counting of events the first category is 0 only. If that is wanted
#' anything but "time" can be assigned.
#' @param LegendTitle String with title of the legend
#' @param LegendLocation Either "top" or "right" (outside the plot margin)
#'
#' @export
#'
add_coloredRivers <- function(
    ext_riversList, sixBreaks, dataType = "time", LegendTitle, LegendLocation
){
  sixBreaks <- c(sixBreaks, Inf)
  MisaColor <- NULL
  data("MisaColor", envir = environment())

  for(j in seq_along(ext_riversList)){
    lines(x = ext_riversList[[j]]$x, y = ext_riversList[[j]]$y,
          col = "steelblue")
    for(i in seq_len(nrow(ext_riversList[[j]]) - 1)){
      lines(x = ext_riversList[[j]]$x[i:(i+1)],
            y = ext_riversList[[j]]$y[i:(i+1)],
            col = ext_riversList[[j]]$color[i+1],
            lwd = 6)
    }
  }

  ll <- length(sixBreaks)
  l_content <-
    if(dataType == "time"){
      c(paste0("<= ", sixBreaks[2]), paste0("> ", sixBreaks[2:(ll-1)]))
    } else {
      c(paste0("<= ", sixBreaks[2:(ll-1)]), paste0(">", sixBreaks[(ll-1)]))
    }
  if(LegendLocation == "top"){
    lx <- mean(par("usr")[1:2])
    ly <- par("usr")[4]
    xadj <- 0.5
    hor <- TRUE
  } else {
    lx <- par("usr")[2]
    ly <- par("usr")[3]
    xadj <- 0
    hor <- FALSE
  }
  legend(x = lx, y = ly, legend = l_content, col = MisaColor[seq_len(ll)], lwd = 6,
         bg= "white", bty = "n", title = LegendTitle,
         xpd = T, xjust = xadj, yjust = 0, horiz = hor)
}


#' Add Combined sewer overflow volume to Berlin map
#'
#' @param eventStats Statistics data frame of one event, as saved by the
#' interface [iw_gerris_interface()]
#' @param sizeMax The maximum Volume in m³ to scale the point size. 100 000 is
#' a good compromise between small and large events. But if a single event
#' is described in more detail, this value can be adapted.
#'
#' @importFrom grDevices rgb
#'
#' @export
#'
add_csoVol <- function(eventStats, sizeMax = 100000){

  points(
    x = eventStats$lon_grad + eventStats$lon_min / 60 + eventStats$lon_s / 3600,
    y = eventStats$lat_grad + eventStats$lat_min / 60 + eventStats$lat_s / 3600,
    pch = 21,
    cex = sqrt(eventStats$tVol_m3 /sizeMax)  * 7,
    bg = rgb(0,0,0, 0.5), col = "white"
  )

  legend(
    x = par("usr")[2],
    y = par("usr")[4],
    xpd = T,
    legend = c("1 000","25 000","50 000"),
    pch = 21,
    pt.cex = c(sqrt(1000 /sizeMax) * 7,
               sqrt(25000 /sizeMax) * 7 ,
               sqrt(50000 /sizeMax) * 7),
    title = "\u00dcberlaufvolumen in m\u00b3",
    bg = "white",
    bty = "n",
    x.intersp = 2,
    y.intersp = 2,
    pt.bg = rgb(0,0,0,0.5), # half transparent black
    col = "white"
  )
}

#' Add Berlin Catchments to a map of Berlin
#'
#' Polygons are drawn in different types of gray and names are added, without
#' overlapping the catchment boundaries or any river
#'
#' @export
#'
add_catchments <- function(){
  ezg <- NULL
  load(file.path(system.file(package = "kwb.misa"),
                 "extdata/misa_data/catch_polygon.RData"))

  {# Positions of catchment names in the plot
    ezg_namePositions <- lapply(ezg, function(loc_df) {
      data.frame("x" = mean(loc_df[,1]), "y" =  mean(loc_df[,2]))
    })

    ezg_namePositions$`Bln II`$y <-  ezg_namePositions$`Bln II`$y - 0.01
    ezg_namePositions$`Bln III`$x <-  ezg_namePositions$`Bln III`$x + 0.01
    ezg_namePositions$`Bln III`$y <-  ezg_namePositions$`Bln III`$y - 0.003
    ezg_namePositions$`Bln IIIa`$x <-  ezg_namePositions$`Bln IIIa`$x - 0.01
    ezg_namePositions$`Bln IIIa`$y <-  ezg_namePositions$`Bln IIIa`$y - 0.04
    ezg_namePositions$`Bln VIII`$x <-  ezg_namePositions$`Bln VIII`$x + 0.005
    ezg_namePositions$`Bln VIII`$y <-  ezg_namePositions$`Bln VIII`$y + 0.005
    ezg_namePositions$`Bln IX`$x <-  ezg_namePositions$`Bln IX`$x + 0.005
    ezg_namePositions$`Bln IX`$y <-  ezg_namePositions$`Bln IX`$y + 0.005
    ezg_namePositions$`Bln IX`$y <-  ezg_namePositions$`Bln IX`$y + 0.005
    ezg_namePositions$`Bln XI`$x <-  ezg_namePositions$`Bln XI`$x - 0.013
    ezg_namePositions$`Chb I`$y <-  ezg_namePositions$`Chb I`$y - 0.005
    ezg_namePositions$`Chb Ia`$y <-  ezg_namePositions$`Chb Ia`$y + 0.02
    ezg_namePositions$`Chb Ia`$x <-  ezg_namePositions$`Chb Ia`$x - 0.08
    ezg_namePositions$`Chb III`$y <-  ezg_namePositions$`Chb III`$y + 0.002
    ezg_namePositions$`Ruh`$x <-  ezg_namePositions$`Ruh`$x + 0.015
    ezg_namePositions$`Spa1`$x <-  ezg_namePositions$`Spa1`$x - 0.008
    ezg_namePositions$`Wil`$y <-  ezg_namePositions$`Wil`$y + 0.01
  }

  colCircle <- rep(paste0("gray",c(60,70,80,90)), 10)
  for(i in seq_along(ezg)){
    col <- colCircle[i]
    polygon(x = ezg[[i]][,1], y = ezg[[i]][,2], col = col)
    text(x = ezg_namePositions[[i]]$x, y = ezg_namePositions[[i]]$y,
         labels = names(ezg_namePositions)[i])
  }
  lines(x = c(ezg_namePositions$`Chb Ia`$x + 0.012, min(ezg$`Chb Ia`[,1]) + 0.004),
        y =  c(ezg_namePositions$`Chb Ia`$y - 0.002, max(ezg$`Chb Ia`[,2]) - 0.002))
  lines(x = c(ezg_namePositions$`Bln IIIa`$x, mean(ezg$`Bln IIIa`[,1]) + 0.0005),
        y =  c(ezg_namePositions$`Bln IIIa`$y + 0.002, mean(ezg$`Bln IIIa`[,2])))
}


#' Calculates x- and y-scale based on longitude and lattitude data
#'
#' Get the right scale for plotting a map. While width must be defined,
#' the according height is calculated
#'
#' @param xlim Minimum and maximum longitude of the map
#' @param ylim Minimum and maximum lattitude of the map
#' @param width Numeric value defining the width of the plot (in inch)
#'
#' @details
#' The ratio between width and height is directly based on the ratio between
#' lonigtude and lattitude. In order to get the correct scale for the plot the
#' margins need to be set to 0 (see example).
#'
#' @return
#' Named vector of two, containing the width (for x-axis dimension) and the
#' height (for y-axis dimension).
#'
#' @export
#' @importFrom geosphere distHaversine
#'
#' @examples
#' xlim <- c(13.18, 13.472)
#' ylim <- c(52.46, 52.57)
#' plotDim <- getDimensions(xlim = xlim, ylim = ylim, width = 10)
#' plotDim
#'
#' dev.new(noRStudioGD = TRUE, width = plotDim[1], height = plotDim[2])
#' par(mar = c(0, 0, 0, 0)) # no margins outside of the plot
#' plot(x = xlim, y = ylim, type = "l")
#' text(x = mean(xlim), y = ylim[2],
#'      labels = "Correctly Scaled Line",
#'      pos = 1, cex = 3)
#'
getDimensions <- function(xlim, ylim, width = 10){
  x_dist <- geosphere::distHaversine(
    p1 = c(xlim[1], ylim[1]),
    p2 = c(xlim[2], ylim[1])) / 1000 # in km

  y_dist <- geosphere::distHaversine(
    p1 = c(xlim[2], ylim[1]),
    p2 = c(xlim[2], ylim[2])) / 1000 # in km

  c("width" = width, "height" = y_dist / x_dist * width)
}


