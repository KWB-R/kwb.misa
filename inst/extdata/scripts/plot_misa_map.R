load(file = "inst/extdata/colors/misaColor.RData")
load(file = "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/input/ezg_polygone.RData")
load(file = "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/output/vor_Sanierung.RData")

if(FALSE){
  rivers <- kwb.misa::load_berlin_rivers()
  xlim <- c(13.2, 13.463)
  ylim <- c(52.46, 52.57)

  plotDim <- plotDimensions(
    xlim = xlim, ylim = ylim , width = 14)


  # Plot Basis
  {
    dev.new(noRStudioGD = T, width = plotDim["width"], height = plotDim["height"])
    par(mar = c(0,0,0,0))
    plot(x = 0, y = 0, type = "n", xlab = "", ylab = "",
         xlim = xlim, ylim = ylim)
    plot_catchments(catchmentList = ezg)
  }

  {  # single event (time) --------------------
    sixBreaks <- c(0,0.5,2,4,10,20,Inf)
    prepared_rivers <- lapply(rivers, prepare_riverPlot,
                              qsim_misa_table = dl_misa$E1$hours,
                              varName = "below_1.5",
                              sixBreaks = sixBreaks)
    plot_river_col(prepared_rivers = prepared_rivers , sixBreaks = sixBreaks,
                   dataType = "time", LegendTitle = "1,5 mg/L Unterschreitung in Stunden")
  }
  {  # single event (number) --------------------
    sixBreaks <- c(-1,0,1)
    prepared_rivers <- lapply(rivers, prepare_riverPlot,
                              qsim_misa_table = dl_misa$E1$events,
                              varName = "below_1.5",
                              sixBreaks = sixBreaks)
    plot_river_col(prepared_rivers = prepared_rivers , sixBreaks = sixBreaks,
                   dataType = "number", LegendTitle = "Anzahl kritischer Ereignisse")
  }
  {  # all events (number) --------------------
    sixBreaks <- c(-1,0,1,2,5,10, Inf)
    prepared_rivers <- lapply(rivers, prepare_riverPlot,
                              qsim_misa_table = df_aggr,
                              varName = "events",
                              sixBreaks = sixBreaks)
    plot_river_col(prepared_rivers = prepared_rivers , sixBreaks = sixBreaks,
                   dataType = "number", LegendTitle = "Anzahl kritischer Ereignisse")
  }
  {  # all events (time) --------------------
    sixBreaks <- c(0,25,50,100,200,300, Inf)
    prepared_rivers <- lapply(rivers, prepare_riverPlot,
                              qsim_misa_table = df_aggr,
                              varName = "hours.below_1.5",
                              sixBreaks = sixBreaks)
    plot_river_col(prepared_rivers = prepared_rivers , sixBreaks = sixBreaks,
                   dataType = "time", LegendTitle = "Unterschreitung (1,5 mg/L) in h")
  }
}


# Functions --------------------------------------------------------------------
plotDimensions <- function(xlim, ylim, width = 10){
  x_dist <- geosphere::distHaversine(
    p1 = c(xlim[1], ylim[1]),
    p2 = c(xlim[2], ylim[1])) / 1000 # in km

  y_dist <- geosphere::distHaversine(
    p1 = c(xlim[2], ylim[1]),
    p2 = c(xlim[2], ylim[2])) / 1000 # in km

  c("width" = width, "height" = y_dist / x_dist * width)
}

plot_catchments <- function(catchmentList, col = "gray90"){
  for(i in 1:length(catchmentList)){
    polygon(x = catchmentList[[i]][,1],
            y = catchmentList[[i]][,2], col = col)
    text(x = mean(catchmentList[[i]][,1]),
         y = mean(catchmentList[[i]][,2]),
         labels = names(catchmentList)[i])
  }
}

prepare_riverPlot <- function(river_table, qsim_misa_table, varName, sixBreaks){
  river_table$value <- NA

  for(i in 1:nrow(qsim_misa_table)){
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
        include.lowest = T, ordered_result = T)

  river_table$color <- MisaColor[as.numeric(river_table$quality)]
  river_table
}



plot_river_col <- function(
  prepared_rivers, sixBreaks, dataType = "time", LegendTitle){
  for(j in 1:length(prepared_rivers)){
    lines(x = prepared_rivers[[j]]$x, y = prepared_rivers[[j]]$y)
    for(i in 1:(nrow(prepared_rivers[[j]]) - 1)){
      lines(x = prepared_rivers[[j]]$x[i:(i+1)],
            y = prepared_rivers[[j]]$y[i:(i+1)],
            col = prepared_rivers[[j]]$color[i+1],
            lwd = 6)
    }
  }
  ll <- length(sixBreaks)

  if(dataType == "time"){
    legend("bottomleft",
           legend = c(
             paste0("< ", sixBreaks[2]),
             paste0("> ", sixBreaks[2:(ll-1)])),
           col = MisaColor[1:ll], lwd = 6,
           title = LegendTitle, cex = 0.8, bg = "white")
  } else {
    legend("bottomleft",
           legend = c(
             paste0("<= ", sixBreaks[2:(ll-1)]),
             paste0(">", sixBreaks[(ll-1)])),
           col = MisaColor[1:ll], lwd = 6,
           title = LegendTitle, cex = 0.8, bg = "white")
  }
}

