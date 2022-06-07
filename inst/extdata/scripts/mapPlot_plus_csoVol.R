# reads the "stats" output from the misa interface
# and plots the local distribution of cso
# amount per rain event

options(OutDec = ",")
if(FALSE){
  path <- paste0(
    "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/",
    "AP1_Vorbereitung-Strategiebewertung/Schnittstelle/output/vor_sanierung")
  saving_path <- paste0(
    "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/",
    "AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/output/vor_sanierung")

  all_files <- dir(path)
  stat_files <- grep(pattern = "stats.csv$", all_files, value = T)
  # load misa assessment
  load("Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/output/vor_sanierung.RData")

  # load Catchment polygons
  load("inst/extdata/bwb_catchments/catch_polygon.RData")

  # load Color scale
  load(file = "inst/extdata/colors/misaColor.RData")
  rivers <- kwb.misa::load_berlin_rivers()

  xlim <- c(13.18, 13.472)
  ylim <- c(52.46, 52.57)
  plotDim <- getDimensions(xlim = xlim, ylim = ylim, width = 10)
  width_factor <- plotDim[1]/plotDim[2]

  for(stat_file in stat_files){
    # peparations ---------------------------------------------------------------
    p_title <- strsplit(x = stat_file, split = "_")[[1]][1]

    if(!is.null(dl_misa[[p_title]]$hours)){
      df <- read.csv(file = file.path(path, stat_file),
                     header = TRUE, sep = ";", dec = ".")

      df_plot <- df[!is.na(df$tBeg),]
      sizeMax <- 100000

      # single event (time) --------------------
      sixBreaks <- c(0,0.5,2,4,10,20,Inf)
      prepared_rivers <- lapply(rivers, prepare_riverPlot,
                                qsim_misa_table = dl_misa[[p_title]]$hours,
                                varName = "below_1.5",
                                sixBreaks = sixBreaks)

      # plot -----------------------------------------------------------------------
      png(filename = paste0(saving_path, "/",p_title, ".png"),
          height = 6, width = 6 * width_factor, units = "in", res = 300)
      xpdDim <- 6
      par(mar = c(xpdDim / 2, 0.2, xpdDim / 2 , xpdDim * width_factor - 0.2))

      plot(x = 0, y = 0, xaxt = "n", yaxt = "n", type = "n", xaxs = "i", yaxs = "i",
           xlab = "", ylab = "", xlim = xlim, ylim = ylim,
           main = paste0("MWÜ und Gewässerbelastung - ", p_title))

      add_catchments(ezg = ezg)

      plot_river_col(prepared_rivers = prepared_rivers , sixBreaks = sixBreaks,
                     dataType = "time", LegendLocation = "left",
                     LegendTitle = "Unterschreitungsdauer\n in Stunden (1,5 mg/L)")

      points(x = df$lon_grad + df$lon_min / 60 + df$lon_s / 3600,
             y = df$lat_grad + df$lat_min / 60 + df$lat_s / 3600,
             pch = 21,
             cex = sqrt(df$tVol_m3 /sizeMax)  * 7,
             bg = rgb(0,0,0, 0.5), col = "white")

      abline(h = ylim)
      abline(v = xlim)

      legend(x = par("usr")[2], y = par("usr")[4], xpd = T,
             legend = c("1 000","25 000","50 000"),
             pch = 21, pt.cex = c(sqrt(1000 /sizeMax) * 7,
                                  sqrt(25000 /sizeMax) * 7 ,
                                  sqrt(50000 /sizeMax) * 7),
             title = "Überlaufvolumen in m³", bg = "white", bty = "n",
             x.intersp = 2, y.intersp = 2, pt.bg = rgb(0,0,0,0.5), col = "white")
      dev.off()
    }
  }
  # all events (number) --------------------
  {
    sixBreaks <- c(-1,0,1,3,6,10, Inf)
    prepared_rivers <- lapply(rivers, prepare_riverPlot,
                              qsim_misa_table = df_aggr,
                              varName = "events",
                              sixBreaks = sixBreaks)

    unique(prepared_rivers$Spree$quality)
    # Plot
    png(filename = paste0(saving_path, "/all_events_deficitNumber", ".png"),
        height = 6, width = 6*width_factor, units = "in", res = 300)
    xpdDim <- 4
    par(mar = c(0.2, xpdDim * width_factor / 2, xpdDim - 0.2 , xpdDim * width_factor / 2))

    plot(x = 0, y = 0, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         xlim = xlim, ylim = ylim)

    add_catchments(ezg = ezg)

    abline(h = ylim)
    abline(v = xlim)

    plot_river_col(prepared_rivers = prepared_rivers , sixBreaks = sixBreaks,
                   dataType = "number", LegendLocation = "top",
                   LegendTitle = "Anzahl kritischer Ereignisse")

    dev.off()
  }

  # all events (time) --------------------
  {
  sixBreaks <- c(0,25,50,100,200,300, Inf)
  prepared_rivers <- lapply(rivers, prepare_riverPlot,
                            qsim_misa_table = df_aggr,
                            varName = "hours.below_1.5",
                            sixBreaks = sixBreaks)

  unique(prepared_rivers$Spree$quality)
  # Plot
  png(filename = paste0(saving_path, "/all_events_deficitTime", ".png"),
      height = 6, width = 6*width_factor, units = "in", res = 300)
  xpdDim <- 4
  par(mar = c(0.2, xpdDim * width_factor / 2, xpdDim - 0.2 , xpdDim * width_factor / 2))

  plot(x = 0, y = 0, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       xlim = xlim, ylim = ylim)

  add_catchments(ezg = ezg)

  abline(h = ylim)
  abline(v = xlim)

  plot_river_col(prepared_rivers = prepared_rivers , sixBreaks = sixBreaks,
                 dataType = "time", LegendLocation = "top",
                 LegendTitle = "Unterschreitungsdauer in Stunden (1,5 mg/L)")
  dev.off()
  }
}


# Functions --------------------------------------------------------------------

prepare_riverPlot <- function(river_table, qsim_misa_table, varName, sixBreaks){
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
        include.lowest = T, ordered_result = T)

  river_table$color <- MisaColor[as.numeric(river_table$quality)]
  river_table
}



plot_river_col <- function(
  prepared_rivers, sixBreaks, dataType = "time", LegendTitle, LegendLocation){
  for(j in seq_along(prepared_rivers)){
    lines(x = prepared_rivers[[j]]$x, y = prepared_rivers[[j]]$y,
          col = "steelblue")
    for(i in seq_len(nrow(prepared_rivers[[j]]) - 1)){
      lines(x = prepared_rivers[[j]]$x[i:(i+1)],
            y = prepared_rivers[[j]]$y[i:(i+1)],
            col = prepared_rivers[[j]]$color[i+1],
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

add_catchments <- function(ezg){

  {# Positions of catchment names in the plot
    ezg_namePositions <- lapply(ezg, function(loc_df) {
      data.frame("x" = mean(loc_df[,1]), "y" =  mean(loc_df[,2]))
    })
    ezg_namePositions$`Bln II`$y <-  ezg_namePositions$`Bln II`$y - 0.01
    ezg_namePositions$`Bln III`$x <-  ezg_namePositions$`Bln III`$x + 0.01
    ezg_namePositions$`Bln III`$y <-  ezg_namePositions$`Bln III`$y - 0.003
    ezg_namePositions$`Bln IIIa`$x <-  ezg_namePositions$`Bln IIIa`$x - 0.037
    ezg_namePositions$`Bln IIIa`$y <-  ezg_namePositions$`Bln IIIa`$y - 0.0014
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
  lines(x = c(ezg_namePositions$`Bln IIIa`$x + 0.013, mean(ezg$`Bln IIIa`[,1]) + 0.0005),
        y =  c(ezg_namePositions$`Bln IIIa`$y, mean(ezg$`Bln IIIa`[,2])))
}

getDimensions <- function(xlim, ylim, width = 10){
  x_dist <- geosphere::distHaversine(
    p1 = c(xlim[1], ylim[1]),
    p2 = c(xlim[2], ylim[1])) / 1000 # in km

  y_dist <- geosphere::distHaversine(
    p1 = c(xlim[2], ylim[1]),
    p2 = c(xlim[2], ylim[2])) / 1000 # in km

  c("width" = width, "height" = y_dist / x_dist * width)
}


