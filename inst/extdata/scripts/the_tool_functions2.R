


plot_misa_assessment <- function(
  site,
  years,
  thresholds_plot = c(thresholds, "Ref"), # only important for legend
  folder
){

  if(site != "MUE"){
    dev.new(noRStudioGD = TRUE, width = 6, height = 8)
    layout(matrix(data = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 5), nrow = 10,
                  ncol = 1, byrow = TRUE))
    par(mar = c(0, 0, 0, 0), las = 1, xaxs = "i")
    plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
         axes = FALSE, xlab = "", ylab = "")
  }else{
    dev.new(noRStudioGD = TRUE, width = 6, height = 6.5)
    layout(matrix(data = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 5), nrow = 8,                                                                       ncol = 1, byrow = TRUE))
    par(mar = c(0, 0, 0, 0), las = 1, xaxs = "i")
    plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
         axes = FALSE, xlab = "", ylab = "")
  }

  # title and legend
  text(x = 50, y = 60, labels = paste0("MISA-Bewertung - ", site), pos =  3,
       cex = 2)
  loop_no <- 0.2
  for(threshold in thresholds_plot){
    rect(xleft = 16 * loop_no, xright = 16 * loop_no + 15,
         ybottom = 10, ytop = 50, border = NA,
         col = MisaColor[grep(x = names(MisaColor), pattern = threshold)])

    if(is.na(as.numeric(threshold))){
      text(x = 16 * loop_no, y = 30,
           labels = bquote("0"[2] <= ~ .(threshold)), pos = 4)
    } else {
      text(x = 16 * loop_no, y = 30,
           labels = bquote("0"[2] <= ~ .(threshold) ~ "mg/L" ), pos = 4)
    }
    loop_no <- loop_no + 1
  }

  # four plots: 2 x deficiency time, 1 x critical Events, 1 x negative deviation (except MUE)

  par(mar = c(0.1, 6.1, 0.5, 0.5), las = 1)

  plot_defTime(df = deficiencyTime[[site]], plot_thresholds = c(5),
               plot_x = FALSE)
  plot_defTime(df = deficiencyTime[[site]], plot_thresholds = c(0.5, 1, 1.5, 2),
               plot_x = FALSE)
  if(site != "MUE"){
    plot_critEvents(df = critEvents[[site]], plot_x = FALSE)}
  else {
    plot_critEvents(df = critEvents[[site]],
                    plot_x = TRUE)}

  par(mar = c(4.1, 6.1, 0.5, 0.5), las = 1)

  if(site != "MUE"){plot_NegDev(df = NegDev[[site]])
  }

  savePlot(filename = paste0(folder, site), type = "tiff")
  dev.off()
}

plot_defTime <- function(df, plot_thresholds, years = 2000:2019, plot_x = TRUE){

  plot_info <- data.frame("th" = plot_thresholds)
  plot_info$colums <- sapply(plot_thresholds, function(threshold){
    grep(pattern = paste0("_", threshold, "$"), x = colnames(df))
  })
  plot_info$colors <- sapply(plot_thresholds, function(threshold){
    grep(pattern = paste0("gw", threshold, "$"), x = names(MisaColor))
  })

  ymax <- max(df[, plot_info$colums], na.rm = TRUE)

  if(ymax == 0){
    ymax <- 4
  }

  plot(x = 0, y = 0,
       xlim = c(min(years) - 0.5, max(years) + 0.5),
       ylim = c(0, ymax),
       type = "n", xaxt = "n",
       ylab = "", xlab = "")
  if(plot_x){
    axis(1, at = years, labels = years,
         las = 2)
  }
  axis(2, at = mean(par("yaxp")[1:2]), labels = "Unterschreitungsdauer [h]",
       las = 0, line = 3, lwd.ticks = 0)


  missing <- which(df$missing_data == "insufficient Data")
  if(length(missing) > 0){
    rect(xleft =  df$year[missing] - 0.4,
         xright =  df$year[missing] + 0.4,
         ybottom = 0, ytop = ymax,
         col = "gray80", border = NA)
  }

  for(i in nrow(plot_info):1){
    rect(xleft =  df$year - 0.4,
         xright =  df$year + 0.4,
         ybottom = 0, ytop = df[, plot_info$colums[i]],
         col = MisaColor[plot_info$colors[i]], border = NA)
  }
}


plot_critEvents <- function(df, years = 2000:2019, plot_x = TRUE){

  th <- unlist(strsplit(colnames(df)[1], split = "_"))[2]
  th_col <- grep(pattern = paste0("gw", th, "$"), x = names(MisaColor))

  ymax <- max(df[, 1], na.rm = TRUE)

  if(ymax < 4){
    ymax <- 4
  }

  plot(x = 0, y = 0,
       xlim = c(min(years) - 0.5, max(years) + 0.5),
       ylim = c(0, ymax),
       type = "n", xaxt = "n",
       ylab = "", xlab = "")
  if(plot_x){
    axis(1, at = years, labels = years,
         las = 2)
  }
  axis(2, at = mean(par("yaxp")[1:2]), labels = "Anzahl kritischer Ereignisse",
       las = 0, line = 3, lwd.ticks = 0)

  missing <- which(df$missing_data == "insufficient Data")
  if(length(missing) > 0){
    rect(xleft =  df$year[missing] - 0.4,
         xright =  df$year[missing] + 0.4,
         ybottom = 0, ytop = ymax,
         col = "gray80", border = NA)
  }

  rect(xleft =  df$year - 0.4,
       xright =  df$year + 0.4,
       ybottom = 0, ytop = df[, 1],
       col = MisaColor[th_col], border = NA)
}

plot_NegDev <- function(df, years = 2000:2019, plot_x = TRUE){

  th <- unlist(strsplit(colnames(df)[1], split = "_"))[2]
  th_col <- grep(pattern = paste0("gw", th, "$"), x = names(MisaColor))

  ymax <- max(df[,1], na.rm = TRUE)

  plot(x = 0, y = 0,
       xlim = c(min(years) - 0.5, max(years) + 0.5),
       ylim = c(0, ymax) * 100,
       type = "n", xaxt = "n",
       ylab = "", xlab = "")
  if(plot_x){
    axis(1, at = years, labels = years,
         las = 2)
  }
  axis(2, at = mean(par("yaxp")[1:2]),
       labels = "Negative Abweichung\nvom Referenzzustand [%]",
       las = 0, line = 2, lwd.ticks = 0)

  missing <- which(df$missing_data == "insufficient Data")
  if(length(missing) > 0){
    rect(xleft =  df$year[missing] - 0.4,
         xright =  df$year[missing] + 0.4,
         ybottom = 0, ytop = ymax * 100,
         col = "gray80", border = NA)
  }

  rect(xleft =  df$year - 0.4,
       xright =  df$year + 0.4,
       ybottom = 0, ytop = df[, 1] * 100,
       col = MisaColor[th_col], border = NA)
}

list_CritEvents <- function(
  dataFrame, # df with oxygen data in 15 min intervals and with year column for one site
  oxygen_colname = "Sauerstoff",
  seperating_hours = 5 * 24,
  thresholds,
  year
){

  df_filtered <- dataFrame[dataFrame$year == year,]

  fdl <- lapply(thresholds, function(th){

    possible_starts <- which(df_filtered[[oxygen_colname]] < th)

    # criteria 1: the following value needs to be below threshold too
    events <-
      possible_starts[which(df_filtered[[oxygen_colname]][possible_starts+1] < th)]

    if(length(events) > 0){
      # criteria 2: events within a defined seperation step are aggregated as one
      event_end <- c(which(diff(events) > 4 * seperating_hours))
      event_start <- c(1,event_end+1)
      event_end <- c(event_end, length(events))

      event_dl <- list()
      for(i in seq_along(event_start)){
        event_dl[[i]] <-
          df_filtered[(events[event_start][i]):(events[event_end][i]),]
      }
      event_dl
    }
    else {0}
  }
  )
  names(fdl) <- paste0("th_", thresholds)
  fdl$seperating_hours <- seperating_hours
  fdl
}
