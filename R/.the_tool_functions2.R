MisaColor <- c(
  "gwRef" = rgb(23, 88, 94, maxColorValue = 255),
  "gw5" = rgb(153, 181, 121, maxColorValue = 255),
  "gw2" = rgb(245, 162, 0, maxColorValue = 255),
  "gw1.5" = rgb(233, 134, 39, maxColorValue = 255),
  "gw1" = rgb(217, 91, 62, maxColorValue = 255),
  "gw0.5" = rgb(199, 22, 71, maxColorValue = 255))


interpolateParameter <- function(
  dataFrame,
  max_na_hours = 1 # in hours
){
  # find NA data
  nas <- same_inarow(v = is.na(dataFrame[["oxygen"]]),
                     NA_treatment = "NA")

  # values ready for interpolation
  rfi <- nas[nas$Value == 1 & nas$repeats < 4 * max_na_hours,]

  if(nrow(rfi) > 0){
    for(i in 1:nrow(rfi)){
      before <- dataFrame[["oxygen"]][(rfi$starts_at[i]-1)]
      after <- dataFrame[["oxygen"]][(rfi$ends_at[i]+1)]

      # interpolated values
      new_values <- seq(before, after, length.out = rfi$repeats[i] + 2)
      new_values <- new_values[-c(1, length(new_values))]

      dataFrame[["oxygen"]][rfi$starts_at[i]:rfi$ends_at[i]] <-
        new_values
    }
  }
  print(paste0(levels(as.factor(dataFrame$site)), ": ",
               sum(rfi$repeats), " NA's interpolated"))
  dataFrame
}

same_inarow <- function(
  v, # vector
  NA_treatment = "NA" # turn to character ("NA") or to "0" (FALSE) or "1" (TRUE)
){
  v_extended <- as.character(c(v, 0))
  v_extended[is.na(v_extended)] <- NA_treatment

  # supporting function --------------------------------------------------------
  count_repeats <- function(
    v, # Vector
    beg
  ){
    n <- 0
    for(i in beg:length(v)){
      if(v[i] == v[beg]){
        n <- n + 1
      } else {
        break
      }
    }
    data.frame("Value" = v[beg], "repeats" = n,
               "starts_at" = beg, "ends_at" = i - 1)
  }
  # ----------------------------------------------------------------------------

  rep_list <- list()
  beg <- 1
  list_entry <- 1
  while(beg < length(v_extended)){
    rep_list[[list_entry]] <- count_repeats(v = v_extended, beg = beg)
    beg <- rep_list[[list_entry]]$ends_at + 1
    list_entry <- list_entry + 1
  }

  finalFrame <- do.call(rbind, rep_list)
  finalFrame$Value <- as.character(finalFrame$Value)
  finalFrame
}

summer_time <- function(
  dl, # list with sites
  months = 5:9){
  lapply(dl, function(x){
    x$month <- as.numeric(
      strftime(x = x[["posixDateTime"]],
               format = "%m"))
    x$year <- as.numeric(
      strftime(x = x[["posixDateTime"]],
               format = "%Y"))
    x[x$month %in% months, ]
  })
}

yearly_deficiency_time <- function(
  dataFrame, # df with oxygen data in 15 min intervals and with year column
  oxygen_colname = "oxygen",
  thresholds,
  max_missing = 25 # in % of all potential measurements
){

  years <- min(dataFrame$year):max(dataFrame$year)

  group_list <- lapply(years, function(x)
  {dataFrame[[oxygen_colname]][dataFrame$year == x]})

  group_df <- as.data.frame(t(sapply(group_list, function(x){
    sapply(thresholds, function(th,gl){
      sum(gl <= th, na.rm = TRUE)/4}, gl = x)
  })))


  group_df$unknown <- sapply(group_list, function(x) {
    missing <- round(sum(is.na(x))/length(x) * 100, 0)
    if(missing > max_missing){
      "insufficient Data"
    } else {
      missing
    }
  })

  group_df[group_df$unknown == "insufficient Data",
           -which(colnames(group_df) =="unknown")] <- NA

  group_df$year <- years
  rownames(group_df) <- NULL
  colnames(group_df) <- c(paste0("below_", thresholds), "missing_data", "year")

  group_df
}

yearly_crit_Events <- function(
  dataFrame, # df with oxygen data in 15 min intervals and with year column
  oxygen_colname = "oxygen",
  seperating_hours = 5 * 24,
  deficiency_hours = 0.25,
  thresholds,
  max_missing = 25, # in % of all potential measurements
  use_recovery_value = FALSE,
  recovery_value = 4){

  years <- min(dataFrame$year):max(dataFrame$year)

  group_list <- lapply(years, function(x)
  {dataFrame[[oxygen_colname]][dataFrame$year == x]})

  group_df <- as.data.frame(t(sapply(group_list, function(x){
    sapply(thresholds, function(th, gl){

      possible_starts <- which(gl < th)
      # criteria 1: the following value needs to be below threshold too
      events <- possible_starts[which(gl[possible_starts+1] < th)]
      if(length(events) > 0){
        # criteria 2: events within a defined seperation step are aggregated as one
        event_end <- c(which(diff(events) > 4 * seperating_hours))
        event_start <- c(1, event_end + 1)
        event_end <- c(event_end, length(events))

        if(use_recovery_value){
          a <- data.frame("tBeg" = events[event_start], "tEnd" = events[event_end])
          # possible criteria 3: minimum reached O2 concentration between events
          seperated <- c(TRUE)
          if(nrow(a) > 1){
            for(r in 2:nrow(a)){
              seperated <- c(
                seperated,
                sum(gl[a$tEnd[r - 1]:a$tBeg[r]] >= recovery_value, na.rm = TRUE) > 0)
            }
          }
          a$event <- seperated
          sum(a$event)
        } else {
          length(event_end)
        }
      } else {0}
    } , gl = x)

  })))

  if(nrow(group_df) == 1){
    group_df <- as.data.frame(t(group_df))
  }
  group_df$unknown <- sapply(group_list, function(x) {
    missing <- round(sum(is.na(x))/length(x) * 100, 0)
    if(missing > max_missing){
      "insufficient Data"
    } else {
      missing
    }
  })

  group_df[group_df$unknown == "insufficient Data",
           -which(colnames(group_df) =="unknown")] <- NA
  group_df$year <- years
  rownames(group_df) <- NULL
  colnames(group_df) <- c(paste0("below_", thresholds), "missing_data", "year")

  group_df
}

# Abweichung vom Idealzustand
yearly_negative_deviation <- function(
  dataFrame, # df with oxygen data in 15 min intervals and with year column
  oxygen_colname = "oxygen",
  oxygen_ref,
  max_missing = 25
){

  years <- min(dataFrame$year):max(dataFrame$year)

  # add MUE oxygen to data frame
  dataFrame$oxygen_ref <- oxygen_ref

  # per site data frames with oxygen in MUE and its difference
  group_list <- lapply(years, function(x){
    data.frame("ox_ref" = dataFrame$oxygen_ref[dataFrame$year == x],
               "ox" = dataFrame[[oxygen_colname]][dataFrame$year == x])})

  group_df <- as.data.frame(sapply(group_list, function(x){
    def <- which((x$ox - x$ox_ref <= 0))
    av_data <- sum(!is.na(x$ox) & !is.na(x$ox_ref)) # available data
    res <- (sum(x$ox[def] / x$ox_ref[def])  + # resilience of values below MUE
              (av_data - length(def))) / # res = 1 for every value above MUE
      av_data # devided by the amount of measurements (that are not NA)
    round((1 - res), 2)
  }))

  group_df$unknown <- sapply(group_list, function(x) {
    missing <- round(sum(is.na(x[["ox_ref"]]) | is.na(x[, "ox"])) / nrow(x) * 100,
                     digits = 0)
    if(missing > max_missing){
      "insufficient Data"
    } else {
      missing
    }
  })

  group_df[group_df$unknown == "insufficient Data",
           -which(colnames(group_df) =="unknown")] <- NA
  group_df$year <- years
  rownames(group_df) <- NULL
  colnames(group_df) <- c("below_Ref", "missing_data", "year")

  group_df
}


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
      for(i in 1:length(event_start)){
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
