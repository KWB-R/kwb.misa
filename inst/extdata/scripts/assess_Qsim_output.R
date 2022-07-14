# Prepare Qsim -----------------------------------------------------------------
path <- file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages",
  "AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/qsim_output/alt/vor_sanierung_alt")
files <- dir(path = path)
files <- grep(pattern = "^2", files, value = T)
scenario <- rev(strsplit(x = path, split = "/")[[1]])[1]


for(file in files){
  print(paste(file, "in progress"))
  kwb.misa::QSIM_prepare_for_tool(
    qsim_output_path = path,
    qsim_fileName = file,
    misa_tool_input_path = file.path(
      "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages",
      "AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/input"),
    output_fileName = paste0("misa_", scenario, "_", file))
  print("done")
}

# write flow tables
for(file in files){
  print(paste(file, "in progress"))
  kwb.misa::QSIM_get_flow(
    qsim_output_path = path,
    qsim_fileName = file,
    save_path = file.path(
      "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages",
      "AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/qsim_output",
      "flow_events"))
  print("done")
}


# List of Events ---------------------------------------------------------------
# the input folder including the document "Ereignisreihe"
path <- paste0(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/",
  "AP1_Vorbereitung-Strategiebewertung/Misa_auswertung")

# 1. timeframe of events
e_data <- read.table(file = file.path(path,"input", "Ereignisreihe.csv"),
                     header = T, sep = ";", dec = ".")
e_data$tBeg <- as.POSIXct(e_data$tBeg, format = "%d.%m.%Y %H:%M")
e_data$tEnd <- as.POSIXct(e_data$tEnd, format = "%d.%m.%Y %H:%M") + 5 * 24 * 60 * 60
es <- lapply(1:nrow(e_data), function(i){
  c("tBeg" = e_data$tBeg[i], "tEnd" = e_data$tEnd[i])
})
# ------------------------------------------------------------------------------


# 1. Read Oxygen Data
data_comp <- kwb.misa::read_misa_files(
  input_path = file.path(path, "input"))

# 2. Filter Data per event
data_comp_per_event <- lapply(es, function(event){
  data_comp_f <- kwb.misa::misa_filter_data(
    dataFrame = data_comp,
    tBeg = event["tBeg"],
    tEnd = event["tEnd"],
    sites = "") # all sites are included
})

names(data_comp_per_event) <- e_data$X

dl_misa <- lapply(data_comp_per_event, function(df_event){
  print(head(df_event))
  # 3. Manipulated Data
  dl <- kwb.misa::misa_prepare_data(
    df_MiSa = df_event,
    res = 15, # temporal resolution in minutes
    max_na_interpolation = 60/15) # 4 missing values a 15 mins  -> one hour max


  # 4. Assess Data
  list(
    "hours" = do.call(rbind, lapply(X = dl, kwb.misa::yearly_deficiency_time,
                                    max_missing = 100, thresholds = c(0.5, 1, 1.5, 2, 3))),
    "events" = do.call(rbind, lapply(X = dl, kwb.misa::yearly_crit_Events, max_missing = 100)),
    "neg_dev" = do.call(rbind, lapply(X = dl, kwb.misa::yearly_negative_deviation,
                                      oxygen_ref = dl[["SOW_S106.SOW_21.2"]]$d, max_missing = 100)))
})

# 5. Add site info to the misa assessment (based on row names)
dl_misa <- lapply(dl_misa, add_site_info)

# 6. Aggregate events
indices <- seq_along(dl_misa)
df_aggr <- data.frame(
  "hours" = sapply(paste0("below_", c(0.5, 1, 1.5, 2, 3)), function(x){
    rowSums(data.frame(sapply(X = indices, function(i){
      v <- dl_misa[[i]]$hours[[x]]
      if(is.null(v)){
        NA
      } else {v}
    })), na.rm = T)
  }),
  "events" = rowSums(data.frame(sapply(X = indices, function(i){
    v <- dl_misa[[i]]$events$below_1.5
    if(is.null(v)){
      NA
    } else {v}
  })), na.rm = T),
  "neg_dev" = rowMeans(data.frame(sapply(X = indices, function(i){
    v <- dl_misa[[i]]$neg_dev$neg_deviation_relative
    if(is.null(v)){
      NA
    } else {v}
  })), na.rm = T)
)

df_aggr$qsim_site <- rownames(dl_misa[[2]]$hours)



rm(list = setdiff(x = ls(), list("df_aggr", "dl_misa")))




# 6. Plot data
load(file = "inst/extdata/colors/misaColor.RData")
#########################
add_site_info <- function(misa_assessment){
  lapply(misa_assessment, function(x){
    if(is.data.frame((x))){
      x[["river"]] <-  sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][1]})

      x[["section"]] <-  sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][2]})

      x[["km"]] <-  as.numeric(sapply(rownames(x), function(NAME){
        strsplit(NAME, split = "_")[[1]][3]}))
      x[order(x$km, decreasing = T),]
    }
  })
}





plot_defTime <- function(df, river, good_water_treshold = FALSE,
                         plot_x = TRUE){

  cols <- if(good_water_treshold){
    5
  } else {
    1:4
  }
  df <- df[df$river == river,]
  load(file = "inst/extdata/colors/misaColor.RData")

  ymax <- max(df[,cols], na.rm = T)

  plot(x = 0, y = 0,
       xlim = c(1, nrow(df)),
       ylim = c(0,ymax),
       type = "n", xaxt = "n",
       ylab = "", xlab = "")

  if(plot_x){
    axis(1, at = 1:nrow(df), labels = paste(df$km, df$section),
         las = 2)
  }
  axis(2, at = mean(par("yaxp")[1:2]), labels = "Unterschreitungsdauer [h]",
       las = 0, line = 3,lwd.ticks = 0)

  missing <- which(df$missing_data == "insufficient Data")
  if(length(missing)){
    rect(xleft =  (1:nrow(df))[missing] - 0.4,
         xright = (1:nrow(df))[missing] + 0.4,
         ybottom = 0, ytop = ymax,
         col = "gray80", border = NA)
  }

  for(i in rev(cols)){
    rect(xleft =  1:nrow(df) - 0.4,
         xright =  1:nrow(df) + 0.4,
         ybottom = 0, ytop = df[,i],
         col = MisaColor[-1 *i + 7], border = NA)
  }
}

plot_fDeath <- function(df, river, plot_x = T){

  df <- df[df$river == river,]
  load(file = "inst/extdata/colors/misaColor.RData")
  th_col <- MisaColor["bad"]

  ymax <- max(df[,1], na.rm = T)
  plot(x = 0, y = 0,
       xlim = c(1, nrow(df)),
       ylim = c(0,ymax),
       type = "n", xaxt = "n",
       ylab = "", xlab = "")
  if(plot_x){
    axis(1, at = 1:nrow(df), labels = paste(df$km, df$section),
         las = 2)
  }
  axis(2, at = mean(par("yaxp")[1:2]), labels = "Anzahl kritischer O2-Ereignisse",
       las = 0, line = 3,lwd.ticks = 0)

  missing <- which(df$missing_data == "insufficient Data")
  if(length(missing) > 0){
    rect(xleft =  (1:nrow(df))[missing] - 0.4,
         xright =  (1:nrow(df))[missing] + 0.4,
         ybottom = 0, ytop = ymax,
         col = "gray80", border = NA)
  }

  rect(xleft = (1:nrow(df)) - 0.4,
       xright = (1:nrow(df)) + 0.4,
       ybottom = 0, ytop = df[,1],
       col = th_col, border = NA)
}

plot_NegDev <- function(df, river, plot_x = T){

  df <- df[df$river == river,]

  th_col <- MisaColor["perfect"]

  ymax <- max(df[,1], na.rm = T)

  plot(x = 0, y = 0,
       xlim = c(1, nrow(df)),
       ylim = c(0,ymax)*100,
       type = "n", xaxt = "n",
       ylab = "", xlab = "")
  if(plot_x){
    axis(1, at = 1:nrow(df), labels = paste(df$km, df$section),
         las = 2)
  }
  axis(2, at = mean(par("yaxp")[1:2]),
       labels = "Negative Abweichung\nvom Referenzzustand [%]",
       las = 0, line = 2,lwd.ticks = 0)

  rect(xleft = 1:nrow(df) - 0.4,
       xright = 1:nrow(df) + 0.4,
       ybottom = 0, ytop = df[,1]*100,
       col = th_col, border = NA)
}
###########################
event <- "E13"
river <- "SOW"

for(event in names(dl_misa)){

  dl_in <- dl_misa[[event]]
  if(length(dl_in$hours)){
    png(filename = file.path(path, "output", paste0(event,"_", river, ".png")),
        width = 6, height = 8, units = "in", res = 300)
    layout(mat = matrix(data = c(1,2,2,3,3,4,4,5,5,5), nrow = 10, ncol = 1))
    par(cex.axis = 0.8)

    par(mar = c(0,0,0,0))
    th <- as.numeric(
      sapply(colnames(dl_in$hours)[1:5], function(x){strsplit(x, "_")[[1]][2]}))
    plot(0,0, type = "n", axes = F, xlim = c(0,1), ylim = c(0,1), xaxs = "i", yaxs = "i")
    rect(xleft = seq(0.008,1,1/6), xright = seq(0.008,1,1/6) + 0.15,
         ybottom = 0.2, ytop = 0.8, col = rev(MisaColor), border = NA)

    text(x = seq(0.083,0.8,1/6), y = 0.61,
         labels = gsub(pattern = "\\.", replacement = ",", x = th), col = "white")
    text(x = seq(0.083,0.8,1/6), y = 0.38,
         labels = "mg/L", col = "white")
    text(x = 5/6+0.083, y = 0.61,
         labels = "Bezogen auf", col = "white")
    text(x = 5/6+0.083, y = 0.38,
         labels = "Referenz", col = "white")

    par(mar = c(1.1, 8.1, 0.5, 0.5), las = 1)

    plot_defTime(df = dl_in$hours, river = river,
                 good_water_treshold = TRUE, plot_x = FALSE)
    plot_defTime(df = dl_in$hours, river = river,
                 good_water_treshold = FALSE, plot_x = FALSE)
    plot_fDeath(df = dl_in$events, river = river, plot_x = FALSE)

    par(mar = c(8.1, 8.1, 0.5, 0.5), las = 1)

    plot_NegDev(df = dl_in$neg_dev, river = river, plot_x = T)

    dev.off()
  }

}

df <- dl_misa$E13$hours
plot_thresholds <- c(5)
plot_thresholds <- c(0.5,1,1.5,2)
river <- "SOW"









