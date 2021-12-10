load("inst/extdata/colors/misaColor.RData")

path <- "C:/Users/mzamzo/Documents/misa/phase_3/gewassersimulationen/"
df_in <- read.csv(file = paste0(path, "MiSa3_Auswertetool.csv"),
         sep = ";", dec = ",")

df_in$site <- paste(df_in$Strang, df_in$Km, sep = "_")
df_in$posixDateTime <- as.POSIXct(df_in$DatumUhrzeit, format = "%d.%m.%Y %H:%M")
df_in$oxygen <- df_in$VO2
df_in <- df_in[order(df_in$Km, decreasing = T),]
data_comp <- df_in[,c("posixDateTime", "oxygen", "site")]

# 1. Read Data
# data_comp <- kwb.misa::read_misa_files(
#   input_path = "inst/extdata/")

# 2. Filter Data
data_comp_f <- kwb.misa::misa_filter_data(
  dataFrame = data_comp,
  tBeg = "",
  tEnd = "",
  sites = "") # all sites are included

# 3. Manipulated Data
dl <- kwb.misa::misa_prepare_data(
  df_MiSa = data_comp_f,
  res = 15, # temporal resolution in minutes
  max_na_interpolation = 60/15) # 4 missing values a 26 mins  -> one hour max

# 4. Assess Data
defTime <- do.call(rbind, lapply(X = dl, kwb.misa::yearly_deficiency_time))
defEvent <- do.call(rbind, lapply(X = dl, kwb.misa::yearly_crit_Events))
negDev <- do.call(rbind, lapply(X = dl, kwb.misa::yearly_negative_deviation,
                      oxygen_ref = dl[["S106-SOW_21.2"]]$d))

# order according to km
defTime <- defTime[order(
  as.numeric(sapply(rownames(defTime), function(x){strsplit(x, "_")[[1]][2]})),
  decreasing = TRUE),]
CHA <- which(rownames(defTime) == "S305-SOW_7.3")

par(mar = c(6.1, 4.1, 3.1, 1.1))
{
  plot(x = 0, y = 0, type = "n", ylab ="Unterschreitungsdauer [h]", xlab = "",
       xaxt = "n", ylim = c(0, max(defTime[,paste0("below_", c(0.5, 1, 1.5, 2))])),
       xlim = c(0.5, nrow(defTime) + 0.5), xaxs = "i")
  axis(side = 1, at = 1:nrow(defTime), labels = rownames(defTime),
       las = 2, cex.axis = 0.6)

  for(i in 1:nrow(defTime)){
    rect(xleft = 0.6 + (i - 1), xright = 1.4 + (i - 1),
         ybottom = 0, ytop = defTime$below_2[i], col = MisaColor["acceptable"],
         border = NA)
    rect(xleft = 0.6 + (i - 1), xright = 1.4 + (i - 1),
         ybottom = 0, ytop = defTime$below_1.5[i], col = MisaColor["bad"],
         border = NA)
    rect(xleft = 0.6 + (i - 1), xright = 1.4 + (i - 1),
         ybottom = 0, ytop = defTime$below_1[i], col = MisaColor["critical"],
         border = NA)
    rect(xleft = 0.6 + (i - 1), xright = 1.4 + (i - 1),
         ybottom = 0, ytop = defTime$below_0.5[i], col = MisaColor["very serious"],
         border = NA)
  }
  abline(v = c(0.6, 1.4) + (CHA - 1), col = "blue", xpd = T)
}

{
  plot(x = 0, y = 0, type = "n", ylab ="Unterschreitungsdauer [h]", xlab = "",
       xaxt = "n", ylim = c(0, max(defTime[,"below_5"])),
       xlim = c(0.5, nrow(defTime) + 0.5), xaxs = "i")
  axis(side = 1, at = 1:nrow(defTime), labels = rownames(defTime),
       las = 2, cex.axis = 0.6)

  for(i in 1:nrow(defTime)){
    rect(xleft = 0.6 + (i - 1), xright = 1.4 + (i - 1),
         ybottom = 0, ytop = defTime$below_5[i], col = MisaColor["good"],
         border = NA)
  }
  abline(v = c(0.6, 1.4) + (CHA - 1), col = "blue", xpd = T)
}

negDev <- negDev[order(
  as.numeric(sapply(rownames(negDev), function(x){strsplit(x, "_")[[1]][2]})),
  decreasing = TRUE),]
CHA <- which(rownames(defTime) == "S305-SOW_7.3")

{
  plot(x = 0, y = 0, type = "n", ylab ="Negative Abweichung zur Referenz (km 21,2)",
       xlab = "",
       xaxt = "n", ylim = c(0, max(negDev$neg_deviation_relative)),
       xlim = c(0.5, nrow(defTime) + 0.5), xaxs = "i")
  axis(side = 1, at = 1:nrow(defTime), labels = rownames(defTime),
       las = 2, cex.axis = 0.6)

  for(i in 1:nrow(defTime)){
    rect(xleft = 0.6 + (i - 1), xright = 1.4 + (i - 1),
         ybottom = 0, ytop = negDev$neg_deviation_relative[i], col = MisaColor["perfect"],
         border = NA)
  }
  abline(v = c(0.6, 1.4) + (CHA - 1), col = "blue", xpd = T)
}
