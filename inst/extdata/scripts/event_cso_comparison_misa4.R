# reads the "stats" output from the misa interface for two different scenarios
# and plots the local distribution of cso volume difference
# Delta = Variante 1 - Variante 2
scenario_path <- file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages",
  "AP3_Szenarienrechnung/berechnungen")


# Varienten
var1 <- "S0"
var2 <- "S5"

# Path for saving
saving_path <- file.path(interfacePath, "_plausibilisierung")

# look for files
{
  path <- file.path(interfacePath, var1, "2_interface_output")
  all_files <- dir(path, full.names = TRUE)
  stat_files1 <- grep(pattern = "stats.csv$", all_files, value = TRUE)
  path <- file.path(interfacePath, var2, "2_interface_output")
  all_files <- dir(path, full.names = TRUE)
  stat_files2 <- grep(pattern = "stats.csv$", all_files, value = T)
}

df_all <- data.frame("nEvents" = 0,
                     "vol_nInc" = 0,
                     "vol_nDec" = 0,
                     "vol_sumDelta" = 0,
                     "max_Inc" = 0,
                     "maxInvEvent" = NA,
                     "max_Dec" = 0,
                     "maxDecEvent" = NA)

for(event in 1:20){
  stat_file1 <- grep(pattern = paste0("E", event, "_"), stat_files1, value = T)
  stat_file2 <- grep(pattern = paste0("E", event, "_"), stat_files2, value = T)

  if(length(stat_file1) > 0L & length(stat_file2) > 0L){
    df1 <- read.csv(file = stat_file1, header = TRUE, sep = ";", dec = ".")
    df2 <- read.csv(file = stat_file2, header = TRUE, sep = ";", dec = ".")

    df_compare <-
      merge(df1, df2, by = "outlet_id", all = T, suffixes = c("_1", "_2"))

    ################################# Volumen ####################################
    # "vor Sanierung" minus "nach Sanierung"
    sizeValue <- df_compare$tVol_m3_1 - df_compare$tVol_m3_2
    df_out <- data.frame("outlet_id" = df_compare$outlet_id,
                         "deltaVol_m3" = sizeValue)

    df_all$nEvents <- df_all$nEvents + 1
    df_all$vol_nInc <- df_all$vol_nInc + sum(sizeValue < 0, na.rm = TRUE)
    df_all$vol_nDec <- df_all$vol_nInc + sum(sizeValue > 0, na.rm = TRUE)
    df_all$vol_sumDelta <- df_all$vol_sumDelta + sum(sizeValue, na.rm = TRUE)
    mi <- min(sizeValue, na.rm = TRUE)
    ma <- max(sizeValue, na.rm = TRUE)
    df_all$maxInvEvent <- ifelse(mi < df_all$max_Inc,
                                 yes = event,
                                 no = df_all$maxInvEvent)
    df_all$max_Inc <- min(c(df_all$max_Inc, mi))

    df_all$maxDecEvent <- ifelse(ma > df_all$max_Dec,
                                 yes = event,
                                 no = df_all$maxDecEvent)
    df_all$max_Dec <- max(c(df_all$max_Dec, ma))



    sizeMax <- 100000
    # Falls Differenz positiv, dann war das Volumen vorher größer
    # (--> Hat durch Sanierung abgenommen, wie erwartbar)
    colVector <- ifelse(test = sizeValue > 0, "forestgreen", "orange")
    sizeValue <- abs(sizeValue)

    png(filename = paste0(saving_path, "/E", event, "_Vol.png"),
        height = 7, width = 11, units = "in", res = 300)
    plot(x = df1$y.coordinate,
         y = df1$x.coordinate,
         pch = 21,
         cex = sqrt(sizeValue /sizeMax)  * 7,
         xaxt = "n", yaxt = "n",
         bg = colVector,
         xlab = "", ylab = "", main = paste0("MWÜ Volumen - E", event)
    )

    legend("bottomleft",
           legend = c("1 000","25 000","50 000"),
           pch = 21, pt.cex = c(sqrt(1000 /sizeMax) * 7,
                                sqrt(25000 /sizeMax) * 7 ,
                                sqrt(50000 /sizeMax) * 7),
           title = "Delta Gesamtvolumen in m³",
           x.intersp = 2, y.intersp = 2)

    legend("topleft",
           legend = c("Verringerung", "Erhöhung"),
           pch = 21, pt.cex = 2, title = "Veränderung durch Variante 2",
           pt.bg = c("forestgreen", "orange"))

    dev.off()


    ################################# Intensität #################################
    # "vor Sanierung" minus "nach Sanierung"
    sizeValue <- df_compare$maxIntens_m3_s_1 - df_compare$maxIntens_m3_s_2
    df_out$DeltaInt_m3_s <- sizeValue
    sizeMax <- 20
    # Falls Differenz positiv, dann war das Volumen vorher größer
    # (--> Hat durch Sanierung abgenommen, wie erwartbar)
    colVector <- ifelse(test = sizeValue > 0, "forestgreen", "orange")
    sizeValue <- abs(sizeValue)

    png(filename = paste0(saving_path, "/E", event, "_flow.png"),
        height = 7, width = 11, units = "in", res = 300)
    plot(x = df1$y.coordinate,
         y = df1$x.coordinate,
         pch = 21,
         cex = sqrt(sizeValue /sizeMax)  * 7,
         xaxt = "n", yaxt = "n",
         bg = colVector,
         xlab = "", ylab = "",main = paste0("MWÜ Intensität - E", event))

    legend("bottomleft",
           legend = c("1","5","10"),
           pch = 21, pt.cex = c(sqrt(1 /sizeMax) * 7,
                                sqrt(5 /sizeMax) * 7 ,
                                sqrt(10 /sizeMax) * 7),
           title = "Delta Maximaler Durchfluss in m³/s",
           x.intersp = 2, y.intersp = 2)

    legend("topleft",
           legend = c("Verringerung", "Erhöhung"),
           pch = 21, pt.cex = 2,  title = "Veränderung durch Variante 2",
           pt.bg = c("forestgreen", "orange"))

    dev.off()

    # write table
    write.table(
      x = df_out,
      file = paste0(saving_path, "/E", event, "_DeltaTable.csv"),
      row.names = F, dec = ".", sep = ";")
  }
}
df_all$vol_meanDelta <- round(df_all$vol_sumDelta / df_all$nEvents, 0)

write.table(
  x = df_all,
  file = paste0(saving_path, "/_DeltaVol.csv"),
  row.names = F, dec = ".", sep = ";")

