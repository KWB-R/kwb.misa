library(kwb.misa)


# compares the infoworks output with the interface output
path <- "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/AP1_Vorbereitung-Strategiebewertung/Schnittstelle"
scenarioName <- "nach_Sanierung"


parameter_conversion <-
  as.data.frame(
    readxl::read_excel(
      path = system.file("extdata/misa_data/parameter_conversion.xlsx",
                         package = "kwb.misa")
    )
  )
outlet_conversion <-
  as.data.frame(
    readxl::read_excel(
      path = system.file("extdata/misa_data/outlet_conversion.xlsx",
                         package = "kwb.misa")
    )
  )

# possible after running "misa4_interface"
eventNo <- 2

iw_out <- kwb.misa:::load_infoworks_output(
  parameter_conversion = parameter_conversion,
  iw_output_folder = interface_input_folder,
  simulation_name = paste0("E", eventNo, "_"),
  outlet_conversion = outlet_conversion,
  flow_only = TRUE)
t_iw_out <- as.POSIXct(iw_out$flow$Time, format = "%d/%m/%Y %H:%M")

flow_only <- iw_out$flow[,-c(1,2)]

colnames(flow_only) <- outlet_conversion$gerris_id[sapply(
  kwb.misa:::ids_from_colnames(COLnames = colnames(flow_only)),
  grep,
  x = outlet_conversion$upstream_link_id)]
asd <- data.frame("t" = t_iw_out, "q" = flow_only[["R1363"]])
skip <- 0.01

plot(x = asd$t[-c(1:floor(nrow(asd) * skip))],
     y = asd$q[-c(1:floor(nrow(asd) * skip))])

totalVolume <- colSums(x = flow_only) * 60 * 5


allFiles <- dir(interface_output_folder)
eventFile <- allFiles[grepl(pattern = paste0("E", eventNo, "_"), allFiles) &
                        grepl(pattern = ".csv", allFiles) & grepl(pattern = "stats", allFiles)
]
inter_out <- read.table(
  file = file.path(interface_output_folder, eventFile),
  sep = ";", header = TRUE)

totalVolume["R1363"]
inter_out$tVol_m3[inter_out$RbId == "R1363"]

dev.new()
plot(x = 0, y = 0, xlim = range(totalVolume), ylim = range(inter_out$tVol_m3),
     xlab = "infoworks Volume", "ylab" = "interface Volume")

for(outlet in names(totalVolume)){
  if(!(is.na(outlet))){
    print(outlet)
    points(x = totalVolume[outlet],
           y = inter_out$tVol_m3[inter_out$RbId == outlet],
           pch = 20)
  }
}

plot(x = 0, y = 0, xlim = range(totalVolume), ylim = range(inter_out$tVol_m3),
     xlab = "infoworks Volume", "ylab" = "interface Volume")
for(outlet in names(totalVolume)){
  if(!(is.na(outlet))){
    text(x = totalVolume[outlet],
         y = inter_out$tVol_m3[inter_out$RbId == outlet],
         labels = outlet, cex = 0.6)
  }
}

