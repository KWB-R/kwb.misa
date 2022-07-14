library(kwb.misa)


# compares the infoworks output with the interface output
path <- "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/AP1_Vorbereitung-Strategiebewertung/Schnittstelle"
scenarioName <- "nach_Sanierung"
eventNo <- 19

parameter_conversion <-
  as.data.frame(
    readxl::read_excel(
      path = system.file("extdata/interface/parameter_conversion.xlsx",
                         package = "kwb.misa")
    )
  )



iw_out <- kwb.misa:::load_infoworks_output(
  iw_output_folder = file.path(path, "input", scenarioName),
  simulation_name = paste0("E", eventNo),
  flow_only = TRUE)

grep(pattern = paste0("E", eventNo), x = dir(file.path(path, "input", scenarioName)), value = TRUE)


allFiles <- dir(file.path(path, "output", scenarioName))
eventFile <- allFiles[grepl(pattern = paste0("E", eventNo, "_"), allFiles) &
  grepl(pattern = ".csv", allFiles) & !grepl(pattern = "stats", allFiles)
    ]
inter_out <- read.table(file = file.path(path, "output", scenarioName, eventFile))


