# Path of CSO stats from interface
statPath <- paste0(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/",
  "AP1_Vorbereitung-Strategiebewertung/Schnittstelle/output/vor_sanierung")

# MiSa assessment after Qsim simulations
load("Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/output/vor_sanierung.RData")

# Path for saving
saving_path <- paste0(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages/",
  "AP1_Vorbereitung-Strategiebewertung/Misa_auswertung/output/vor_sanierung")

for(i in 1:20){
  kwb.misa::mapPlot_EventTime(
    BerlinRivers = kwb.misa::load_berlin_rivers(),
    statFilesPath = statPath,
    dl_misa = dl_misa,
    savingPath = saving_path,
    event = i
  )
}

kwb.misa::mapPlot_EventsNumber(
  BerlinRivers = kwb.misa::load_berlin_rivers(),
  df_aggr = df_aggr,
  savingPath = saving_path
)
kwb.misa::mapPlot_EventsTime(
  BerlinRivers = kwb.misa::load_berlin_rivers(),
  df_aggr = df_aggr,
  savingPath = saving_path
)
