# Path of CSO stats from interface
statPath <- file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages",
  "AP1_Vorbereitung-Strategiebewertung/Schnittstelle/output/vor_sanierung")

# MiSa assessment after Qsim simulations
load(file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Exchange/Misa_auswertung/output",
  "sow100.RData"))

# Path for saving
saving_path <- paste0(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Exchange/Misa_auswertung/output/sow100")

for(i in 1:20){
    kwb.misa::mapPlot_EventTime(
      BerlinRivers = kwb.misa::load_berlin_rivers(),
      statFilesPath = statPath,
      dl_misa = dl_misa,
      savingPath = saving_path,
      varName = "below_1.5",
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
