scenario <- "S7"

# either NULL --> no highlighting, or character vector of catchment names
highlight_catchments <-
  c('Bln I', 'Bln II', 'Bln III', 'Bln IIIa', 'Bln IV', 'Bln IX', 'Bln V',
    'Bln VII', 'Bln VIII', 'Bln X', 'Bln XI', 'Bln XII', 'Chb I', 'Chb Ia',
    'Chb III', 'Nkn I', 'Nkn II', 'Ruh', 'Spa I', 'Wil')
# c(
# "Bln VII", "Bln I", "Bln II", "Bln IIIa", "Bln IV", "Bln V", "Bln VIII",
# "Bln IX", "Nkn I", "Nkn II")



highlight_style <- "shaded"
  # "lightblue"

# This is the correct
scenario_path <- file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages/AP3_Szenarienrechnung/berechnungen",
  scenario
)

#Path of CSO stats from interface
statPath <- file.path(scenario_path,  "2_interface_output")

# MiSa assessment after Qsim simulations
load(file.path(scenario_path, paste0("5_assessment_output/misa_tool_", scenario, ".RData")))

# Path for saving
saving_path <- # file.path(scenario_path, "5_assessment_output")
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Communication/Pictures"
for(i in 1:20){
  kwb.misa::mapPlot_EventTime(
    BerlinRivers = kwb.misa::load_berlin_rivers(),
    statFilesPath = statPath,
    dl_misa = dl_misa,
    savingPath = saving_path,
    below = 1.5,
    decoupling = "",
    event = i,
    scenarioName = scenario,
    dec = ",",
    highlight_catchments = highlight_catchments,
    highlight_style = highlight_style
  )
}

kwb.misa::mapPlot_EventsNumber(
  BerlinRivers = kwb.misa::load_berlin_rivers(),
  df_aggr = df_aggr,
  scenarioName = scenario,
  decoupling = "",
  savingPath = saving_path,
  highlight_catchments = highlight_catchments,
  highlight_style = highlight_style
)

kwb.misa::mapPlot_EventsTime(
  BerlinRivers = kwb.misa::load_berlin_rivers(),
  df_aggr = df_aggr,
  scenarioName = scenario,
  decoupling = "",
  savingPath = saving_path,
  highlight_catchments = highlight_catchments,
  highlight_style = highlight_style,
  english = TRUE,
  file_type = "wmf"
)

