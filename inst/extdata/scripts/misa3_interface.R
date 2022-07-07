library(kwb.misa)

# Wo liegen die Dateien von Infoworks?
interface_input_folder <-
  paste0("Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/",
         "Data-Work packages/AP1_Vorbereitung-Strategiebewertung/",
         "Schnittstelle/input/nach_sanierung")

# Wo sollen die für ormatierten Dateien abgespeichert werden?
interface_output_folder <-
  paste0("Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/",
         "Data-Work packages/AP1_Vorbereitung-Strategiebewertung/",
         "Schnittstelle/output/test_paket")

# Wie heißen die Simulationen (hier reicht ein eindeutiger Teil des Namens)?
# Außerdem muss zusätzlich die in Infoworks mitsimulierte Vorlauftrockenzeit
# in Tagen angegeben werden. Diese Tage werden von der Schnittstelle
# abgeschnitten.
simulations <- c(
  "E2_2011" = 5, "E3_2011" = 5, "E5_2012" = 5, "E6_2012" = 5, "E7_2013" = 22,
  "E8_2013" = 5, "E9_2014" = 5, "E10_2014" = 6, "E12_2015" = 5,
  "E13_2016" = 5, "E16_2017" = 6, "E17_2017" = 5, "E19_2018" = 14,
  "E20_2019" = 5)

# Nur der Durchfluss (TRUE) oder auch die Wasserqualitätsparameter?
flow_only <- FALSE


# Ausführen (pro Simulation)
for(i in seq_along(simulations)){
  simulation_name <- names(simulations)[i]
  print(simulation_name)
  trocken <- simulations[i]

  result <- iw_gerris_interface(
    interface_input_folder = interface_input_folder,
    interface_output_folder = interface_output_folder,
    simulation_name = simulation_name,
    return_after = 16, # hier kann eine Zahl eingegeben werden, an der die Datenumformung stopt (siehe Dokumentation)
    return_output_table = TRUE, # Bei FALSE wird nur gespeichert
    infoworks_time_format = "%d/%m/%Y %H:%M:%S",
    gerris_time_format = "%d.%m.%Y %H:%M",
    timestep_out = 15, # in Minuten
    flow_only = flow_only,
    skip_hours = trocken * 24, # depending on inforworks pre calculations
    flow_threshold = 0.003 # in m3/s
  )
}

# Checken, warum einige Werte zu hoch sind
result <- iw_gerris_interface(
  interface_input_folder = interface_input_folder,
  interface_output_folder = interface_output_folder,
  simulation_name = simulation_name,
  stop_after = 5, # hier kann eine Zahl eingegeben werden, an der die Datenumformung stopt (siehe Dokumentation)
  return_output_table = TRUE, # Bei FALSE wird nur gespeichert
  infoworks_time_format = "%d/%m/%Y %H:%M:%S",
  gerris_time_format = "%d.%m.%Y %H:%M",
  timestep_out = 15, # in Minuten
  flow_only = flow_only,
  skip_hours = trocken * 24, # depending on inforworks pre calculations
  flow_threshold = 0.003) # in m?/s

# Memo: Die warnungen zur Korrktur der Messwerte werden nicht in die Log-Datei
# geschrieben sondern in die Console -> sollte geaendert werden

check_high_concentration(
  result = result,
  parameter_conversion = parameter_conversion,
  parameterID_infoworks = "mfcodtot"
)

