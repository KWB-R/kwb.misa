

szenario_id <- "S9"
outlets_to_suppress <- NULL
  # "16234008"

if(FALSE){
  # Wo liegen die Dateien von Infoworks?
  interface_input_folder <- file.path(
    "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages/AP3_Szenarienrechnung/berechnungen",
    szenario_id, "1_iw_output")

  # Wo sollen die für ormatierten Dateien abgespeichert werden?
  interface_output_folder <- file.path(
    "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages/AP3_Szenarienrechnung/berechnungen",
    szenario_id, "2_interface_output")

  # Simulationsname (hier reicht ein eindeutiger Teil des Namens) und
  # Vorlauftrockenzeit
  simulations <- c(
    "E2_2011" = 5,"E3_2011" = 5, "E5_2012" = 5, "E6_2012" = 5, "E7_2013" = 22,
    "E8_2013" = 5, "E9_2014" = 5, "E10_2014" = 6, "E12_2015" = 5,"E13_2016" = 5,
    "E16_2017" = 6, "E17_2017" = 5,
    "E19_2018" = 14, "E20_2019" = 5)

  # Falls TRUE, werden Wasserqualitätsparameter nicht mit simuliert
  flow_only <- FALSE


  all_results <- list()
  for(i in seq_along(simulations)){
    simulation_name <- names(simulations)[i]
    print(simulation_name)
    trocken <- simulations[i]

    all_results[[i]] <- kwb.misa::iw_gerris_interface(
      interface_input_folder = interface_input_folder,
      interface_output_folder = interface_output_folder,
      simulation_name = simulation_name,
      return_after = "",
      return_output_table = TRUE,
      infoworks_time_format = "%d/%m/%Y %H:%M:%S",
      gerris_time_format = "%d.%m.%Y %H:%M",
      timestep_out = 15,
      flow_only = flow_only,
      skip_hours = trocken * 24,
      flow_threshold = 0.003,
      outlets_to_suppress = outlets_to_suppress
    )
    sink()
  }


  # Additional checks  #########################################################

  # Checken, warum einige Werte zu hoch sind
  simulation_name <- names(simulations)[10]
  print(simulation_name)
  trocken <- simulations[i]

  result <- kwb.misa::iw_gerris_interface(
    interface_input_folder = interface_input_folder,
    interface_output_folder = interface_output_folder,
    simulation_name = simulation_name,
    return_after = 5,
    return_output_table = TRUE,
    infoworks_time_format = "%d/%m/%Y %H:%M:%S",
    gerris_time_format = "%d.%m.%Y %H:%M",
    timestep_out = 15,
    flow_only = flow_only,
    skip_hours = trocken * 24,
    flow_threshold = 0.003)

  asd <- kwb.misa::check_high_concentration(
    result = result,
    parameterID_infoworks = "mfcodtot"
  )

  asd$load <- asd$Parameter * asd$flow
  unique(asd$outlet[asd$load > 1000])
  View(asd)



  # Checken, ob es irgendwo verdächtig langandauernde Überläufe gibt
  library(dplyr)

  susp <- lapply(all_results, function(df){
    overflow_steps <- data.frame(
      df %>% group_by(RbId, ParamId) %>%
        summarize(n = n()) %>%
        filter(ParamId == "Q", n > 2))
    bp <- boxplot(
      overflow_steps$n,
      range = 5,
      plot = FALSE)
    overflow_steps$RbId[overflow_steps$n %in% bp$out]
  })
  # the number of suspicious long overflows per outlet (R1309 is Rochstr.)
  sort(summary(as.factor(unlist(susp))), decreasing = TRUE)

}


