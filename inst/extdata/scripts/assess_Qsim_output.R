# Prepare Qsim -----------------------------------------------------------------
# Enter manually (for MiSa 4)
scenario <- "S3"

# Prepare Qsim output and Save prepared output ---------------------------------
scenario_path <- file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages/AP3_Szenarienrechnung/berechnungen",
  scenario
)
Qsim_output_path <- file.path(scenario_path, "3_qsim_output")

files <- dir(path = Qsim_output_path)
files <- grep(pattern = "^20", files, value = T) # Qsim output files start with a year
for(file in files){
  print(paste(file, "in progress"))
  kwb.misa::QSIM_prepare_for_tool(
    qsim_output_path = Qsim_output_path,
    qsim_fileName = file,
    misa_tool_input_path = file.path(scenario_path, "4_assessment_input"),
    output_fileName = paste0("misa_", scenario, "_", file)
  )
  print("done")
}

# Do MiSa Assessment -----------------------------------------------------------
# List of Events
e_data <- kwb.misa::loadMisa_events()
# increase time frame since CHA is not the last site of the river stretch
e_data$tEnd <- e_data$tEnd + 5 * 24 * 60 * 60
e_data <- e_data[e_data$use == 1,] # filter for  only selected Events
es <- lapply(1:nrow(e_data), function(i){ # turn into list of events
  c("tBeg" = e_data$tBeg[i], "tEnd" = e_data$tEnd[i])
})

# 1. Read Oxygen Data
data_comp <- kwb.misa::read_misa_files(
  input_path = file.path(scenario_path,"4_assessment_input")
)

# 2. Filter Data per event
data_comp_per_event <- lapply(es, function(event){
  data_comp_f <- kwb.misa::misa_filter_data(
    dataFrame = data_comp,
    tBeg = event["tBeg"],
    tEnd = event["tEnd"],
    sites = "") # all sites are included
})

names(data_comp_per_event) <- e_data$X

# test for ovelapping timestamps of the events
# there is an overlap of event 2 and 3 --> Check if there is a critical situation
# somewhere between 2011-06-14 00:00 and 2011-06-14 08:00

# sapply(seq_along(data_comp_per_event)[-1], function(i){
#   data_comp_per_event[[i]]$posixDateTime[1] < rev(data_comp_per_event[[i-1]]$posixDateTime)[1]
#   })

# Reference for neg_dev is "Oberhalb Abzweig LWK"
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
dl_misa <- lapply(dl_misa, kwb.misa:::siteInfo_from_QsimName)

# 6. Aggregate events
df_aggr <- kwb.misa:::aggregate_eventSeries(dl_misa = dl_misa)

rm(list = setdiff(x = ls(), list("df_aggr", "dl_misa", "scenario", "scenario_path")))

save.image(file.path(
  scenario_path,
  "5_assessment_output",
  paste0("misa_tool_", scenario, ".RData"))
)

