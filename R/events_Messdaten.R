
# Load Data
load("C:/Users/mzamzo/Documents/misa/phase_1/Processing/rdata/misa.RData")

# List of Events ---------------------------------------------------------------
e_data <- loadMisa_events()
# increase time frame since CHA is not the last site of the river stretch
e_data$tEnd <- e_data$tEnd + 5 * 24 * 60 * 60
e_data <- e_data[e_data$use == 1,]
es <- lapply(1:nrow(e_data), function(i){
  c("tBeg" = e_data$tBeg[i], "tEnd" = e_data$tEnd[i])
})

# manual data processing for compatibility reasons
colnames(data_comp) <-
  gsub(pattern = "Sauerstoff", replacement = "oxygen", x = colnames(data_comp))
del <- which(data_comp$site %in% c("BEL", "DOV"))
data_comp <- data_comp[-del,]

# 2. Filter Data per event
data_comp_per_event <- lapply(es, function(event){
  data_comp_f <- kwb.misa::misa_filter_data(
    dataFrame = data_comp,
    tBeg = event["tBeg"],
    tEnd = event["tEnd"],
    sites = "") # all sites are included
})
names(data_comp_per_event) <- e_data$X


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
                                      oxygen_ref = dl[["MUE"]]$d, max_missing = 100)))
})



