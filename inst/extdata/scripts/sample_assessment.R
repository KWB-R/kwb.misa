
# for debugging (load everything from package into environment)
# kwb.utils::assignPackageObjects("kwb.misa")



# 1. Read Data
data_comp <- kwb.misa::read_misa_files(
  input_path = "inst/extdata/")

# 2. Filter Data
data_comp_f <- kwb.misa::misa_filter_data(
  dataFrame = data_comp,
  tBeg = "2007-01-01",
  tEnd = "2008-12-31",
  sites = "") # all sites are included

# 3. Manipulated Data
dl <- kwb.misa::misa_prepare_data(
  df_MiSa = data_comp_f,
  res = 15, # temporal resolution in minutes
  max_na_interpolation = 60/15) # 4 missing values a 26 mins  -> one hour max

# 4. Assess Data
do.call(rbind, lapply(X = dl, kwb.misa::yearly_deficiency_time))
do.call(rbind, lapply(X = dl, kwb.misa::yearly_crit_Events))
do.call(rbind, lapply(X = dl, kwb.misa::yearly_negative_deviation,
                      oxygen_ref = dl[["MUE"]]$d))


# 3. get results ---------------------------------------------------------------
thresholds <- c(0.5, 1, 1.5, 2, 5) # in mgg O2/L
event_seperating_hours <- 5 * 24 # in hours
min_deficiency_hours <- 0.25 # in hours
allowed_missing_values <- 25 # in percent (if more values are missing --> no assessment)
reference_site_name <- "MUE" # column name of reference oxygen concentration

################################################################################
source(file = paste0(functionsFile, "the_tool_functions2.R"))
################################################################################

# 1. read Data -----------------------------------------------------------------


# 2. prepare Data --------------------------------------------------------------
if(!("dl" %in% ls())){
  dl <- continuousTimeIntervals(dataFrame = data_comp_f, res = time_resolution)

  dl <- lapply(dl, interpolateParameter,
               max_na_hours = TimeInt_for_interpolation)

  dl <- summer_time(dl = dl)
}

# 3. get results ---------------------------------------------------------------
# Assessment: Deficiency time
deficiencyTime <- do.call(
  rbind,
  lapply(X = dl, FUN = yearly_deficiency_time,
         thresholds = thresholds,
         max_missing = allowed_missing_values))

# Assessment: Number of critical events
critEvents <- do.call(
  rbind,
  lapply(X = dl, FUN = yearly_crit_Events,
         thresholds = c(1.5),
         seperating_hours = event_seperating_hours,
         deficiency_hours = min_deficiency_hours,
         max_missing = allowed_missing_values))

# Assessment: negative deviation from reference (M?hlendammschleuse)
NegDev <- do.call(
  rbind,
  lapply(X = dl, FUN = yearly_negative_deviation,
         oxygen_ref = dl[[reference_site_name]][["oxygen"]],
         max_missing = allowed_missing_values))

# Details about critical events
chosenSite <- "S2_storage_2020"

EventDetails <- list_CritEvents(
  dataFrame = dl[[chosenSite]], oxygen_colname = "oxygen",
  seperating_hours = event_seperating_hours,
  thresholds = c(1.5),
  year = year)

# Example event no 1, treshold 1.5 mg/L
min_time <- min(EventDetails[["th_1.5"]][[1]]$posixDateTime) - 5 * 60 * 60
max_time <- max(EventDetails[["th_1.5"]][[1]]$posixDateTime) + 5 * 60 * 60

plot_rows <-
  which(dl[[chosenSite]]$posixDateTime == min_time):
  which(dl[[chosenSite]]$posixDateTime == max_time)

plot(x = dl[[chosenSite]]$posixDateTime[plot_rows],
     y = dl[[chosenSite]]$oxygen[plot_rows], type = "l", lwd = 2,
     xlab = "Zeit", ylab = "O2-Konzentration [mg/L]", main = chosenSite)
axis(side = 3, at = c(min_time, max_time),
     labels = format(c(min_time, max_time), "%d.%m.%y %H:%M"))
abline(h = 1.5, lty = "dashed")




