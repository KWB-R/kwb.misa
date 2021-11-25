# compare misa time interval functions with Haukes function
df <- kwb.base::hsExampleTSeries(60)

kwb.base::demoGroupByInterval(
  df, step = kwb.datetime::minTimeStep(df[, 1]), to_pdf = TRUE)

install.packages("xts")

# vergleichen der same_inarow und hsEventsOnChange
