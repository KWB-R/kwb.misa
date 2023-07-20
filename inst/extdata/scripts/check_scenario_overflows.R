library(dplyr)
if(FALSE){
  s_a <- stat_summary(scenario = "S4")
  s_b <- stat_summary(scenario = "S7", include_bsb = TRUE)

  df <- merge(x = s_a, y = s_b, by = "RbId")

  outlet_catch <- kwb.misa:::loadMisa_decouplingInfo()

  df <- merge(x = df, y = outlet_catch, by.x = "RbId", by.y = "gerris_id")

  df2 <- df %>%
    mutate("diff_V" = tVol.x - tVol.y,
           "red_V" = round(diff_V / tVol.x * 100, 0),
           "diff_BSB" = tBSB.x - tBSB.y,
           "red_BSB" = ifelse(
             test = tBSB.x == 0,
             yes = 0, no =  round(diff_BSB / tBSB.x * 100, 0)))

  # Regarding Volume
  df2 %>% group_by(surface_water.x) %>%
    summarize("mean_removal" = mean(red_V),
              "sum_absol" = sum(diff_V),
              "mean_absol" = mean(diff_V))

  View(df2 %>% group_by(catchment_arab) %>%
    summarize("mean_removal" = mean(red_V),
              "sum_removal" = sum(diff_V),
              "n" = n(),
              "mean_absolut" = sum_removal / n))

  # Regarding BOD
  df2 %>% group_by(surface_water.x) %>%
    summarize("mean_removal" = mean(red_BSB),
              "sum_absol" =sum(diff_BSB),
              "mean_absol" = mean(diff_BSB))

  View(df2 %>% group_by(catchment_arab) %>%
         summarize("mean_removal" = mean(red_BSB),
                   "sum_removal" = sum(diff_BSB),
                   "n" = n(),
                   "mean_absolut" = sum_removal / n))

  df2[df2$outlet_id.x == "16234002",]
  hist(df2$removal[df2$removal > 0])
  summary(df2$removal[df2$removal > 0])
  summary(df2$diff[df2$removal > 0])
  summary(df2$diff)
  df3 <-



  df2 %>% filter(removal > 0) %>%
    group_by(surface_water.x) %>%
    summarize("mean_removal" = mean(removal))

  df3 <- df2 %>% filter(removal > 0) %>%
    group_by(catchment_arab) %>%
    summarize("mean_removal" = mean(removal))

  # Regarding BSB
  df2 <- df %>% mutate("diff" = tBSB.x - tBSB.y,
                       "removal" = round(diff / tBSB.x * 100, 0))

  df2 %>% group_by(surface_water.x) %>%
    summarize("mean_removal" = mean(removal, na.rm = TRUE),
              "mean_absolute" = mean(diff, na.rm = TRUE))

  df2 %>% filter(removal > 0) %>%
    group_by(surface_water.x) %>%
    summarize("mean_removal" = mean(removal, na.rm = TRUE),
              "mean_absolute" = mean(diff, na.rm = TRUE) / mean(tBSB.x) * 100)
}

stat_summary <- function(scenario, include_bsb = TRUE){
  scenario_path <- file.path(
    "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages/AP3_Szenarienrechnung/berechnungen",
    scenario
  )

  #Path of CSO stats from interface
  statPath <- file.path(scenario_path,  "2_interface_output")

  all_stats <- grep(pattern = "stats.csv$", x = dir(statPath, full.names = TRUE), value = TRUE)

  stat_list <- list()
  for(i in seq_along(all_stats)){
    stat_list[[i]] <- read.table(
      file = all_stats[i], header = TRUE, sep = ";", dec = "."
    )
    stat_list[[i]]$event_count <- i
  }

  df_stat <- do.call(rbind, stat_list)

  if(include_bsb){
    df_stat %>%
      group_by(RbId, outlet_id, upstream_link_id, surface_water) %>%
      summarize("tVol" = sum(tVol_m3, na.rw = TRUE),
                "tBSB" = sum(tBSB_g, na.rm = TRUE))
  } else {
    df_stat %>%
      group_by(RbId, outlet_id, upstream_link_id, surface_water) %>%
      summarize("tVol" = sum(tVol_m3, na.rw = TRUE))
  }

}




df_stat %>%
  group_by(RbId, outlet_id, upstream_link_id, surface_water) %>%
  summarize("tVol" = sum(tVol_m3, na.rw = TRUE),
            "tBSB" = sum(tBSB_g, na.rm = TRUE))


