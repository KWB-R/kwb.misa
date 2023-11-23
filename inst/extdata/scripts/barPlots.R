# for this script all misa assessment output files (.Rdata) to be compared must
# be at the same path

misa4_path <- "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages/AP3_Szenarienrechnung/berechnungen"

# NULL for all events
events <- NULL
# c("E2", "E3", "E6", "E9", "E10")

# load scenario table in the right scenario order
ms <- select_scenarios(
  scenario_IDs = c(
    "basis", "str_bln7_10", "lieg_bln7_10","strlieg_bln7_10", "str_ges_30",
    "lieg_ges_30", "strlieg_ges_30","strlieg_ges_xx"))



if(FALSE){
  # focus sites
  fs <- kwb.misa::loadMisa_focus_sites(used_sites_only = FALSE)
  df_plot <- read_and_prepare_data(
    misa4_scarnario_table = ms,
    misa4_path = misa4_path,
    qsim_focus_sites = fs$QSim_name,
    events = events)

  df_plot <- merge(x = df_plot, y = fs, by.x = "qsim_site", by.y = "QSim_name", all = TRUE)

  # Barplots: One site ---------------------------------------------------------
  for(siteName in unique(df_plot$ID)){
    png(filename = paste0(misa4_path, "/szenarienvergleich/barplot_", siteName, ".png"),
        width = 6, height = 10, units = "in", res = 300)
    {
      layout(mat = c(1,2,3,4))

      kwb.misa::barPlot_site(
        df_plot = df_plot,
        siteName = siteName,
        barType = "crit_ox"
      )
      kwb.misa::barPlot_site(
        df_plot = df_plot,
        siteName = siteName,
        barType = "comf_ox"
      )
      kwb.misa::barPlot_site(
        df_plot = df_plot,
        siteName = siteName,
        barType = "crit_events"
      )
      kwb.misa::barPlot_site(
        df_plot = df_plot,
        siteName = siteName,
        barType = "neg_dev"
      )
    }
    dev.off()
  }

  # All focus sites aggregated -------------------------------------------------
  # witout NSK
  fs <- fs[fs$ID != "NSK",]

  df_plot <- read_and_prepare_data(
    misa4_scarnario_table = ms,
    misa4_path = misa4_path,
    qsim_focus_sites = fs$QSim_name,
    events = events)

  df_plot <- merge(x = df_plot, y = fs, by.x = "qsim_site", by.y = "QSim_name", all = TRUE)
  v_col <- c("black", "cadetblue3", "chocolate1", "blueviolet", "brown2",
             "forestgreen", "dodgerblue", "goldenrod2", "hotpink", "olivedrab", "navy", "red4")

  # critical events
  s_list <- split(x = df_plot, f = df_plot$scenario)

  df_events <- data.frame(
    "mean_events" = sapply(s_list, function(x){mean(x$events)}),
    "max_events" = sapply(s_list, function(x){max(x$events)}),
    "effort" = ms$surface_decoupling_percent)
  df_events$reduction_of_mean <- round(
    (1 - df_events$mean_events /
       df_events$mean_events[rownames(df_events) == "Basis"]) * 100, 0)
  df_events$reduction_of_max <- round(
    (1 - df_events$max_events /
       df_events$max_events[rownames(df_events) == "Basis"]) * 100, 0)
  n_scenarios <- nrow(df_events)
  write.table(
    x = df_events,
    file = file.path(misa4_path, "szenarienvergleich", "reduction_events.csv"),
    dec = ".", sep = ";", row.names = TRUE)

  {
    png(filename = paste0(misa4_path, "/szenarienvergleich/revenue_events_max.png"),
        width = 6, height = 6, units = "in", res = 300)
    par(mar = c(4.1, 4.1, 2.1, 2.1))
    plot(x = 0, y = 0, type = "n", xlab = "Aufwand: Abgekoppelte Fläche in %",
         ylab = "Ertrag: Verringerung kritischer Events in %",
         xlim = c(0,50), ylim = c(0,100), xaxs = "i", yaxs = "i", bty = "n")

    mtext(text = "Minimale Verbesserung unter den Gewässerschwerpunkten",
          side = 3, line = 1)

    points(x = df_events$effort,
           y = df_events$reduction_of_max,
           pch = 1:n_scenarios,
           cex = 2,
           xpd = TRUE,
           col = v_col[1:n_scenarios], lwd = 2)

    legend("bottomright", legend = rownames(df_events),
           pch = 1:n_scenarios, col =  v_col[1:n_scenarios])

    dev.off()
  }

  {
    png(filename = paste0(misa4_path, "/szenarienvergleich/revenue_events_mean.png"),
        width = 6, height = 6, units = "in", res = 300)
    par(mar = c(4.1, 4.1, 2.1, 2.1))
    plot(x = 0, y = 0, type = "n", xlab = "Aufwand: Abgekoppelte Fläche in %",
         ylab = "Ertrag: Verringerung kritischer Events in %",
         xlim = c(0,50), ylim = c(0,100), xaxs = "i", yaxs = "i", bty = "n")

    mtext(text = "Mittlere Verbesserung unter den Gewässerschwerpunkten",
          side = 3, line = 1)

    points(x = df_events$effort,
           y = df_events$reduction_of_mean,
           pch = 1:n_scenarios,
           cex = 2,
           xpd = TRUE,
           col = v_col[1:n_scenarios], lwd = 2)

    legend("bottomright", legend = rownames(df_events),
           pch = 1:n_scenarios, col =  v_col[1:n_scenarios])

    dev.off()
  }

  df_events <- data.frame(
    "mean_events" = sapply(s_list, function(x){mean(x$hours.below_1.5)}),
    "max_events" = sapply(s_list, function(x){max(x$hours.below_1.5)}),
    "effort" = ms$surface_decoupling_percent)
  df_events$reduction_of_mean <- round(
    (1 - df_events$mean_events /
       df_events$mean_events[rownames(df_events) == "Basis"]) * 100, 0)
  df_events$reduction_of_max <- round(
    (1 - df_events$max_events /
       df_events$max_events[rownames(df_events) == "Basis"]) * 100, 0)
  write.table(
    x = df_events,
    file = file.path(misa4_path, "szenarienvergleich", "reduction_deficieny_time.csv"),
    dec = ".", sep = ";", row.names = TRUE)

  n_scenarios <- nrow(df_events)

  {
    png(filename = paste0(misa4_path, "/szenarienvergleich/revenue_deficieny_max.png"),
        width = 6, height = 6, units = "in", res = 300)
    par(mar = c(4.1, 4.1, 2.1, 2.1))
    plot(x = 0, y = 0, type = "n", xlab = "Aufwand: Abgekoppelte Fläche in %",
         ylab = "Ertrag: Verringerung der Unterschreitungsdauer in %",
         xlim = c(0,50), ylim = c(0,100), xaxs = "i", yaxs = "i", bty = "n")

    mtext(text = "Minimale Verbesserung unter den Gewässerschwerpunkten",
          side = 3, line = 1)

    points(x = df_events$effort,
           y = df_events$reduction_of_max,
           pch = 1:n_scenarios,
           cex = 2,
           xpd = TRUE,
           col = v_col[1:n_scenarios], lwd = 2)

    legend("bottomright", legend = rownames(df_events),
           pch = 1:n_scenarios, col =  v_col[1:n_scenarios])

    dev.off()
  }
  {
    png(filename = paste0(misa4_path, "/szenarienvergleich/revenue_deficieny_mean.png"),
        width = 6, height = 6, units = "in", res = 300)
    par(mar = c(4.1, 4.1, 2.1, 2.1))
    plot(x = 0, y = 0, type = "n", xlab = "Aufwand: Abgekoppelte Fläche in %",
         ylab = "Ertrag: Verringerung der Unterschreitungsdauer in %",
         xlim = c(0,50), ylim = c(0,100), xaxs = "i", yaxs = "i", bty = "n")

    mtext(text = "Mittlere Verbesserung unter den Gewässerschwerpunkten",
          side = 3, line = 1)

    points(x = df_events$effort,
           y = df_events$reduction_of_mean,
           pch = 1:n_scenarios,
           cex = 2,
           xpd = TRUE,
           col = v_col[1:n_scenarios], lwd = 2)

    legend("bottomright", legend = rownames(df_events),
           pch = 1:n_scenarios, col =  v_col[1:n_scenarios])

    dev.off()
  }

  df_events <- data.frame(
    "mean_events" = sapply(s_list, function(x){mean(x$neg_dev)}),
    "max_events" = sapply(s_list, function(x){max(x$neg_dev)}),
    "effort" = ms$surface_decoupling_percent)
  df_events$reduction_of_mean <- round(
    (1 - df_events$mean_events /
       df_events$mean_events[rownames(df_events) == "Basis"]) * 100, 0)
  df_events$reduction_of_max <- round(
    (1 - df_events$max_events /
       df_events$max_events[rownames(df_events) == "Basis"]) * 100, 0)
  write.table(
    x = df_events,
    file = file.path(misa4_path, "szenarienvergleich", "reduction_negative_dev.csv"),
    dec = ".", sep = ";", row.names = TRUE)

  n_scenarios <- nrow(df_events)

  {
    png(filename = paste0(misa4_path, "/szenarienvergleich/revenue_neg-dev_max.png"),
        width = 6, height = 6, units = "in", res = 300)
    par(mar = c(4.1, 4.1, 2.1, 2.1))
    plot(x = 0, y = 0, type = "n", xlab = "Aufwand: Abgekoppelte Fläche in %",
         ylab = "Ertrag: Verringerung der negativen Abweichung in %",
         xlim = c(0,50), ylim = c(0,50), xaxs = "i", yaxs = "i", bty = "n")

    mtext(text = "Minimale Verbesserung unter den Gewässerschwerpunkten",
          side = 3, line = 1)

    points(x = df_events$effort,
           y = df_events$reduction_of_max,
           pch = 1:n_scenarios,
           cex = 2,
           xpd = TRUE,
           col = v_col[1:n_scenarios], lwd = 2)

    legend("bottomright", legend = rownames(df_events),
           pch = 1:n_scenarios, col =  v_col[1:n_scenarios])

    dev.off()
  }

  {
    png(filename = paste0(misa4_path, "/szenarienvergleich/revenue_neg-dev_mean.png"),
        width = 6, height = 6, units = "in", res = 300)
    par(mar = c(4.1, 4.1, 2.1, 2.1))
    plot(x = 0, y = 0, type = "n", xlab = "Aufwand: Abgekoppelte Fläche in %",
         ylab = "Ertrag: Verringerung der negativen Abweichung in %",
         xlim = c(0,50), ylim = c(0,50), xaxs = "i", yaxs = "i", bty = "n")

    mtext(text = "Mittlere Verbesserung unter den Gewässerschwerpunkten",
          side = 3, line = 1)

    points(x = df_events$effort,
           y = df_events$reduction_of_mean,
           pch = 1:n_scenarios,
           cex = 2,
           xpd = TRUE,
           col = v_col[1:n_scenarios], lwd = 2)

    legend("bottomright", legend = rownames(df_events),
           pch = 1:n_scenarios, col =  v_col[1:n_scenarios])

    dev.off()
  }
}


# select scenarios in the right order by scenario ID
select_scenarios <- function(scenario_IDs){
  # misa scenarios
  ms <- read.csv(
    file = system.file(
      package = "kwb.misa", "extdata/misa_data/misa4_scenarios.csv"),
    header = TRUE, sep = ";")
  s_order <- sapply(paste0("^", scenario_IDs, "$"), grep, x = ms$ID)
  ms[s_order,]
}

read_and_prepare_data <- function(
    misa4_scarnario_table,
    misa4_path, qsim_focus_sites,
    events
){
  x <- misa4_scarnario_table
  file_list <- lapply(x$misa4_sccenario_no, function(sn){
    file.path(
      misa4_path,
      paste0("S", sn),
      "5_assessment_output",
      paste0("misa_tool_S", sn, ".RData"))
  })

  kwb.misa::prepareBarplot(
    rdata_files = file_list,
    scenario_names = x$plot_name,
    qsim_focus_sites = qsim_focus_sites,
    events = events)
}
# read scenario data
df_plot <-







  # Alle Schwerpunkte ------------------------------------------------------------
df_plot$name

df_agg <- aggregate(x = df_plot[,2:8], list(df_plot$scenario), sum)
df_agg$ID <- "Gesamt"
df_agg$Ausgeschrieben <- "Alle Schwerpunkte"
df_agg$scenario <- df_agg$Group.1

siteName <- "Gesamt"
dev.new(noRStudioGD = TRUE, height = 10, widths = 5)
layout(mat = c(1,2,3,4))

kwb.misa::barPlot_site(
  df_plot = df_agg,
  siteName = siteName,
  barType = "crit_ox"
)
kwb.misa::barPlot_site(
  df_plot = df_agg,
  siteName = siteName,
  barType = "comf_ox"
)
kwb.misa::barPlot_site(
  df_plot = df_agg,
  siteName = siteName,
  barType = "crit_events"
)
kwb.misa::barPlot_site(
  df_plot = df_agg,
  siteName = siteName,
  barType = "neg_dev"
)

