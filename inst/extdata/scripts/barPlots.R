# for this script all misa assessment output files (.Rdata) to be compared must
# be at the same path

misa4_path <- "Y:/AUFTRAEGE/_Auftraege_laufend/MISA4/Data-Work packages/AP3_Szenarienrechnung/berechnungen"


# focus sites
fs <- kwb.misa::loadMisa_focus_sites(used_sites_only = FALSE)

df_plot <- kwb.misa::prepareBarplot(
  rdata_files = list(
    file.path(misa4_path, "S0_basis", "5_assessment_output", "misa_tool_basis.RData"),
    file.path(misa4_path, "S3", "5_assessment_output", "misa_tool_S3.RData")),
  scenario_names = c(
    "basis",
    "str10_bln7"),
  qsim_focus_sites = fs$QSim_name)

df_plot <- merge(x = df_plot, y = fs, by.x = "qsim_site", by.y = "QSim_name", all = TRUE)


# Eine Messstelle --------------------------------------------------------------
unique(df_plot$ID)

siteName <- "SOP"
{
  #png(filename = )
  dev.new(noRStudioGD = TRUE, height = 10, widths = 6)
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

