df_plot <- kwb.misa::prepareBarplot(
  scenarioNames = c("s0", "lwk100", "spr100", "bsk100"),
  scenarioPath =
    "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Exchange/Misa_auswertung/output"
)

# Eine Messstelle --------------------------------------------------------------
unique(df_plot$ID)

siteName <- "BEL"
{
  #png(filename = )
  dev.new(noRStudioGD = TRUE, height = 10, width = 4)
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
df_agg$Name <- "Gesamt"
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

