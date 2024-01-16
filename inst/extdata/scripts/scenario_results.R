# Dieses Skript vergleicht die Überlaufmenge und die BSB-Fracht aller ereignisse
# in das Gewässersystem für verschiedene Szenarien

statPath <- file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Data-Work packages",
  "AP1_Vorbereitung-Strategiebewertung/Schnittstelle/output/s0")

all_files <- dir(statPath, full.names = TRUE)
stat_files <- grep(pattern = "stats.csv$", all_files, value = TRUE)
e_df <- kwb.misa::loadMisa_events()
e_df <- e_df[as.logical(e_df$use),]

e_stats <- lapply(X = e_df$X, function(p_title){
  statFile <- grep(pattern = paste0(p_title, "_"), x = stat_files, value = TRUE)
  read.csv(file = statFile,header = TRUE, sep = ";", dec = ".")
})

names(e_stats) <- e_df$X
scenario <- "lwk"

decouplingInfo <-
  kwb.misa::decoupledCatchments(decouplingScenario = scenario)

dec_list <- lapply(e_stats, function(x){
  x[x$RbId %in% decouplingInfo$Scenario_outlets,]
})

dec_df <- do.call(rbind, dec_list)
sum(dec_df$tVol_m3)

# Anzahl der Überläufe
length(unique(dec_df$RbId))

# Anzahl der Überläufe mit mindestens einem Überlauf
length(unique(dec_df$RbId[dec_df$tVol_m3 > 0]))

# Gesamtvolumen
sum(dec_df$tVol_m3)

# BSB-Fracht
sum(dec_df$tBSB_g)/ 1000

##################################
# Aggregierte Bewertung
df_plot <- kwb.misa::prepareBarplot(
  scenarioNames = c("s0", "lwk100", "spr100", "bsk100"),
  scenarioPath =
    "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Exchange/Misa_auswertung/output"
)

kwb.misa::barPlot_site(
  df_plot = df_plot,
  siteName = "KIE",
  barType = "crit_ox")

###################

library(dplyr)

df_plot %>%
  group_by(scenario) %>%
  summarise("Events" = max(events))

df_plot %>%
  group_by(scenario) %>%
  summarise("time" = max(hours.below_1.5))

######################
weighted_mean <- function(x){
  x <- x[!is.na(x$value) & !is.na(x$distance_to_neighbour),]
  sum(x$distance_to_neighbour * x$value) / sum(x$distance_to_neighbour)
}


load(file.path(
  "Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Exchange/Misa_auswertung/output",
  "lwk100.Rdata"))

varName <-
   "events"
  # "hours.below_1.5"
  # "neg_dev"

prepared_rivers <- lapply(
  kwb.misa::load_berlin_rivers(), kwb.misa::extend_riverTable,
  qsim_misa_table = df_aggr,
  varName = varName,
  sixBreaks = 1:6
)

################## abgeschnitten ab Schleuse Charlottenburg
prepared_rivers$Spree <- prepared_rivers$Spree[prepared_rivers$Spree$km > 6,]
prepared_rivers <- prepared_rivers[!names(prepared_rivers) %in% "Havel"]

# Pro Gewässer
sapply(prepared_rivers, weighted_mean)

# Gesamt
minimal_data <- lapply(prepared_rivers, function(x){
  data.frame("distance_to_neighbour" = x$distance_to_neighbour,
             "value" = x$value)
})

weighted_mean(x = do.call(rbind, minimal_data))


############################## Aufwand-Etrags Plot

dev.new(noRStudioGD = TRUE, height = 5, width = 5)
par(mar = c(5,5,5,5))

plot(x = 0, y = 0, xlim = c(0,1), ylim = c(0,1),
     type = "n", xlab = "Verringertes des\nÜberlaufvolumen",
     ylab = "Verringerung der Ereignisse", xaxs = "i", yaxs = "i")

lines <- seq(-1, 1, by = 0.01)
col1 <- as.vector(col2rgb(kwb.misa::MisaColor["critical"]) / 255)
col2 <- as.vector(col2rgb(kwb.misa::MisaColor["good"]) / 255)
i <- 1
for(l in lines){
  col <- col1 - (col1 - col2) * i/length(lines)
  col <- rgb(col[1], col[2], col[3])
  abline(a = l, b = 1, lwd = 2, col = col)
  i <- i + 1
}
abline(h = c(0,1))
abline(v = c(0,1))
text(labels = "Geringer Aufwand -\n großer Effekt", x = 0.01, y = 0.99,
     cex = 0.8, adj = c(0,1) )
text(labels = "Hoher Aufwand -\n geringer Effekt", x = 0.98, y = 0.02,
     cex = 0.8, adj = c(1,0) )

# Flachere Geraden
plot(x = 0, y = 0, xlim = c(0,1), ylim = c(0,1),
     type = "n", xlab = "Verringertes des\nÜberlaufvolumen",
     ylab = "Verringerung der Ereignisse", xaxs = "i", yaxs = "i")

lines <- seq(-1, 1, by = 0.001)
col1 <- as.vector(col2rgb(kwb.misa::MisaColor["critical"]) / 255)
col2 <- as.vector(col2rgb(kwb.misa::MisaColor["good"]) / 255)
i <- 1
for(l in lines){
  col <- col1 - (col1 - col2) * i/length(lines)
  col <- rgb(col[1], col[2], col[3])
  abline(a = l, b = 0.5, lwd = 2, col = col)
  i <- i + 1
}
abline(h = c(0,1))
abline(v = c(0,1))
text(labels = "Geringer Aufwand -\n großer Effekt", x = 0.01, y = 0.99,
     cex = 0.8, adj = c(0,1) )
text(labels = "Hoher Aufwand -\n geringer Effekt", x = 0.98, y = 0.02,
     cex = 0.8, adj = c(1,0) )
