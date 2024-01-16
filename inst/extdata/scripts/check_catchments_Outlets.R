xlim <- c(13.18, 13.472)
ylim <- c(52.46, 52.57)
plotDim <- kwb.misa::getDimensions(xlim = xlim, ylim = ylim, width = 10)
width_factor <- plotDim[1]/plotDim[2]

# plot outlets per catchment
dev.new(noRStudioGD = TRUE, height = 6, width = 6 * width_factor)
xpdDim <- 4
par(mar = c(0.2,  xpdDim * width_factor / 2,
            xpdDim - 0.2, xpdDim * width_factor / 2))
options(OutDec = dec)


library(readxl)
kwb.misa::loadMisa_decouplingInfo()
df <- read_excel(path = file.path("Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/",
"Data-Work packages/AP2_Potenzialanalyse", "outlet_conversion_abkopplung.xlsx"), )
unique(df$catchment)

catch <- "Bln 3"
s <- grep(pattern = catch, x = df$catchment)
y <- df$lat_grad + df$lat_min / 60 + df$lat_s / 3600
x <- df$lon_grad + df$lon_min / 60 + df$lon_s / 3600

plot(
  x = 0, y = 0,
  xaxt = "n", yaxt = "n", type = "n",
  xaxs = "i", yaxs = "i",
  xlab = "", ylab = "",
  xlim = xlim, ylim = ylim)

kwb.misa::add_catchments(
  highlight_catchments = "Bln III",
  highlight_style = "indianred1"
  )
points(x = x[s], y = y[s], pch = 20, col = "red")

# plot decoupling scenarios
rivers <- kwb.misa::load_berlin_rivers()
de_catch <- list(
  "bsk" = c("Bln IX", "Bln IV", "Bln X", "Bln VIII"),
  "spr" = c("Bln XI", "Bln XII", "Bln V", "Chb III", "Bln VIII",
            "Bln IIIa", "Bln IV", "Bln X"),
  "lwk" = c("Bln II", "Bln VII", "Nkn I", "Nkn II", "Wil",
            "Bln I", "Bln III", "Chb I"))

sc <- "lwk"
high_catch <- de_catch[[sc]]


s <- which(!(as.logical(df[[sc]])))
y <- df$lat_grad + df$lat_min / 60 + df$lat_s / 3600
x <- df$lon_grad + df$lon_min / 60 + df$lon_s / 3600
unique(unlist(strsplit(unique(df$catchment[s]), split = " \\+ ")))

plot(
  x = 0, y = 0,
  xaxt = "n", yaxt = "n", type = "n",
  xaxs = "i", yaxs = "i",
  xlab = "", ylab = "",
  xlim = xlim, ylim = ylim, main = paste0("Outlets draußen (", sc, ")"))

kwb.misa::add_catchments(
  highlight_catchments = high_catch,
  highlight_style = "indianred1")

for(r in rivers){
  lines(x = r$x, y = r$y, col = "steelblue", lwd = 2)
}

points(x = x, y = y, pch = 20, cex = 2)
points(x = x[s], y = y[s], pch = 20, col = "green")
