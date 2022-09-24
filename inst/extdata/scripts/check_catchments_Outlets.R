xlim <- c(13.18, 13.472)
ylim <- c(52.46, 52.57)
plotDim <- kwb.misa::getDimensions(xlim = xlim, ylim = ylim, width = 10)
width_factor <- plotDim[1]/plotDim[2]

# plot
dev.new(noRStudioGD = TRUE, height = 6, width = 6 * width_factor)
xpdDim <- 4
par(mar = c(0.2,  xpdDim * width_factor / 2,
            xpdDim - 0.2, xpdDim * width_factor / 2))
options(OutDec = dec)


library(readxl)
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

add_catchments()
points(x = x[s], y = y[s], pch = 20, col = "red")

###########
sc <- "bsk"
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

add_catchments()
points(x = x, y = y, pch = 20, cex = 2)
points(x = x[s], y = y[s], pch = 20, col = "green")
