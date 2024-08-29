fs <- kwb.misa::loadMisa_focus_sites()
ri <- kwb.misa::load_berlin_rivers()

xlim <- c(13.18, 13.472)
ylim <- c(52.46, 52.57)
plotDim <- kwb.misa::getDimensions(
  xlim = xlim,
  ylim = ylim,
  width = 10
)
width_factor <- plotDim[1]/plotDim[2]

# plot
dev.new(
  noRStudioGD = TRUE,
  height = 6,
  width = 6 * width_factor
)
xpdDim <- 1
par(
  mar = c(0.2,
          xpdDim * width_factor / 2,
          xpdDim - 0.2,
          xpdDim * width_factor / 2)
)

plot(
  x = 0, y = 0,
  xaxt = "n", yaxt = "n", type = "n",
  xaxs = "i", yaxs = "i",
  xlab = "", ylab = "",
  xlim = xlim, ylim = ylim
)

kwb.misa::add_catchments()

for(i in seq_along(ri)){
  lines(
    x = ri[[i]]$x,
    y = ri[[i]]$y,
    col = "steelblue",
    lwd = 5
  )
}
rect(
  xleft = par("usr")[1],
  xright = par("usr")[2],
  ybottom = par("usr")[3],
  ytop = par("usr")[4],
  col = rgb(1, 1, 1, alpha = 0.5)
)
points(
  x = fs$lat,
  y = fs$lon,
  pch = 20,
  cex = 2,
  col = kwb.misa::MisaColor[6]
)
text(
  x = fs$lat,
  y = fs$lon,
  labels = fs$ID,
  col = kwb.misa::MisaColor[6],
  pos = 4
)


