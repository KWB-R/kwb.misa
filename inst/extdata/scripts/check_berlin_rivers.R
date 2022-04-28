load_berlin_rivers <- function(){
  river_files <- dir(file.path(system.file(package = "kwb.misa"),
                               "extdata/berlin_rivers"), full.names = T)

  river_names <- dir(file.path(system.file(package = "kwb.misa"),
                               "extdata/berlin_rivers"), full.names = F)
  river_names <- substr( x =  river_names,
    start = 7, stop = nchar(river_names) - 4)



  rivers <- lapply(river_files, function(file){
    river <- read.table(file = file, header = T, sep = ";", dec = ".")
  })

  names(rivers) <- river_names
  rivers
}

rivers <- load_berlin_rivers()
r1 <- rivers$Landwehrkanal
r2 <- rivers$Neukoellner_Schiffahrtskanal

confluence_x <- which(r1$x %in% r2$x)
confluence_y <- which(r1$y %in% r2$y)
if(identical(confluence_x, confluence_y)){
  rivers$Berlin_Spandauer_Schifffahrtskanal[confluence_x,]
}


all_diff <- sapply(X = r1$x, function(x){abs(x - r2$x)})
apply(all_diff, 2, function(x){x == min(all_diff)})
which(all_diff == min(all_diff))
confluence_x <- which(abs(r1$x - r2$x) == min(abs(r1$x - r2$x)))
confluence_y <- which(r1$y %in% r2$y)
if(identical(confluence_x, confluence_y)){
  rivers$Berlin_Spandauer_Schifffahrtskanal[confluence_x,]
}


dev.new()

# single rivers
df_plot <- rivers$Landwehrkanal
xlim <- range(df_plot$x)
ylim <- range(df_plot$y)


plot(x = 0, y = 0, xlim = xlim, ylim = ylim, type = "n")


lines(x = df_plot$x, y = df_plot$y)
text(x = df_plot$x, y = df_plot$y, labels = df_plot$comment, cex = 0.6)
text(x = df_plot$x, y = df_plot$y, labels = df_plot$qsim_id, cex = 0.6)


# all rivers
xlim <- range(unlist(sapply(rivers, function(x){x$x})))
ylim <- range(unlist(sapply(rivers, function(x){x$y})))


dev.new()
plot(x = 0, y = 0, xlim = xlim, ylim = ylim, type = "n")

plot(x = 0, y = 0, xlim =  c(13.28, 13.32), ylim = c(52.5, 52.55), type = "n")
for(i in 1:8){
  lines(x = rivers[[i]]$x, y = rivers[[i]]$y)
  text(x = rivers[[i]]$x, y = rivers[[i]]$y, labels = rivers[[i]]$comment, cex = 0.6)
  #text(x = rivers[[i]]$x, y = rivers[[i]]$y, labels = rivers[[i]]$qsim_id, cex = 0.6)

}
