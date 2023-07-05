#' Loads a set of Misa Assessment output files and writes them into one table
#'
#' Comparison of the aggregated misa files at specified focus river sites
#'
#' @param rdata_files A list of filnames (including filt paths) of the MiSa
#' assessment output files. (.RData-files containing "df_aggr" and "dl_misa")
#' @param scenario_names A character vector of scenario names correspronding to
#' the list of RData files.
#' @param qsim_focus_sites A vector of Qsim IDs of the sites to be compared
#'
#' @return
#' One table wit all Misa output data at the focus sites
#'
#' @export
#'
prepareBarplot <- function(
    rdata_files,
    scenario_names,
    qsim_focus_sites
){

  df_aggr <- NULL # to be loaded and overwritten by the scenario data

  comp_list <- list()
  for(i in seq_along(rdata_files)){
    if(!file.exists(rdata_files[[i]])){
      stop(paste("File:", rdata_files[[i]], "cannot be found."))
    } else {
      load(rdata_files[[i]])
      comp_list[[i]] <- df_aggr[df_aggr$qsim_site %in% qsim_focus_sites,]
      comp_list[[i]][["scenario"]] <- scenario_names[[i]]
    }
  }
  do.call(rbind, comp_list)
}

#' Barplot comparing different Scenarios at one Site
#'
#' @param df_plot Data frame perpared by function [prepareBarplot()]
#' @param siteName One of the site names from the "Name" column of df_plot
#' @param barType One of "crit_ox", "comf_ox", "crit_events" and "neg_dev"
#'
#' @details
#' Bar type "crit_ox" plots bars showing the amount of time the oxygen content
#' is below 0.5, 1, 1.5 and 2  mg/L.
#' Bar type "comf_ox" plots bars showing the amount of time the oxygen content
#' is below 3  mg/L (this was originally 5 mg/L in MiSa 1)
#' Bar type "crit_events" plots bars representing the amount critical oxygen
#' events (O2 < 1.5 mg/L)
#' Bar type "neg_dev" plots bars representing the negative deviation of oxygen
#' concentration compared to a non polluted site (MUE was defined in MiSa 1)
#'
#' @importFrom utils data
#'
#' @export
#'
barPlot_site <- function(
    df_plot, siteName, barType
){
  df_site <- df_plot[df_plot$ID %in% siteName,]
  n <- nrow(df_site)
  title <- unique(df_site$Ausgeschrieben)

  MisaColor <- NULL
  data("MisaColor", envir = environment())


  if(barType == "crit_ox"){
    y_vars <-
      c( "hours.below_2", "hours.below_1.5", "hours.below_1", "hours.below_0.5")
    barCol <- MisaColor[2 + seq_along(y_vars)]
    ymax <- max(df_site["hours.below_2"])
    ylab <- "Unterschreitungsdauer [h]"
  } else if(barType == "comf_ox"){
    y_vars <-  "hours.below_3"
    barCol <- MisaColor[2]
    ymax <- max(df_site["hours.below_3"])
    ylab <- "Unterschreitungsdauer [h]"
  } else if(barType == "crit_events"){
    y_vars <- "events"
    barCol <- MisaColor[3]
    ymax <- max(df_site["events"])
    ylab <- bquote(Anzahl~kritischer~O[2]~-Ereignisse)
  } else if(barType == "neg_dev"){
    y_vars <- "neg_dev"
    barCol <- MisaColor[1]
    ymax <- max(df_site["neg_dev"])
    ylab <- "Negative Abweichung\nvom Referenzzustand [%]"
  }

  par(mar = c(2.1, 5.1, 2.1, 1.1))

  plot(x = 0, y = 0,
       xlim = c(0, n),
       ylim = c(0, ymax),
       type = "n", main = title,
       ylab = ylab, xlab = "", xaxt = "n", xaxs = "i", las = 2)

  axis(side = 1, at = 1:n - 0.5, labels = df_site$scenario, tick = FALSE)

  for(i in seq_along(y_vars)){
    misaBars(
      df_site = df_site,
      y_var = y_vars[i],
      Color = barCol[i]
    )
  }
}

#' Draws all bars within the function [barPlot_site()]
#'
#' @param df_site data frame of all secnarios considered at one side
#' @param y_var Variable column name in df_site that is used for y-values
#' @param Color Color of bars
#'
misaBars <- function(df_site, y_var, Color){
  n <- nrow(df_site)
  rect(xleft = seq(0.1, n - 0.9, 1),
       xright = seq(0.9, n - 0.1, 1),
       ybottom = 0,
       ytop = df_site[,y_var],
       col = Color,
       border = NA)
}


