#' MRIP data visualization
#'
#' Plots of data over the time span
#' @param combined_catch this is the data frame of all of the catch data within the time period of interest
#' @param effortall this is the data frame of all of the effort data within the time period of interest
#' @param species A vector of the species to include
#' @param waves The MRIP waves to include
#' @param areas Strata in distance from shore
#' @param outdir where your files should go
#' @return Output files to explore the data with the parameters entered
#' @import ggplot2
makeplots <- function(combined_catch, effortall, species, waves, outdir) {
  # Wanted to graph the outputs to see how catch levels & PSEs compare across years by wave, mode, and area
  # Graphed according to the species list, waves, areas, and modes you set in the beginning section of code
  
  # set up total catch plot function
  totcatplot <- function(wavenum, species) {
    df <- combined_catch[combined_catch$COMMON == species & combined_catch$WAVE == wavenum, ]
    p <-
      ggplot(df, aes(x = YEAR, y = TOT_CAT)) +
      geom_point() +
      geom_errorbar(aes(ymin = LOWER_TOT_CAT, ymax = UPPER_TOT_CAT)) +
      labs(title = paste0(species, " WAVE ", wavenum, " TOTAL CATCH"), y = "Total Catch (numbers)") +
      facet_grid(rows = vars(MODE_FX_F), cols = vars(AREA_X_F), scales = "free_y", drop = FALSE) +
      theme_bw() +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    print(p)
  }
  
  # Loops through each species and produces a graph for each wave
  pdf(file.path(outdir, "Total Catch.pdf"))
  for (s in species) {
    for (w in waves) {
      totcatplot(w, s)
    }
  }
  dev.off()
  
  # Graphing of landings
  # set up landings plot function
  landingplot <- function(wavenum, species) {
    df <- combined_catch[combined_catch$COMMON == species & combined_catch$WAVE == wavenum, ]
    p <-
      ggplot(df, aes(x = YEAR, y = LANDING)) +
      geom_point() +
      geom_errorbar(aes(ymin = LOWER_LANDING, ymax = UPPER_LANDING)) +
      labs(title = paste0(species, " WAVE ", wavenum, " LANDINGS (A+B1)"), y = "Landings (numbers)") +
      facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y", drop = FALSE) +
      theme_bw() +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    print(p)
  }
  
  # Loops through each species and produces a graph for each wave
  pdf(file.path(outdir, "Landings.pdf"))
  for (s in species) {
    for (w in waves) {
      landingplot(w, s)
    }
  }
  dev.off() # closes the PDF device
  
  # Graphing of releases
  # set up release plot function
  relplot <- function(wavenum, species) {
    df <- combined_catch[combined_catch$COMMON == species & combined_catch$WAVE == wavenum, ]
    p <-
      ggplot(df, aes(x = YEAR, y = ESTREL)) +
      geom_point() +
      geom_errorbar(aes(ymin = LOWER_ESTREL, ymax = UPPER_ESTREL)) +
      labs(title = paste0(species, " WAVE ", wavenum, " Live Releases (B2)"), y = "Live Releases (numbers)") +
      facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y") +
      theme_bw() +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    print(p)
  }
  
  # Loops through each species and produces a graph for each wave
  pdf(file.path(outdir, "Releases.pdf"))
  for (s in species) {
    for (w in waves) {
      relplot(w, s)
    }
  }
  dev.off() # closes the PDF device
  
  effplot <- function(wavenum) {
    df <- effortall[effortall$WAVE == wavenum, ]
    p <-
      ggplot(df, aes(x = YEAR, y = ESTRIPS)) +
      geom_point() +
      geom_errorbar(aes(ymin = LOWER_ESTRIPS, ymax = UPPER_ESTRIPS)) +
      labs(title = paste0("WAVE ", wavenum, " Estimated Angler Trips"), y = "Est. Angler Trips (numbers)") +
      facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y") +
      theme_bw() +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    print(p)
  }
  
  # Loops through each species and produces a graph for each wave
  pdf(file.path(outdir, "EstTrips.pdf"))
  for (w in waves) {
    effplot(w)
  }
  dev.off() # closes the PDF device
}