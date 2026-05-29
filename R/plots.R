#' MRIP data visualization
#'
#' Plots of data over the time span
#' @param catch this is the data frame of all of the catch data within the time period of interest
#' @param effort this is the data frame of all of the effort data within the time period of interest
#' @param species A vector of the species to include
#' @param waves The MRIP waves to include
#' @param vars (optional) The MRIP variables to include
#' @param outdir where your files should go
#' @return Output files to explore the data with the parameters entered
#' @import ggplot2
#' @export
Plot <- function(catch, effort, species, waves,
                      vars = c("ESTREL", "LANDING", "TOT_CAT"), outdir) {
  time_pd <- unique(catch$YEAR)
  modes <- unique(effort$MODE_FX_F)
  areas <- unique(effort$AREA_X_F)
  ##### TOTAL CATCH COMPARISONS######

  # prelim calculations###########
  # totcat_notCommon <- totcat[is.na(totcat$n), ]

  all_combinations <- expand.grid(
    COMMON = species,
    YEAR = time_pd,
    WAVE = waves,
    MODE_FX_F = modes,
    AREA_X_F = areas
  )

  # Merge with the original mrip_data frame
  combined_catch <- merge(all_combinations, catch, all.x = TRUE)
  # Optionally, replace NA values with 0...
  combined_catch <- combined_catch[combined_catch$COMMON %in% species, ]
  combined_catch$YEAR <- as.factor(combined_catch$YEAR)
  combined_catch$WAVE <- as.factor(combined_catch$WAVE)
  combined_catch$MODE_FX_F <- factor(combined_catch$MODE_FX_F, levels=c("SHORE", "PRIVATE/RENTAL BOAT", "PARTY BOAT", "CHARTER BOAT"))
  combined_catch$AREA_X_F <- factor(combined_catch$AREA_X_F,
                                       levels=c("INLAND","OCEAN (<= 3 MI)", "OCEAN (> 3 MI)"))
  combined_catch$COMMON <- as.factor(combined_catch$COMMON)
  # combined_catch <- combined_catch[!is.na(combined_catch$STATUS), ] #does this undo the purpose of doing the grid expand?
  ################## EFFORT
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
    ggsave(file.path(outdir, s, "Total Catch.png"), p)
  }

  if ("TOT_CAT" %in% vars) {
    # Loops through each species and produces a graph for each wave
    for (s in species) {
      dir.create(file.path(outdir, s), showWarnings = F)
      pdf(file.path(outdir, s, "Total Catch.pdf"))
      for (w in waves) {
        totcatplot(w, s)
      }
      dev.off()
    }
  }

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
    ggsave(file.path(outdir, s, "Landings.png"), p)
  }

  if ("LANDING" %in% vars) {
    # Loops through each species and produces a graph for each wave
    for (s in species) {
      dir.create(file.path(outdir, s), showWarnings = F)
      pdf(file.path(outdir, s, "Landings.pdf"))
      for (w in waves) {
        landingplot(w, s)
      }
      dev.off()
    }
  } # closes the PDF device

  # Graphing of releases
  # set up release plot function
  relplot <- function(wavenum, s) {
    df <- combined_catch[combined_catch$COMMON == s & combined_catch$WAVE == wavenum, ]
    # df$YEAR <- as.integer(df$YEAR)
    if (nrow(df) > 0) {
      p <-
        ggplot(df, aes(x = YEAR, y = ESTREL)) +
        geom_point() +
        geom_errorbar(aes(ymin = LOWER_ESTREL, ymax = UPPER_ESTREL)) +
        labs(title = paste0(s, " WAVE ", wavenum, " Live Releases (B2)"), y = "Live Releases (numbers)") +
        facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y") +
        theme_bw() +
        scale_x_discrete(guide = guide_axis(n.dodge = 2))
      print(p)
      ggsave(file.path(outdir, s, "Releases.png"), p)
    }
  }

  if ("ESTREL" %in% vars) {
    # Loops through each species and produces a graph for each wave
    for (s in species) {
      print(s)
      dir.create(file.path(outdir, s), showWarnings = F)
      pdf(file.path(outdir, s, "Releases.pdf"))
      for (w in waves) {
        print(w)
        relplot(w, s)
      }
      dev.off()
    }
  }
  # closes the PDF device
  # effort$YEAR <- as.factor(effort$YEAR)
  effort$MODE_FX_F <- factor(effort$MODE_FX_F, levels=c("SHORE", "PRIVATE/RENTAL BOAT", "PARTY BOAT", "CHARTER BOAT"))
  effplot <- function(wavenum) {
    df <- effort[effort$WAVE == wavenum, ]
    # df$YEAR <- as.integer(df$YEAR)
    p <-
      ggplot(df, aes(x = YEAR, y = ESTRIPS)) +
      geom_point() +
      geom_errorbar(aes(ymin = LOWER_ESTRIPS, ymax = UPPER_ESTRIPS)) +
      labs(title = paste0("WAVE ", wavenum, " Estimated Angler Trips"), y = "Est. Angler Trips (numbers)") +
      facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y") +
      theme_bw() #+
      #scale_x_discrete(guide = guide_axis(n.dodge = 2))
    print(p)
    ggsave(file.path(outdir, s, "EstTrips.png"), p)
  }

  # Loops through each species and produces a graph for each wave
  pdf(file.path(outdir, "EstTrips.pdf"))
  for (w in waves) {
    effplot(w)
  }
  dev.off() # closes the PDF device
}
