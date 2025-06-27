# catch data
# switched to readr::read_csv rather than read.csv due to issues setting catch estimates with commas in them to numeric values
# Info on setting columns to specific data types and options can be found https://stackoverflow.com/questions/31568409/override-column-types-when-importing-data-using-readrread-csv-when-there-are
# subsets to just your state's data #need same number of columns for this to work
# need same number of columns for this to work
readcatch <- function(x, xfile=NULL, state, species, waves) {
  if (length(grep("z", x))) {
    filen <- archive::archive_read(x, file = xfile)
  } else {
    filen <- x
  }
  C.tmp <- readr::read_csv(filen,
    na = "",
    col_types = readr::cols(
      LAND_VAR = readr::col_number(),
      ALT_FLAG = readr::col_integer(),
      YEAR = readr::col_integer(),
      WAVE = readr::col_integer(),
      SUB_REG = readr::col_integer(),
      ST = readr::col_integer(),
      MODE_FX = readr::col_integer(),
      AREA_X = readr::col_integer(),
      #ESTCLAIM = readr::col_integer(),
      #ESTCLVAR = readr::col_integer(),
      #LOWER_ESTCLAIM = readr::col_integer(),
      #UPPER_ESTCLAIM = readr::col_integer(),
      # PC_ESTCLAIM_IMP=readr::col_integer(),
      #ESTHARV = readr::col_integer(),
      #ESTHVAR = readr::col_integer(),
      #LOWER_ESTHARV = readr::col_integer(),
      #UPPER_ESTHARV = readr::col_integer(),
      #LANDING = readr::col_integer(),
      #LAND_VAR = readr::col_integer(),
      LOWER_LANDING = readr::col_integer(), 
      SP_CODE = readr::col_character(), #, #UPPER_LANDING = readr::col_integer(),
      #ESTREL = readr::col_integer(),
      ESTRLVAR = readr::col_integer(), LOWER_ESTREL = readr::col_integer(), UPPER_ESTREL = readr::col_integer()#,
      #TOT_VAR = readr::col_integer(), UPPER_TOT_CAT = readr::col_integer(), LBS_AB1 = readr::col_integer(),
      #VAR_LBS = readr::col_integer(), LOWER_LBS_AB1 = readr::col_integer(), UPPER_LBS_AB1 = readr::col_integer(),
      #WGT_AB1 = readr::col_integer(), VAR_WAB1 = readr::col_integer(), LOWER_WGT_AB1 = readr::col_integer(),
      #UPPER_WGT_AB1 = readr::col_integer(), TOT_LEN = readr::col_integer(),
      # VARTOLEN=readr::col_integer(),
      #LOWER_TOT_LEN = readr::col_integer(), UPPER_TOT_LEN = readr::col_integer(), MISS_FISH = readr::col_integer(), 
    ), lazy=T
  )
  C.tmp <- C.tmp %>% filter(ST == state & COMMON %in% species & WAVE %in% waves) #& AREA_X_F %in% areas & MODE_FX_F %in% modes)
  names(C.tmp) <- toupper(names(C.tmp))
  return(C.tmp)
}

readeffort <- function(x, xfile=NULL, state, waves, areas, modes) {
  if (length(grep("z", x))) {
    filen <- archive::archive_read(x, file = xfile)
  } else {
    filen <- x
  }
  C.tmp <- readr::read_csv(filen, na = "", lazy=T)
  C.tmp <- C.tmp %>% filter(ST == state & WAVE %in% waves & AREA_X_F %in% areas & MODE_FX_F %in% modes)
  names(C.tmp) <- toupper(names(C.tmp))
  return(C.tmp)
}

#' MRIP Outlier
#'
#' Creates output to help you identify outliers
#' @param styr Start year
#' @param endyr End year
#' @param y_prelim The latest year in the data (preliminary)
#' @param species A vector of the species to include
#' @param waves The MRIP waves to include
#' @param areas Strata in distance from shore
#' @param modes Modes of fishing
#' @param state The FIPS code for the state of interest
#' @return Output files to explore the data with the parameters entered
#' @export
#' @import ggplot2
#' @import readr
#' @import dplyr
#' @importFrom tidyr complete
#' @importFrom RCurl getURL
#' @importFrom archive archive_read
#' @examples
#' mrip(2022, 2023, 2024, c("SUMMER FLOUNDER", "TAUTOG"), c(3, 4), c("INLAND", "OCEAN (<= 3 MI)"), c("CHARTER BOAT", "PARTY BOAT"), 24)

mrip <- function(styr, endyr, y_prelim = NA, species, waves, areas, modes, state) {
  url <- "https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads/"
  filenames <- paste(url, strsplit(RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r*\n")[[1]], sep = "")
  #filenames <- stringr::str_extract(filenames[c(grep("zip", filenames), grep(".csv", filenames, fixed = TRUE))], "mr[a-z0-9_]*[.][a-z]*")
  filenames <- gsub('.*(mr[a-z0-9_]*[.][a-z]{3}).*','\\1', filenames[c(grep("zip", filenames), grep(".csv", filenames, fixed = TRUE))])
  #yrs <- stringr::str_extract(filenames, "[0-9]{4}(_[0-9]{4})*")
  yrs <- regmatches(fullnames, regexpr("[0-9]{4}(_[0-9]{4})*", fullnames))
  yrs <- sapply(sapply(yrs, strsplit, split = "_"), as.integer)
  yrs <- c(lapply(yrs[which(sapply(yrs, length) > 1)], function(x) x[1]:x[2]), yrs[which(sapply(yrs, length) < 2)])
  
  # Combine files for catch and effort estimates for the "bywave" files
  # Code modified from a loop written by Katie Drew (ASMFC)
  data <- lapply(unique(c(styr:endyr, y_prelim)), function(y) {
    yr <- which(sapply(yrs, function(x) {
      y %in% x
    }))
    data1 <- lapply(
      filenames[yr],
      function(x) {
        path <- paste0(url, x)
        print(path)
        if (length(grep("z", x)) > 0) {
          if (length(grep("ca", x)) > 0) {
            file <- paste0("mrip_catch_bywave_", y, ".csv")
            data <- readcatch(path, file, state, species, waves)
          } else {
            file <- paste0("mrip_effort_bywave_", y, ".csv")
            data <- readeffort(path, file, state, waves, areas, modes)
        }} else {
          if (length(grep("ca", x)) > 0) {
            data <- readcatch(path, state=state, species=species, waves=waves)
          } else {
            data <- readeffort(path, state=state, waves=waves, areas=areas, modes=modes)
          }
        }
      }
    )
    return(data1)
  })

  # now as it is, data is a nested list...
  # first level: year
  # 2nd level: 1 - catch, 2 - effort

  catchall <- bind_rows(lapply(data, "[[", 1))
  
  catch_prelim <- catchall[catchall$YEAR == y_prelim,]
  catch <- catchall[catchall$YEAR %in% styr:endyr, ]
  # For looking for outliers by species at the wave level, first need to collapse estimates
  # across the modes and areas to get wave level estimates by species for each year
  catch_summed <- catch %>%
    #filter(ST == state) %>%
    group_by(COMMON, YEAR, WAVE) %>%
    summarise(
      sum_totcat = sum(TOT_CAT, na.rm = TRUE),
      sum_land = sum(LANDING, na.rm = TRUE),
      sum_rel = sum(ESTREL, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Group by relevant columns and calculate sample size n, mean, and std dev
  totcat_stats <- catch_summed %>%
    #filter(YEAR >= styr & YEAR <= endyr) %>% #do we need this filter?
    group_by(COMMON, WAVE) %>%
    summarise(
      mean_catch = mean(sum_totcat, na.rm = TRUE),
      sd_catch = sd(sum_totcat, na.rm = TRUE),
      mean_catch_l = mean(sum_land, na.rm = TRUE),
      sd_catch_l = sd(sum_land, na.rm = TRUE),
      mean_catch_r = mean(sum_rel, na.rm = TRUE),
      sd_catch_r = sd(sum_rel, na.rm = TRUE),
      n = n(), # Calculate sample size for each group
      .groups = "drop"
    )
  
    totcat_prelim <- catch_prelim %>%
      #filter(ST==state) %>%
      group_by(COMMON, WAVE) %>%
      summarise(
        sum_catch = sum(TOT_CAT, na.rm = TRUE),
        sum_land = sum(LANDING, na.rm = TRUE),
        sum_rel = sum(ESTREL, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Join 2024 data with calculated harvest_stats (mean, sd, and n) PICK UP HERE  JMGO
    totcat <- totcat_prelim %>%
      left_join(totcat_stats, by = c("COMMON", "WAVE"))
    
    ##### TOTAL CATCH COMPARISONS###### 
    
    #prelim calculations###########
    # need combined file for graphing later in code
    
    # Had an issue in the next section because of some tot_catch estimates in 2024 that aren't in the 2017-2023 summaries
    # Removed these first and put the ones being removed into a unique file so they can still be looked at later
    totcat_notCommon <- totcat[is.na(totcat$n), ]
    totcat <- totcat[!is.na(totcat$n), ]
    
    # Apply the Thompson Tau calculation
    
    tau <- function(n, sum_catch, mean_catch, sd_catch) {
      if (n > 2) {
        t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
        tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
        abs(sum_catch - mean_catch) / sd_catch > tau
      } else {
        FALSE # Not enough data to calculate outliers
      }
    }
    
    totcat <- totcat %>%
      rowwise() %>%
      mutate(
      outlier_CATCH = tau(n, sum_catch, mean_catch, sd_catch),
      outlier_LAND = tau(n, sum_land, mean_catch_l, sd_catch_l),
      outlier = tau(n, sum_rel, mean_catch_r, sd_catch_r))
    
    # Filter the outliers
    totcat_outliers <- totcat %>%
      filter(outlier_CATCH == TRUE)
    write.csv(totcat_outliers, "totcat_outliers.csv")
    
    land_outliers <- totcat %>%
      filter(outlier_LAND == TRUE)
    write.csv(land_outliers, "landings_outliers.csv")
    
    rel_outliers <- totcat %>%
      filter(outlier == TRUE)
    write.csv(rel_outliers, "release_outliers.csv")
    
    combined_catch <- catchall %>%
      tidyr::complete(COMMON = species, YEAR = styr:endyr + 1, WAVE = waves, MODE_FX_F = modes, AREA_X_F = areas)
    
    combined_catch$YEAR <- as.factor(combined_catch$YEAR)
    combined_catch$WAVE <- as.factor(combined_catch$WAVE)
    combined_catch$MODE_FX_F <- as.factor(combined_catch$MODE_FX_F)
    combined_catch$AREA_X_F <- as.factor(combined_catch$AREA_X_F)
    combined_catch$COMMON <- as.factor(combined_catch$COMMON)
    
    # Wanted to graph the outputs to see how catch levels & PSEs compare across years by wave, mode, and area
    # Graphed according to the species list, waves, areas, and modes you set in the beginning section of code
    
    # set up total catch plot function
    totcatplot <- function(wavenum, species) {
      p <- combined_catch %>% filter(COMMON==species) %>% filter(WAVE==wavenum) %>%
        ggplot(aes(x = YEAR, y = TOT_CAT)) +
        geom_point() +
        geom_errorbar(aes(ymin = LOWER_TOT_CAT, ymax = UPPER_TOT_CAT)) +
        labs(title = paste0(species, " WAVE ", wavenum, " TOTAL CATCH"), y = "Total Catch (numbers)") +
        facet_grid(rows = vars(MODE_FX_F), cols = vars(AREA_X_F), scales = "free_y", drop = FALSE) +
        theme_bw() +
        scale_x_discrete(guide = guide_axis(n.dodge = 2))
      print(p)
    }
    
    # Loops through each species and produces a graph for each wave
    pdf("Total Catch.pdf")
    for (s in species) {
      for (w in waves) {
        totcatplot(w, s)
      }
    }
    dev.off()
    
    # Graphing of landings
    # set up landings plot function
    landingplot <- function(wavenum, species) {
      p <- combined_catch %>% filter(COMMON==species) %>% filter(WAVE==wavenum) %>%
        ggplot(aes(x = YEAR, y = LANDING)) +
        geom_point() +
        geom_errorbar(aes(ymin = LOWER_LANDING, ymax = UPPER_LANDING)) +
        labs(title = paste0(species, " WAVE ", wavenum, " LANDINGS (A+B1)"), y = "Landings (numbers)") +
        facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y", drop = FALSE) +
        theme_bw() +
        scale_x_discrete(guide = guide_axis(n.dodge = 2))
      print(p)
    }
    
    # Loops through each species and produces a graph for each wave
    pdf("Landings.pdf")
    for (s in species) {
      for (w in waves) {
        landingplot(w, s)
      }
    }
    dev.off() # closes the PDF device
    
    # Graphing of releases
    # set up release plot function
    relplot <- function(wavenum, species) {
      p <- combined_catch %>% filter(COMMON==species) %>% filter(WAVE==wavenum) %>%
        ggplot(aes(x = YEAR, y = ESTREL)) +
        geom_point() +
        geom_errorbar(aes(ymin = LOWER_ESTREL, ymax = UPPER_ESTREL)) +
        labs(title = paste0(species, " WAVE ", wavenum, " Live Releases (B2)"), y = "Live Releases (numbers)") +
        facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y") +
        theme_bw() +
        scale_x_discrete(guide = guide_axis(n.dodge = 2))
      print(p)
    }
    
    # Loops through each species and produces a graph for each wave
    pdf("Releases.pdf")
    for (s in species) {
      for (w in waves) {
        relplot(w, s)
      }
    }
    dev.off() # closes the PDF device
  
  ##################EFFORT
  effortall <- bind_rows(lapply(data, "[[", 2))
  
  effplot <- function(wavenum) {
    p <- effortall %>% filter(WAVE==wavenum) %>%
      ggplot(aes(x = YEAR, y = ESTRIPS)) +
      geom_point() +
      geom_errorbar(aes(ymin = LOWER_ESTRIPS, ymax = UPPER_ESTRIPS)) +
      labs(title = paste0("WAVE ", wavenum, " Estimated Angler Trips"), y = "Est. Angler Trips (numbers)") +
      facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales = "free_y") +
      theme_bw() +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    print(p)
  }
  
  # Loops through each species and produces a graph for each wave
  pdf("EstTrips.pdf")
  for (w in waves) {
    effplot(w)
  }
  dev.off() # closes the PDF device
  
  effort_prelim <- effortall[effortall$YEAR == y_prelim,]
  effort <- effortall[effortall$YEAR %in% styr:endyr, ]
  
  # Group by relevant columns and calculate sample size n, mean, and std dev
  effort_stats <- effort %>%
    #filter(YEAR >= styr & YEAR <= endyr) %>%
    group_by(WAVE, MODE_FX_F, AREA_X_F) %>% #add year?
    summarise(
      mean_trips = mean(ESTRIPS, na.rm = TRUE),
      sd_trips = sd(ESTRIPS, na.rm = TRUE),
      n = n(), # Calculate sample size for each group
      .groups = "drop",
      #across(ESTRIPS)
    )
  
  trips <- effort_prelim %>% left_join(effort_stats, by = c("WAVE","MODE_FX_F","AREA_X_F"))
  ##### Effort section
  # Apply the Thompson Tau calculation
  trips <- trips %>%
    rowwise() %>%
    mutate(outlier = {
      # Calculate tau critical value
      if (n > 2) {
        t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
        tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
        abs(ESTRIPS - mean_trips) / sd_trips > tau
      } else {
        FALSE # Not enough data to calculate outliers
      }
    })
  
  effort_outliers <- trips %>%
    filter(outlier == TRUE)
  write.csv(effort_outliers, "effort_outliers.csv")
  
  #effortall <- rbind(effort, effort_prelim) #write back in filtering to only modes and areas of interest at read-in
  # Graphing of effort
  # set up effort plot function

 # closes the PDF device
  # Had an issue in the next section because of some tot_catch estimates in 2024 that aren't in the 2017-2023 summaries
  # Removed these first and put the ones being removed into a unique file so they can still be looked at
}
