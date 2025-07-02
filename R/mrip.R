# catch data
# switched to readr::read_csv rather than read.csv due to issues setting catch estimates with commas in them to numeric values
# Info on setting columns to specific data types and options can be found https://stackoverflow.com/questions/31568409/override-column-types-when-importing-data-using-readrread-csv-when-there-are
# subsets to just your state's data #need same number of columns for this to work
# need same number of columns for this to work
readcatch <- function(filen, state, species, waves) {
  #if (length(grep("zip", x))) {
  #  filen <- archive::archive_read(x, file = xfile)
  #} else {
  #  filen <- x
  #}
  C.tmp <- read.csv(filen, colClasses=c("SP_CODE"="character"))
    #readr::read_csv(filen,
    #na = "",
    #col_types = readr::cols(
      #LAND_VAR = readr::col_number(),
      #ALT_FLAG = readr::col_integer(),
      #YEAR = readr::col_integer(),
      #WAVE = readr::col_integer(),
      #SUB_REG = readr::col_integer(),
      #ST = readr::col_integer(),
      #MODE_FX = readr::col_integer(),
      #AREA_X = readr::col_integer(),
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
      #LOWER_LANDING = readr::col_integer(), 
      #SP_CODE = readr::col_character(), #, #UPPER_LANDING = readr::col_integer(),
      #ESTREL = readr::col_integer(),
      #ESTRLVAR = readr::col_integer(), LOWER_ESTREL = readr::col_integer(), UPPER_ESTREL = readr::col_integer()#,
      #TOT_VAR = readr::col_integer(), UPPER_TOT_CAT = readr::col_integer(), LBS_AB1 = readr::col_integer(),
      #VAR_LBS = readr::col_integer(), LOWER_LBS_AB1 = readr::col_integer(), UPPER_LBS_AB1 = readr::col_integer(),
      #WGT_AB1 = readr::col_integer(), VAR_WAB1 = readr::col_integer(), LOWER_WGT_AB1 = readr::col_integer(),
      #UPPER_WGT_AB1 = readr::col_integer(), TOT_LEN = readr::col_integer(),
      # VARTOLEN=readr::col_integer(),
      #LOWER_TOT_LEN = readr::col_integer(), UPPER_TOT_LEN = readr::col_integer(), MISS_FISH = readr::col_integer(), 
    #), lazy=T
  #)
  
  numvars <- c(names(which(apply(C.tmp, 2, function(x) any(grepl("[[:digit:]]", x))))))
  numvars <- numvars[!numvars %in% c("AREA_X_F", "SP_CODE")]
  C.tmp[,numvars] <- apply(C.tmp[,numvars], 2, function(x) as.numeric(gsub("," ,"", x))) 
  names(C.tmp) <- toupper(names(C.tmp))
  C.tmp <- C.tmp[C.tmp$ST == state & C.tmp$COMMON %in% species & C.tmp$WAVE %in% waves,] #& AREA_X_F %in% areas & MODE_FX_F %in% modes)
  return(C.tmp)
}

readeffort <- function(filen, state, waves, areas, modes) {
  #if (length(grep("z", x))) {
  #  filen <- archive::archive_read(x, file = xfile)
  #} else {
  #  filen <- x
  #}
  C.tmp <- read.csv(filen)
  numvars <- c(names(which(apply(C.tmp, 2, function(x) any(grepl("[[:digit:]]", x))))))
  numvars <- numvars[!numvars %in% c("AREA_X_F", "SP_CODE")]
  C.tmp[,numvars] <- apply(C.tmp[,numvars], 2, function(x) as.numeric(gsub("," ,"", x))) 
  
  C.tmp <- C.tmp[C.tmp$ST == state & C.tmp$WAVE %in% waves & C.tmp$AREA_X_F %in% areas & C.tmp$MODE_FX_F %in% modes,]
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
#' @importFrom tidyr complete
#' @importFrom RCurl getURL
#' @examples
#' mrip(2022, 2023, 2024, c("SUMMER FLOUNDER", "TAUTOG"), c(3, 4), c("INLAND", "OCEAN (<= 3 MI)"), c("CHARTER BOAT", "PARTY BOAT"), 24)

mrip <- function(styr, endyr, y_prelim = NA, species, waves, areas, modes, state, input=NULL) {
  url <- "https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads/"
  filenames <- paste(url, strsplit(RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r*\n")[[1]], sep = "")
  filenames <- gsub('.*(mr[a-z0-9_]*[.][a-z]{3}).*','\\1', filenames[c(grep("zip", filenames), grep(".csv", filenames, fixed = TRUE))])
  yrs <- sapply(sapply(regmatches(filenames, regexpr("[0-9]{4}(_[0-9]{4})*", filenames)), strsplit, split = "_"), as.integer)
  yrs <- c(lapply(yrs[which(sapply(yrs, length) > 1)], function(x) x[1]:x[2]), yrs[which(sapply(yrs, length) < 2)])
  names(yrs) <- filenames
  myyrs <- c(styr:endyr, y_prelim)
  yrs <- lapply(yrs, function(x) x[x %in% myyrs])
  yrs <- Filter(length, yrs)
  
  # Combine files for catch and effort estimates for the "bywave" files
  # Code modified from a loop written by Katie Drew (ASMFC)
  data <- Map(function(x,y) {
    #x <- names(yrs)
    #y <- yrs

  path <- paste0(url, x)
  print(path)
  if (tools::file_ext(x) == "zip") {
    temp <- tempfile()
    temp2 <- tempfile()
    download.file(path, temp)
    unzip(zipfile = temp, exdir = temp2)
    if (length(grep("ca", x)) > 0) {
      data <- do.call(rbind, lapply(y, function(y) {
        file <- paste0("mrip_catch_bywave_", y, ".csv")
        data <- readcatch(file.path(temp2, file), state, species, waves)
      }))
      } else {
      data <- do.call(rbind, lapply(y, function(y) {
          file <- paste0("mrip_effort_bywave_", y, ".csv")
          data <- readeffort(file.path(temp2, file), state, waves, areas, modes)}))
     }
    unlink(c(temp, temp2))
   } else {
    if (length(grep("ca", x)) > 0) {
      data <- readcatch(path, state=state, species=species, waves=waves)
    } else {
      data <- readeffort(path, state=state, waves=waves, areas=areas, modes=modes)
    }
  }
  return(data)}, names(yrs), yrs)

  # now as it is, data is a nested list...
  # first level: year
  # 2nd level: 1 - catch, 2 - effort
  data <- list(data[which(grepl("catch",names(data)))], data[which(grepl("effort",names(data)))])
  catchall <- do.call(rbind, data[[1]])
  
  catch_prelim <- catchall[catchall$YEAR == y_prelim,]
  catch <- catchall[catchall$YEAR %in% styr:endyr, ]
  # For looking for outliers by species at the wave level, first need to collapse estimates
  # across the modes and areas to get wave level estimates by species for each year
  
  res <- by(catch, list(catch$COMMON, catch$YEAR, catch$WAVE), function(x) {
    #if (nrow(x) > 1000) {
      c(
        COMMON = unique(x$COMMON),
        YEAR = unique(x$YEAR),
        WAVE = unique(x$WAVE),
        sum_totcat = sum(x$TOT_CAT, na.rm = TRUE),
        sum_land = sum(x$LANDING, na.rm = TRUE),
        sum_rel = sum(x$ESTREL, na.rm = TRUE))
    }
) #PICK BACK UP HERE 6/27/25...COMES OUT AS CHAR
  
  df <- as.data.frame(do.call(rbind, res))
  df[,2:6] <- sapply(df[,2:6], as.numeric)
  catch_summed <- df[order(df$COMMON, df$YEAR),]
  
  #catch_summed <- catch %>%
  #  group_by(COMMON, YEAR, WAVE) %>%
  #  summarise(
  #    sum_totcat = sum(TOT_CAT, na.rm = TRUE),
  #    sum_land = sum(LANDING, na.rm = TRUE),
  #    sum_rel = sum(ESTREL, na.rm = TRUE),
  #    .groups = "drop"
  #  )
  
  res <- by(catch_summed, list(catch_summed$COMMON, catch_summed$WAVE), function(x) {
    #if (nrow(x) > 1000) {
    c(
      COMMON = unique(x$COMMON),
      WAVE = unique(x$WAVE),
      mean_catch = mean(x$sum_totcat, na.rm = TRUE),
      sd_catch = sd(x$sum_totcat, na.rm = TRUE),
      mean_catch_l = mean(x$sum_land, na.rm = TRUE),
      sd_catch_l = sd(x$sum_land, na.rm = TRUE),
      mean_catch_r = mean(x$sum_rel, na.rm = TRUE),
      sd_catch_r = sd(x$sum_rel, na.rm = TRUE),
      n = nrow(x))
  }
  )
  df <- as.data.frame(do.call(rbind, res))
  df[,2:9] <- sapply(df[,2:9], as.numeric)
  totcat_stats <- df[order(df$COMMON, df$WAVE),]
  
  # Group by relevant columns and calculate sample size n, mean, and std dev
  #totcat_stats <- catch_summed %>%
  #  group_by(COMMON, WAVE) %>%
  #  summarise(
  #    mean_catch = mean(sum_totcat, na.rm = TRUE),
  #    sd_catch = sd(sum_totcat, na.rm = TRUE),
  #    mean_catch_l = mean(sum_land, na.rm = TRUE),
  #    sd_catch_l = sd(sum_land, na.rm = TRUE),
  #    mean_catch_r = mean(sum_rel, na.rm = TRUE),
  #    sd_catch_r = sd(sum_rel, na.rm = TRUE),
  #    n = n(), # Calculate sample size for each group
  #    .groups = "drop"
  #  )
  
  res <- by(catch_prelim, list(catch_prelim$COMMON, catch_prelim$WAVE), function(x) {
    #if (nrow(x) > 1000) {
    c(
      COMMON = unique(x$COMMON),
      #YEAR = unique(x$YEAR),
      WAVE = unique(x$WAVE),
      sum_totcat = sum(x$TOT_CAT, na.rm = TRUE),
      sum_land = sum(x$LANDING, na.rm = TRUE),
      sum_rel = sum(x$ESTREL, na.rm = TRUE))
  })
  
  df <- as.data.frame(do.call(rbind, res))
  df[,2:5] <- sapply(df[,2:5], as.numeric)
  totcat_prelim <- df[order(df$COMMON),]
  
    #totcat_prelim <- catch_prelim %>%
     # group_by(COMMON, WAVE) %>%
    #  summarise(
    #    sum_catch = sum(TOT_CAT, na.rm = TRUE),
    #    sum_land = sum(LANDING, na.rm = TRUE),
    #    sum_rel = sum(ESTREL, na.rm = TRUE),
    #    .groups = 'drop'
    #  )
    
    # Join 2024 data with calculated harvest_stats (mean, sd, and n) PICK UP HERE  JMGO
    totcat <- merge(totcat_prelim, totcat_stats, by = c("COMMON", "WAVE"), all.x=T)
    
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
    
    totcat$outlier_CATCH <- mapply(tau, totcat$n, totcat$sum_totcat, totcat$mean_catch, totcat$sd_catch)
    totcat$outlier_LAND <- mapply(tau, totcat$n, totcat$sum_land, totcat$mean_catch_l, totcat$sd_catch_l)
    totcat$outlier <- mapply(tau, totcat$n, totcat$sum_rel, totcat$mean_catch_r, totcat$sd_catch_r)
    
    #totcat <- totcat %>%
    #  rowwise() %>%
    #  mutate(
    #  outlier_CATCH = tau(n, sum_totcat, mean_catch, sd_catch),
    #  outlier_LAND = tau(n, sum_land, mean_catch_l, sd_catch_l),
    #  outlier = tau(n, sum_rel, mean_catch_r, sd_catch_r))
    
    # Filter the outliers
    totcat_outliers <- totcat[totcat$outlier_CATCH == TRUE,]
    write.csv(totcat_outliers, "totcat_outliers.csv")
    
    land_outliers <- totcat[totcat$outlier_LAND == TRUE,]
    write.csv(land_outliers, "landings_outliers.csv")
    
    rel_outliers <- totcat[totcat$outlier == TRUE,]
    write.csv(rel_outliers, "release_outliers.csv")
    
    combined_catch <- tidyr::complete(catchall, COMMON = species, YEAR = styr:endyr + 1, WAVE = waves, MODE_FX_F = modes, AREA_X_F = areas)
    
    combined_catch$YEAR <- as.factor(combined_catch$YEAR)
    combined_catch$WAVE <- as.factor(combined_catch$WAVE)
    combined_catch$MODE_FX_F <- as.factor(combined_catch$MODE_FX_F)
    combined_catch$AREA_X_F <- as.factor(combined_catch$AREA_X_F)
    combined_catch$COMMON <- as.factor(combined_catch$COMMON)
    
    # Wanted to graph the outputs to see how catch levels & PSEs compare across years by wave, mode, and area
    # Graphed according to the species list, waves, areas, and modes you set in the beginning section of code
    
    # set up total catch plot function
    totcatplot <- function(wavenum, species) {
      df <- combined_catch[combined_catch$COMMON==species & combined_catch$WAVE==wavenum,]
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
      df <- combined_catch[combined_catch$COMMON==species & combined_catch$WAVE==wavenum,]
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
      df <- combined_catch[combined_catch$COMMON==species & combined_catch$WAVE==wavenum,]
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
    pdf("Releases.pdf")
    for (s in species) {
      for (w in waves) {
        relplot(w, s)
      }
    }
    dev.off() # closes the PDF device
  
  ##################EFFORT
  effortall <- do.call(rbind, data[[2]])
  
  effplot <- function(wavenum) {
    df <- effortall[effortall$WAVE==wavenum,]
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
  pdf("EstTrips.pdf")
  for (w in waves) {
    effplot(w)
  }
  dev.off() # closes the PDF device
  
  effort_prelim <- effortall[effortall$YEAR == y_prelim,]
  effort <- effortall[effortall$YEAR %in% styr:endyr, ]
  
  res <- by(effort, list(effort$WAVE, effort$MODE_FX_F, effort$AREA_X_F), function(x) {
    #if (nrow(x) > 1000) {
    c(MODE_FX_F = unique(x$MODE_FX_F),
      AREA_X_F = unique(x$AREA_X_F),
      WAVE = unique(x$WAVE),
      mean_trips = mean(x$ESTRIPS, na.rm = TRUE),
      sd_trips = sd(x$ESTRIPS, na.rm = TRUE),
      n = nrow(x))
  }
  )
  
  df <- as.data.frame(do.call(rbind, res))
  df[,3:6] <- sapply(df[,3:6], as.numeric)
  effort_stats <- df
  # Group by relevant columns and calculate sample size n, mean, and std dev
  #effort_stats <- effort %>%
  #  group_by(WAVE, MODE_FX_F, AREA_X_F) %>% #add year?
  #  summarise(
  #    mean_trips = mean(ESTRIPS, na.rm = TRUE),
  #    sd_trips = sd(ESTRIPS, na.rm = TRUE),
  #    n = n(), # Calculate sample size for each group
  #    .groups = "drop",
  #  )
  
  trips <- merge(effort_prelim,effort_stats, by = c("WAVE","MODE_FX_F","AREA_X_F"), all.x=T)
  ##### Effort section
  # Apply the Thompson Tau calculation
  
  trips$outlier <- mapply(tau, trips$n, trips$ESTRIPS, trips$mean_trips, trips$sd_trips)
  #trips <- trips %>%
  #  rowwise() %>%
  #  mutate(outlier = {
      # Calculate tau critical value
  #    if (n > 2) {
  #      t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
  #      tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
  #      abs(ESTRIPS - mean_trips) / sd_trips > tau
  #    } else {
  #      FALSE # Not enough data to calculate outliers
  #    }
  #  })
  
  effort_outliers <- trips[trips$outlier == TRUE,]
  write.csv(effort_outliers, "effort_outliers.csv")
  
  #effortall <- rbind(effort, effort_prelim) #write back in filtering to only modes and areas of interest at read-in
  # Graphing of effort
  # set up effort plot function

 # closes the PDF device
  # Had an issue in the next section because of some tot_catch estimates in 2024 that aren't in the 2017-2023 summaries
  # Removed these first and put the ones being removed into a unique file so they can still be looked at
}
