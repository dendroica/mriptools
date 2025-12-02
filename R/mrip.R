#' MRIP data
#'
#' Creates output to help you identify outliers
#' @param comparison_timespan Years to include to calculate the baseline
#' @param prelim_yr The latest year in the MRIP data (preliminary)
#' @param waves The MRIP waves to include
#' @param areas Strata in distance from shore
#' @param modes Modes of fishing
#' @param state The FIPS code for the state of interest
#' @param in_dir (optional) point to locally downloaded MRIP files
#' @return List of catch and effort data
#' @export
#' @examples
#' MRIPData(
#'   comparison_timespan = 2017:2024,
#'   prelim_yr = 2025,
#'   waves = c(2, 3, 4, 5, 6),
#'   areas = c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)"),
#'   modes = c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE"),
#'   state = 24
#' )
MRIPData <- function(
    comparison_timespan,
    prelim_yr = NA,
    waves,
    areas,
    modes,
    state,
    in_dir = NULL) {
  time_pd <- c(comparison_timespan, prelim_yr)
  if (is.null(in_dir)) {
    in_dir <- "https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads"
    website <- readLines(paste0(in_dir, "/"))
    files <- website[c(grep("zip", website), grep(".csv", website, fixed = TRUE))]
    filenames <- gsub(".*(mr[a-z0-9_]*[.][a-z]{3}).*", "\\1", files)
  } else {
    filenames <- list.files(in_dir)
  }
  filenames <- MatchYrs(filenames, time_pd)
  # function(x, y, src, state, species, waves, areas, modes)
  vars <- list(
    in_dir = in_dir,
    state,
    waves,
    areas,
    modes
  )
  mrip_data <- ReadInMRIP(filenames, vars)
  # print("download complete")
  names(mrip_data) <- names(filenames)
  # print("read in complete")

  mrip_data <- list(
    mrip_data[which(grepl("mrip_catch_bywave", names(mrip_data)))],
    mrip_data[which(grepl("mrip_effort_bywave", names(mrip_data)))]
  )
  print("mrip data sorted")
  catch <- do.call(rbind, mrip_data[[1]])
  print("catch data compiled")
  catch <- catch[!duplicated(catch), ]
  print("catch data cleaned")

  effort <- do.call(rbind, mrip_data[[2]])
  print("effort data compiled")
  effort <- effort[!duplicated(effort), ]
  print("effort data cleaned")
  return(list(catch, effort))
}

#' MRIP outlier
#'
#' Creates output to help you identify outliers
#' @param catch Compiled MRIP catch data
#' @param effort Compiled MRIP effort data
#' @param comparison_timespan Years to include to calculate the baseline
#' @param prelim_yr The latest year in the mrip data (preliminary)
#' @param species A vector of the species to include
#' @param aggregate_factors (optional) Vector of variables to aggregate over
#' @return Outlier flagging
#' @export
#' @examples
#' MRIPOutlier(catch, effort, comparison_timepsan, prelim_yr, species)
MRIPOutlier <- function(catch, effort, comparison_timespan, prelim_yr, species,
                        aggregate_factors=c("COMMON", "WAVE")) {
  base_catch <- catch[catch$YEAR %in% comparison_timespan, ] # years for base_catch/ave
  prelim_catch <- catch[catch$YEAR == prelim_yr, ] # year for comparison
  vars_of_interest <- c("TOT_CAT", "LANDING", "ESTREL")
  prelim_aggregates <- lapply(aggregate_factors, function(x) {
    prelim_catch[,which(names(prelim_catch)==x)]
  })
  prelim_catch2 <- aggregate(prelim_catch[, vars_of_interest],
                             prelim_aggregates, sum)
  names(prelim_catch2)[1:length(aggregate_factors)] <- aggregate_factors
  
  # For looking for outliers by species across the modes and areas
  # to get wave level estimates by species for each year
  base_aggregates <- lapply(c(aggregate_factors,"YEAR"), function(x) {
    base_catch[,which(names(base_catch)==x)]
  })
  base_catch2 <- aggregate(base_catch[, vars_of_interest],
                           base_aggregates, sum)
  names(base_catch2)[1:length(aggregate_factors)] <- aggregate_factors
  # Join mrip_data with calculated harvest_stats (mean, sd, and n)
  
  base_catch <- base_catch2[, c(aggregate_factors, vars_of_interest)]
  catch_outlier <- outlie(base_catch, prelim_catch2, vars_of_interest, aggregate_factors)
  
  prelim_effort <- effort[effort$YEAR == prelim_yr, ]
  base_effort <- effort[effort$YEAR %in% comparison_timespan, ]
  effort_outlier <- outlie(base_effort, prelim_effort, "ESTRIPS", c("WAVE", "MODE_FX_F", "AREA_X_F"))
  outliers <- c(catch_outlier, effort_outlier)
  return(outliers)
}

#' MRIP analysis
#'
#' Creates output to help you identify outliers
#' @param comparison_timespan Years to include to calculate the baseline
#' @param prelim_yr The latest year in the mrip data (preliminary)
#' @param species A vector of the species to include
#' @param waves The MRIP waves to include
#' @param areas Strata in distance from shore
#' @param modes Modes of fishing
#' @param state The FIPS code for the state of interest
#' @param in_dir (optional) point to locally downloaded MRIP files
#' @param out_dir where your files should go
#' @return Output files to explore the mrip data with the parameters entered
#' @export
#' @examples
#' mrip(
#'   comparison_timespan = 2017:2024,
#'   prelim_yr = 2025,
#'   species = c("BLACK SEA BASS", "TAUTOG"),
#'   waves = c(2, 3, 4, 5, 6),
#'   areas = c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)"),
#'   modes = c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE"),
#'   state = 24,
#'   out_dir = "~/output/mrip_ex"
#' )
mrip <- function(comparison_timespan, prelim_yr, species, waves, areas, modes, state, in_dir = NULL, out_dir) {
  mrip_data <- MRIPData(
    comparison_timespan,
    prelim_yr,
    waves,
    areas,
    modes,
    state,
    in_dir
  )
  combined_catch <- MRIPOutlier(
    mrip_data[[1]], mrip_data[[2]], comparison_timespan, prelim_yr, species
  )
  writeout(combined_catch, out_dir)
  makeplots(mrip_data[[1]], mrip_data[[2]], species, waves, out_dir)
}

#' Catch mrip_data
#'
#' subsets to just your state's mrip_data
#' need same number of columns for this to work
#'
#' @noRd
readcatch <- function(filen, state, waves) {
  readin <- read.csv(filen, colClasses = c("SP_CODE" = "character"))
  # readr::read_csv(filen,
  # na = "",
  # col_types = readr::cols(
  # LAND_VAR = readr::col_number(),
  # ALT_FLAG = readr::col_integer(),
  # YEAR = readr::col_integer(),
  # WAVE = readr::col_integer(),
  # SUB_REG = readr::col_integer(),
  # ST = readr::col_integer(),
  # MODE_FX = readr::col_integer(),
  # AREA_X = readr::col_integer(),
  # ESTCLAIM = readr::col_integer(),
  # ESTCLVAR = readr::col_integer(),
  # LOWER_ESTCLAIM = readr::col_integer(),
  # UPPER_ESTCLAIM = readr::col_integer(),
  # PC_ESTCLAIM_IMP=readr::col_integer(),
  # ESTHARV = readr::col_integer(),
  # ESTHVAR = readr::col_integer(),
  # LOWER_ESTHARV = readr::col_integer(),
  # UPPER_ESTHARV = readr::col_integer(),
  # LANDING = readr::col_integer(),
  # LAND_VAR = readr::col_integer(),
  # LOWER_LANDING = readr::col_integer(),
  # SP_CODE = readr::col_character(), #, #UPPER_LANDING = readr::col_integer(),
  # ESTREL = readr::col_integer(),
  # ESTRLVAR = readr::col_integer(), LOWER_ESTREL = readr::col_integer(), UPPER_ESTREL = readr::col_integer()#,
  # TOT_VAR = readr::col_integer(), UPPER_TOT_CAT = readr::col_integer(), LBS_AB1 = readr::col_integer(),
  # VAR_LBS = readr::col_integer(), LOWER_LBS_AB1 = readr::col_integer(), UPPER_LBS_AB1 = readr::col_integer(),
  # WGT_AB1 = readr::col_integer(), VAR_WAB1 = readr::col_integer(), LOWER_WGT_AB1 = readr::col_integer(),
  # UPPER_WGT_AB1 = readr::col_integer(), TOT_LEN = readr::col_integer(),
  # VARTOLEN=readr::col_integer(),
  # LOWER_TOT_LEN = readr::col_integer(), UPPER_TOT_LEN = readr::col_integer(), MISS_FISH = readr::col_integer(),
  # ), lazy=T
  # )

  numvars <- c(names(which(apply(readin, 2, \(x) any(grepl("[[:digit:]]", x))))))
  numvars <- numvars[!numvars %in% c("AREA_X_F", "SP_CODE")]
  readin[, numvars] <- apply(
    readin[, numvars],
    2,
    \(x) as.numeric(gsub(",", "", x))
  )
  names(readin) <- toupper(names(readin))
  readin <- subset(readin, ST == state & readin$WAVE %in% waves) # COMMON %in% species
  return(readin)
}

#' Effort mrip_data
#'
#' @noRd
readeffort <- function(filen, state, waves, areas, modes) {
  readin <- read.csv(filen)
  num <- apply(readin, 2, \(x) any(grepl("[[:digit:]]", x)))
  numvars <- c(names(which(num)))
  numvars <- numvars[!numvars %in% c("AREA_X_F", "SP_CODE")]
  readin[, numvars] <- apply(
    readin[, numvars],
    2,
    \(x) as.numeric(gsub(",", "", x))
  )
  names(readin) <- toupper(names(readin))
  readin <- subset(
    readin,
    ST == state & WAVE %in% waves & AREA_X_F %in% areas & MODE_FX_F %in% modes
  )
  return(readin)
}

readmripdata <- function(filen, state, waves, areas, modes) {
  if (length(grep("mrip_catch_bywave_", filen)) > 0) {
    mrip_data <- readcatch(
      filen,
      state = state,
      waves = waves
    )
  } else if (length(grep("mrip_effort_bywave_", filen)) > 0) {
    mrip_data <- readeffort(
      filen,
      state = state,
      waves = waves,
      areas = areas,
      modes = modes
    )
  }
}

MatchYrs <- function(filenames, time_pd) {
  yr_regex <- regexpr("[0-9]{4}(_[0-9]{4})*", filenames)
  yrs <- regmatches(filenames, yr_regex)
  yrs <- sapply(sapply(yrs, strsplit, split = "_"), as.integer)
  multiyr <- yrs[which(sapply(yrs, length) > 1)]
  single_yr <- yrs[which(sapply(yrs, length) < 2)]
  yrs <- c(lapply(multiyr, \(x) x[1]:x[2]), single_yr)
  names(yrs) <- filenames
  yrs <- lapply(yrs, \(x) x[x %in% time_pd])
  filenames <- Filter(length, yrs)
}

ReadInMRIP <- function(filenames, vars) {
  Map(ReadMRIP, names(filenames), filenames, MoreArgs = vars)
}

ReadMRIP <- function(
    x,
    y,
    in_dir,
    state,
    waves,
    areas,
    modes) {
  print(x) # x <- names(filenames)
  print(y) # y <- filenames

  path <- file.path(in_dir, x)

  if (tools::file_ext(x) == "zip") {
    temp <- tempfile()
    temp2 <- tempfile()
    if (grepl("^https", in_dir)) {
      download.file(path, temp)
      unzip(zipfile = temp, exdir = temp2)
    } else {
      unzip(zipfile = path, exdir = temp2)
    }
    match_files <- sapply(unlist(unname(y)), \(b) grep(b, list.files(temp2)))
    files_that_match <- list.files(temp2)[match_files]
    mrip_data <- do.call(rbind, lapply(files_that_match, function(z) {
      file_path <- file.path(temp2, z)
      mrip_data <- readmripdata(file_path, state, waves, areas, modes)
      print(mrip_data[1:5, 1:12])
      return(mrip_data)
    }))
    unlink(temp)
    unlink(temp2)
  } else {
    mrip_data <- readmripdata(path, state, waves, areas, modes)
    print(mrip_data[1:5, 1:12])
  }
  # print(mrip_data[1:5,1:10])
  return(mrip_data)
}

tau <- function(n, sum_catch, mean_catch, sd_catch) {
  if (n > 2) {
    t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
    tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
    outlier <- abs(sum_catch - mean_catch) / sd_catch > tau
  } else {
    outlier <- FALSE # Not enough mrip_data to calculate outliers
  }
  return(outlier)
}

agg <- function(vars_of_interest, ids, base_catch) {
  agged <- aggregate(base_catch[, vars_of_interest],
    by = lapply(ids, \(x) base_catch[, x]),
    FUN = \(x) c(mn = mean(x), n = length(x), sd = sd(x))
  )
  names(agged)[1:length(ids)] <- ids
  namefill <- (length(ids) + 1):(length(ids) + length(vars_of_interest))
  names(agged)[namefill] <- vars_of_interest
  return(agged)
}

outlie <- function(base_catch, totcat_prelim, vars_of_interest, mergeby) {
  df <- agg(vars_of_interest, mergeby, base_catch)
  lapply(vars_of_interest, function(x) {
    sumstats <- df[, x]
    base_catch <- cbind(df[, mergeby], sumstats)
    base_catch <- base_catch[!is.na(base_catch$n), ]

    comp <- totcat_prelim[, mergeby]
    comp$val <- totcat_prelim[, x]
    comp <- merge(comp, base_catch, by = mergeby, all.x = T)
    comp <- comp[!apply(comp, 1, function(x) any(is.na(x))), ]
    comp$outlier <- mapply(tau, comp$n, comp$val, comp$mn, comp$sd)
    comp$var <- x
    return(comp)
  })
}

#' Write outliers
#'
#' Creates output to help you identify outliers
#' @param comp Outlier dataframe
#' @param out_dir where your files should go
#' @return Output files to explore the mrip data with the parameters entered
#' @export
#' @examples
#' writeout(
#'   comp = comp,
#'   out_dir = "~/output/mrip_ex"
#' )
writeout <- function(comp, out_dir) {
  lapply(comp, function(y) {
    x <- y$var[1]
    totcat_outliers <- y[which(y$outlier), ]
    write.csv(totcat_outliers, file.path(out_dir, paste(x, "outliers.csv", sep = "-")))
  })
}
