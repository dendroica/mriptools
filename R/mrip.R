#' MRIP outlier
#'
#' Creates output to help you identify outliers
#' @param styr Start year
#' @param endyr End year
#' @param y_prelim The latest year in the mrip data (preliminary)
#' @param species A vector of the species to include
#' @param waves The MRIP waves to include
#' @param areas Strata in distance from shore
#' @param modes Modes of fishing
#' @param state The FIPS code for the state of interest
#' @param indir (optional) point to locally downloaded MRIP files
#' @param outdir where your files should go
#' @return Output files to explore the mrip data with the parameters entered
#' @export
#' @examples
#' mrip(
#'   styr = 2017,
#'   endyr = 2024,
#'   y_prelim = 2025,
#'   species = c("BLACK SEA BASS", "TAUTOG"),
#'   waves = c(2, 3, 4, 5, 6),
#'   areas = c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)"),
#'   modes = c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE"),
#'   state = 24,
#'   outdir = "~/output/mrip_ex"
#' )
mrip <- function(
    styr,
    endyr,
    y_prelim = NA,
    species, waves,
    areas,
    modes,
    state,
    indir = NULL,
    outdir) {
  myyrs <- c(styr:endyr, y_prelim)
  if (is.null(indir)) {
    indir <- "https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads"
    tmp <- readLines(paste0(indir, "/"))
    fiels <- tmp[c(grep("zip", tmp), grep(".csv", tmp, fixed = TRUE))]
    filenames <- gsub(".*(mr[a-z0-9_]*[.][a-z]{3}).*", "\\1", fiels)
    yrs <- years(filenames, myyrs)
    # function(x, y, src, state, species, waves, areas, modes)
    vars <- list(
      indir = indir,
      state,
      species,
      waves,
      areas,
      modes
    )
    mripdata <- Map(readmripfiles, names(yrs), yrs, MoreArgs = vars)
    print("download complete")
  } else {
    filenames <- list.files(indir)
    yrs <- years(filenames, myyrs)
    vars <- list(
      indir = indir,
      state = state,
      species = species,
      waves = waves,
      areas = areas,
      modes = modes
    )
    mripdata <- Map(readmripfiles, names(yrs), yrs, MoreArgs = vars)
    names(mripdata) <- names(yrs)
    print("read in complete")
  }

  mripdata <- list(
    mripdata[which(grepl("mrip_catch_bywave", names(mripdata)))],
    mripdata[which(grepl("mrip_effort_bywave", names(mripdata)))]
  )
  print("mrip data sorted")
  catchall <- do.call(rbind, mripdata[[1]])
  print("catch data compiled")
  catchall <- catchall[!duplicated(catchall), ]
  print("catch data cleaned")

  catch_prelim <- catchall[catchall$YEAR == y_prelim, ] # year for comparison
  compute_subset <- catch_prelim[, c("TOT_CAT", "LANDING", "ESTREL")]
  groupvar <- list(COMMON = catch_prelim$COMMON, WAVE = catch_prelim$WAVE)
  totcat_prelim <- aggregate(compute_subset, groupvar, sum)
  
  catch <- catchall[catchall$YEAR %in% styr:endyr, ] # years for baseline/ave

  # For looking for outliers by species across the modes and areas
  # to get wave level estimates by species for each year
  res <- aggregate(
    catch[, c("TOT_CAT", "LANDING", "ESTREL")],
    list(COMMON = catch$COMMON, YEAR = catch$YEAR, WAVE = catch$WAVE),
    sum
  )
  # Join mripdata with calculated harvest_stats (mean, sd, and n)
  vats <- c("TOT_CAT", "LANDING", "ESTREL")
  factyors <- c("COMMON", "WAVE")
  compute_subset <- res[, c(factyors, vats)]
  outlie(vats, compute_subset, totcat_prelim, factyors, outdir)

  ##### TOTAL CATCH COMPARISONS######

  # prelim calculations###########
  # totcat_notCommon <- totcat[is.na(totcat$n), ]

  all_combinations <- expand.grid(
    COMMON = species,
    YEAR = myyrs,
    WAVE = waves,
    MODE_FX_F = modes,
    AREA_X_F = areas
  )

  # Merge with the original mripdata frame
  combined_catch <- merge(all_combinations, catchall, all.x = TRUE)
  # Optionally, replace NA values with 0...
  combined_catch <- combined_catch[combined_catch$COMMON %in% species,]
  combined_catch$YEAR <- as.factor(combined_catch$YEAR)
  combined_catch$WAVE <- as.factor(combined_catch$WAVE)
  combined_catch$MODE_FX_F <- as.factor(combined_catch$MODE_FX_F)
  combined_catch$AREA_X_F <- as.factor(combined_catch$AREA_X_F)
  combined_catch$COMMON <- as.factor(combined_catch$COMMON)

  ################## EFFORT
  effortall <- do.call(rbind, mripdata[[2]])
  print("effort data compiled")
  effortall <- effortall[!duplicated(effortall), ]
  print("effort data cleaned")

  effort_prelim <- effortall[effortall$YEAR == y_prelim, ]
  effort <- effortall[effortall$YEAR %in% styr:endyr, ]

  compute_subset <- effort[, c("WAVE", "MODE_FX_F", "AREA_X_F", "ESTRIPS")]
  trips <- outlie("ESTRIPS", compute_subset, effort_prelim, c("WAVE", "MODE_FX_F", "AREA_X_F"), outdir)
  
  effortall$YEAR <- as.factor(effortall$YEAR)
  makeplots(combined_catch, effortall, species, waves, outdir)
}

#' Catch mripdata
#'
#' subsets to just your state's mripdata
#' need same number of columns for this to work
#'
#' @noRd
readcatch <- function(filen, state, species, waves) {
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
  readin <- subset(readin, ST == state & readin$WAVE %in% waves) #COMMON %in% species 
  return(readin)
}

#' Effort mripdata
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

readmrip <- function(filen, state, species, waves, areas, modes) {
  if (length(grep("mrip_catch_bywave_", filen)) > 0) {
    mripdata <- readcatch(
      filen,
      state = state,
      species = species,
      waves = waves
    )
  } else if (length(grep("mrip_effort_bywave_", filen)) > 0) {
    mripdata <- readeffort(
      filen,
      state = state,
      waves = waves,
      areas = areas,
      modes = modes
    )
  }
}

years <- function(filenames, myyrs) {
  findyrs <- regexpr("[0-9]{4}(_[0-9]{4})*", filenames)
  yrsin <- regmatches(filenames, findyrs)
  yrs <- sapply(sapply(yrsin, strsplit, split = "_"), as.integer)
  multiyr <- yrs[which(sapply(yrs, length) > 1)]
  singleyr <- yrs[which(sapply(yrs, length) < 2)]
  yrs <- c(lapply(multiyr, \(x) x[1]:x[2]), singleyr)
  names(yrs) <- filenames
  yrs <- lapply(yrs, function(x) x[x %in% myyrs])
  yrs <- Filter(length, yrs)
}

readmripfiles <- function(
    x,
    y,
    indir,
    state,
    species,
    waves,
    areas,
    modes) {
  
  print(x) # x <- names(yrs)
  print(y) # y <- yrs

  path <- file.path(indir, x)

  if (tools::file_ext(x) == "zip") {
    temp <- tempfile()
    temp2 <- tempfile()
    if (grepl("^https", indir)) {
      download.file(path, temp)
      unzip(zipfile = temp, exdir = temp2)
    } else {
      unzip(zipfile = path, exdir = temp2)
    }
    findfiles <- sapply(unlist(unname(y)), \(b) grep(b, list.files(temp2)))
    files <- list.files(temp2)[findfiles]
    mripdata <- do.call(rbind, lapply(files, function(z) {
      filepath <- file.path(temp2, z)
      mripdata <- readmrip(filepath, state, species, waves, areas, modes)
      print(mripdata[1:5, 1:12])
      return(mripdata)
    }))
    unlink(temp)
    unlink(temp2)
  } else {
    mripdata <- readmrip(path, state, species, waves, areas, modes)
    print(mripdata[1:5, 1:12])
  }
  # print(mripdata[1:5,1:10])
  return(mripdata)
}

tau <- function(n, sum_catch, mean_catch, sd_catch) {
  if (n > 2) {
    t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
    tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
    outlier <- abs(sum_catch - mean_catch) / sd_catch > tau
  } else {
    outlier <- FALSE # Not enough mripdata to calculate outliers
  }
  return(outlier)
}

agg <- function(vats, ids, compute_subset) {
  agged <- aggregate(compute_subset[, vats],
    by = lapply(ids, \(x) compute_subset[, x]),
    FUN = \(x) c(mn = mean(x), n = length(x), sd = sd(x))
  )
  names(agged)[1:length(ids)] <- ids
  namefill <- (length(ids) + 1):(length(ids) + length(vats))
  names(agged)[namefill] <- vats
  return(agged)
}

outlie <- function(vats, compute_subset, totcat_prelim, mergeby, outdir) {
  df <- agg(vats, mergeby, compute_subset)
  lapply(vats, function(x) {
    sumstats <- df[, x]
    baseline <- cbind(df[, mergeby], sumstats)
    baseline <- baseline[!is.na(baseline$n), ]

    comp <- totcat_prelim[, mergeby]
    comp$val <- totcat_prelim[, x]
    comp <- merge(comp, baseline, by = mergeby, all.x = T)
    comp <- comp[!apply(comp, 1, function(x) any(is.na(x))),]
    comp$outlier <- mapply(tau, comp$n, comp$val, comp$mn, comp$sd)
    totcat_outliers <- comp[comp$outlier == TRUE, ]
    write.csv(totcat_outliers, file.path(outdir, paste(x, "outliers.csv", sep = "-")))
    return(comp)
  })
}
