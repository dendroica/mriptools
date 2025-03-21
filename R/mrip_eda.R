library(tidyverse)
library(outliers)
library(readr)
library(ggplot2)
library(tidyr)
#library(archive)
#library(RCurl)
#library(stringr)
library(dplyr) #Depends

readcatch <- function(x, xfile, state) {
  if(length(grep("z", x))) {filen <- archive::archive_read(x, file=xfile)} else {filen <- x}
  C.tmp <- readr::read_csv(filen, na = "", 
                    col_types = cols(LAND_VAR=col_number(), 
                                     ALT_FLAG=col_integer(),
                                     YEAR=col_integer(),
                                     WAVE=col_integer(),
                                     SUB_REG=col_integer(),
                                     ST=col_integer(),
                                     MODE_FX=col_integer(),
                                     AREA_X=col_integer(),
                                     ESTCLAIM=col_integer(),
                                     ESTCLVAR=col_integer(),
                                     LOWER_ESTCLAIM=col_integer(),
                                     UPPER_ESTCLAIM=col_integer(),
                                     #PC_ESTCLAIM_IMP=col_integer(),
                                     ESTHARV=col_integer(),
                                     ESTHVAR=col_integer(),
                                     LOWER_ESTHARV=col_integer(),
                                     UPPER_ESTHARV=col_integer(),
                                     LANDING=col_integer(),
                                     LAND_VAR=col_integer(),
                                     LOWER_LANDING=col_integer(), SP_CODE = col_character(), UPPER_LANDING=col_integer(),
                                     ESTREL=col_integer(),
                                     ESTRLVAR=col_integer(), LOWER_ESTREL=col_integer(), UPPER_ESTREL=col_integer(),
                                     TOT_VAR=col_integer(), UPPER_TOT_CAT=col_integer(), LBS_AB1= col_integer(), 
                                     VAR_LBS=col_integer(), LOWER_LBS_AB1=col_integer(), UPPER_LBS_AB1=col_integer(),
                                     WGT_AB1=col_integer(), VAR_WAB1=col_integer(), LOWER_WGT_AB1=col_integer(),
                                     UPPER_WGT_AB1=col_integer(), TOT_LEN=col_integer(), 
                                     #VARTOLEN=col_integer(),
                                     LOWER_TOT_LEN=col_integer(), UPPER_TOT_LEN=col_integer(), MISS_FISH=col_integer()))
  
  names(C.tmp) <- toupper(names(C.tmp))
  C.tmp <- C.tmp[C.tmp$ST==state,]
  #catch[[length(catch)+1]] <- C.tmp
  return(C.tmp)}

readeffort <- function(x, xfile, state) {
  if(length(grep("z", x))) {filen <- archive::archive_read(x, file=xfile)} else {filen <- x}
  C.tmp <- readr::read_csv(filen, na = "")
  names(C.tmp) <- toupper(names(C.tmp))
  C.tmp <- C.tmp[C.tmp$ST==state,]
  #effort[[length(effort)+1]] <- C.tmp
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
#' @examples 
#' mrip(2022,2023,2024,c("SUMMER FLOUNDER","TAUTOG"),c(3,4),c("INLAND","OCEAN (<= 3 MI)"),c("CHARTER BOAT", "PARTY BOAT"), 24);
#' @export
mrip <- function(styr, endyr, y_prelim, species, waves, areas, modes, state) {
url <- 'https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads/'
filenames = paste(url, strsplit(RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r*\n")[[1]], sep = "")
filenames <- stringr::str_extract(filenames[c(grep("zip", filenames),grep(".csv", filenames,fixed=TRUE))],"mr[a-z0-9_]*[.][a-z]*")
yrs <- stringr::str_extract(filenames, "[0-9]{4}(_[0-9]{4})*")
yrs <- sapply(sapply(yrs, strsplit, split="_"), as.integer) 
yrs <- c(lapply(yrs[which(sapply(yrs, length) > 1)], function(x) x[1]:x[2]), yrs[which(sapply(yrs, length) < 2)])

catchp <- paste0("mrip_catch_bywave_", y_prelim, "_prelim", ".csv")
effortp <- paste0("mrip_effort_bywave_", y_prelim, "_prelim", ".csv")
catch_prelim <- readcatch(paste0(url,catchp),catchp, state)
effort_prelim <- readeffort(paste0(url,effortp),effortp, state)

####You don't need to change the code past this section for your state###
#Combine files for catch and effort estimates for the "bywave" files
#Code modified from a loop written by Katie Drew (ASMFC)
data <- lapply(styr:endyr, function(y) {
  yr <- which(sapply(yrs, function(x) {y %in% x}))
  data1 <- lapply(filenames[yr],
                  function(x) {
                    path <- paste0(url, x)
                    print(path)
                    if (length(grep("ca",x)) > 0) {
                      file <- paste0("mrip_catch_bywave_", y, ".csv")
                      data <- readcatch(path,file, state)
                      
                    } else {
                      file <- paste0("mrip_effort_bywave_", y, ".csv")
                      data <- readeffort(path,file, state)
                    }
                  })
  
  return(data1)
  #catch data
  #switched to readr::read_csv rather than read.csv due to issues setting catch estimates with commas in them to numeric values
  #Info on setting columns to specific data types and options can be found https://stackoverflow.com/questions/31568409/override-column-types-when-importing-data-using-readrread-csv-when-there-are
  
  
  #subsets to just your state's data #need same number of columns for this to work
  #catch <- bind_rows(catch, C.tmp) #allows for the extra columns that were added since 2020 in public datasets
  #subsets to just your state's data
  #need same number of columns for this to work
  #effort <- bind_rows(effort, E.tmp) #allows for the extra columns that were added since 2020 in public datasets
})

#now as it is, data is a nested list...
#first level: year
#2nd level: 1 - catch, 2 - effort
#PICK UP HERE

catch <- bind_rows(lapply(data, "[[", 1))
effort <- bind_rows(lapply(data, "[[", 2))
#need combined file for graphing later in code
combined_catch <- rbind(catch, catch_prelim)
#combined_catch <- combined_catch[combined_catch$ST==state,]

#For looking for outliers by species at the wave level, first need to collapse estimates
#across the modes and areas to get wave level estimates by species for each year

#LEFT OFF HERE, PICKUP TO SPECIFY THE PACKAGES with ::
catch_summed <- catch %>%
  filter(ST==state) %>%
  group_by(COMMON, YEAR, WAVE) %>%
  summarise(
    sum_totcat = sum(TOT_CAT, na.rm = TRUE),
    sum_land = sum(LANDING, na.rm = TRUE),
    sum_rel = sum(ESTREL, na.rm = TRUE),
    .groups = 'drop'
  )

#####TOTAL CATCH COMPARISONS######

# Group by relevant columns and calculate sample size n, mean, and sd for 2017-2023
totcat_stats <- catch_summed %>%
  filter(YEAR >= styr & YEAR <= endyr) %>%
  group_by(COMMON, WAVE) %>%
  summarise(
    mean_catch = mean(sum_totcat, na.rm = TRUE),
    sd_catch = sd(sum_totcat, na.rm = TRUE),
    n = n(),  # Calculate sample size for each group
    .groups = 'drop'
  )

totcat_prelim <- catch_prelim %>%
  filter(ST==state) %>%
  group_by(COMMON, WAVE) %>%
  summarise(
    sum_catch = sum(TOT_CAT, na.rm = TRUE),
    .groups = 'drop'
  )

# Join 2024 data with calculated harvest_stats (mean, sd, and n)
totcat <- totcat_prelim %>%
  left_join(totcat_stats, by = c("COMMON", "WAVE"))

#Had an issue in the next section because of some tot_catch estimates in 2024 that aren't in the 2017-2023 summaries
#Removed these first and put the ones being removed into a unique file so they can still be looked at later
totcat_notCommon <- totcat[is.na(totcat$n),]

totcat <-totcat[!is.na(totcat$n),]

# Apply the Thompson Tau calculation
totcat <- totcat %>%
  rowwise() %>%
  
  mutate(outlier = {
    # Calculate tau critical value
    if (n > 2) {
      t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
      tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
      abs(sum_catch - mean_catch) / sd_catch > tau
    } else {
      FALSE  # Not enough data to calculate outliers
    }
  })

# Filter the outliers
totcat_outliers <- totcat %>% 
  filter (outlier == TRUE)
write.csv(totcat_outliers, "totcat_outliers.csv")

#Wanted to graph the outputs to see how catch levels & PSEs compare across years by wave, mode, and area
#Graphed according to the species list, waves, areas, and modes you set in the beginning section of code

combined_catch <- combined_catch %>%
  tidyr::complete(COMMON = species, YEAR = styr:endyr+1, WAVE = waves, MODE_FX_F = modes, AREA_X_F = areas)

combined_catch$YEAR <- as.factor(combined_catch$YEAR)
combined_catch$WAVE <- as.factor(combined_catch$WAVE)
combined_catch$MODE_FX_F <- as.factor(combined_catch$MODE_FX_F)
combined_catch$AREA_X_F <- as.factor(combined_catch$AREA_X_F)
combined_catch$COMMON <- as.factor(combined_catch$COMMON)

#set up total catch plot function
totcatplot <- function(wavenum, species){
  p <- combined_catch %>% filter(COMMON==species) %>%
    filter(WAVE==wavenum) %>%
    ggplot(aes(x=YEAR, y=TOT_CAT)) +
    geom_point() +
    geom_errorbar(aes(ymin = LOWER_TOT_CAT,ymax = UPPER_TOT_CAT)) +
    labs(title=paste0(species, " WAVE ", wavenum, " TOTAL CATCH"), y="Total Catch (numbers)") +
    facet_grid(rows=vars(MODE_FX_F), cols=vars(AREA_X_F), scales="free_y", drop=FALSE) +
    theme_bw()+
    scale_x_discrete(guide=guide_axis(n.dodge = 2))
  print(p)
}

#Loops through each species and produces a graph for each wave
pdf("Total Catch.pdf")
for(s in species){
  for (w in waves) {
    totcatplot(w,s)
  }
}
dev.off() #closes the PDF device

#####LANDING (A+B1) COMPARISONS######
# Group by relevant columns and calculate sample size n, mean, and sd for 2017-2023
land_stats <- catch_summed %>%
  filter(YEAR >= styr & YEAR <= endyr) %>%
  group_by(COMMON, WAVE) %>%
  summarise(
    mean_catch = mean(sum_land, na.rm = TRUE),
    sd_catch = sd(sum_land, na.rm = TRUE),
    n = n(),  # Calculate sample size for each group
    .groups = 'drop'
  )

land_prelim <- catch_prelim %>%
  filter(ST==state) %>%
  group_by(COMMON, WAVE) %>%
  summarise(
    sum_catch = sum(LANDING, na.rm = TRUE), #sums each species' landings across modes & areas for each wave
    .groups = 'drop'
  )

# Join 2024 data with calculated harvest_stats (mean, sd, and n)
land <- land_prelim %>%
  left_join(land_stats, by = c("COMMON", "WAVE"))

#Had an issue in the next section because of some tot_catch estimates in 2024 that aren't in the 2017-2023 summaries
#Removed these first and put the ones being removed into a unique file so they can still be looked at
land_notCommon <- land[is.na(land$n),]

land <-land[!is.na(land$n),]

# Apply the Thompson Tau calculation
land <- land %>%
  rowwise() %>%
  
  mutate(outlier = {
    # Calculate tau critical value
    if (n > 2) {
      t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
      tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
      abs(sum_catch - mean_catch) / sd_catch > tau
    } else {
      FALSE  # Not enough data to calculate outliers
    }
  })

# Filter the outliers
land_outliers <- land %>% 
  filter (outlier == TRUE)
write.csv(land_outliers, "landings_outliers.csv")

#Graphing of landings
#set up landings plot function
landingplot <- function(wavenum, species){
  p <- combined_catch %>% filter(COMMON==species) %>%
    filter(WAVE==wavenum) %>%
    ggplot(aes(x=YEAR, y=LANDING)) +
    geom_point() +
    geom_errorbar(aes(ymin = LOWER_LANDING, ymax = UPPER_LANDING)) +
    labs(title=paste0(species, " WAVE ", wavenum, " LANDINGS (A+B1)"), y="Landings (numbers)") +
    facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales="free_y", drop=FALSE) +
    theme_bw()+
    scale_x_discrete(guide=guide_axis(n.dodge = 2))
  print(p)
}

#Loops through each species and produces a graph for each wave
pdf("Landings.pdf")
for(s in species){
  for (w in waves) {
    landingplot(w,s)
  }
}
dev.off() #closes the PDF device

#####LIVE RELEASE (B2) COMPARISONS######
# Group by relevant columns and calculate sample size n, mean, and sd for 2017-2023
rel_stats <- catch_summed %>%
  filter(YEAR >= styr & YEAR <= endyr) %>%
  group_by(COMMON, WAVE) %>%
  summarise(
    mean_catch = mean(sum_rel, na.rm = TRUE),
    sd_catch = sd(sum_rel, na.rm = TRUE),
    n = n(),  # Calculate sample size for each group
    .groups = 'drop'
  )

rel_prelim <- catch_prelim %>%
  filter(ST==state) %>%
  group_by(COMMON, WAVE) %>%
  summarise(
    sum_catch = sum(ESTREL, na.rm = TRUE), #sums each species' landings across modes for each wave
    .groups = 'drop'
  )

# Join 2024 data with calculated harvest_stats (mean, sd, and n)
rel <- rel_prelim %>%
  left_join(rel_stats, by = c("COMMON", "WAVE"))

#Had an issue in the next section because of some tot_catch estimates in 2024 that aren't in the 2017-2023 summaries
#Removed these first and put the ones being removed into a unique file so they can still be looked at
rel_notCommon <- rel[is.na(rel$n),]

rel <-rel[!is.na(rel$n),]

# Apply the Thompson Tau calculation
rel <- rel %>%
  rowwise() %>%
  
  mutate(outlier = {
    # Calculate tau critical value
    if (n > 2) {
      t_critical <- qt(1 - 0.05 / (2 * n), df = n - 2)
      tau <- (t_critical * (n - 1)) / (sqrt(n) * sqrt(n - 2 + t_critical^2))
      abs(sum_catch - mean_catch) / sd_catch > tau
    } else {
      FALSE  # Not enough data to calculate outliers
    }
  })

# Filter the outliers
rel_outliers <- rel %>% 
  filter (outlier == TRUE)
write.csv(rel_outliers, "release_outliers.csv")

#Graphing of releases
#set up release plot function
relplot <- function(wavenum, species){
  p <- combined_catch %>% filter(COMMON==species) %>%
    filter(WAVE==wavenum) %>%
    ggplot(aes(x=YEAR, y=ESTREL)) +
    geom_point() +
    geom_errorbar(aes(ymin = LOWER_ESTREL, ymax = UPPER_ESTREL)) +
    labs(title=paste0(species, " WAVE ", wavenum, " Live Releases (B2)"), y="Live Releases (numbers)") +
    facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales="free_y") +
    theme_bw()+
    scale_x_discrete(guide=guide_axis(n.dodge = 2))
  print(p)
}

#Loops through each species and produces a graph for each wave
pdf("Releases.pdf")
for(s in species){
  for (w in waves) {
    relplot(w,s)
  }
}
dev.off() #closes the PDF device

#####Effort section
#combined_effort = read.csv("combined_effort_2017_2024_level2_filter.csv", header = TRUE)

#need combined file for graphing later in code
combined_effort <- rbind(effort, effort_prelim)
#combined_effort <- combined_effort[combined_effort$ST==state,]

#Calculating effort outliers at the wave, mode, and area level

# Group by relevant columns and calculate sample size n, mean, and sd for 2017-2023
effort_stats <- effort %>%
  filter(YEAR >= styr & YEAR <= endyr) %>%
  group_by(WAVE, MODE_FX_F, AREA_X_F) %>%
  summarise(
    mean_trips = mean(ESTRIPS, na.rm = TRUE),
    sd_trips = sd(ESTRIPS, na.rm = TRUE),
    n = n(),  # Calculate sample size for each group
    .groups = 'drop'
  )

# Join 2024 data with calculated harvest_stats (mean, sd, and n)
trips <- effort_prelim %>%
  filter(ST == state) %>%
  left_join(effort_stats, by = c("WAVE","MODE_FX_F","AREA_X_F"))

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
      FALSE  # Not enough data to calculate outliers
    }
  })

effort_outliers <- trips %>%
  filter (outlier == TRUE)
write.csv(effort_outliers, "effort_outliers.csv")

#Graphing of effort
#set up effort plot function
effplot <- function(wavenum){
  p <- combined_effort %>% 
    filter(WAVE==wavenum) %>%
    ggplot(aes(x=YEAR, y=ESTRIPS)) +
    geom_point() +
    geom_errorbar(aes(ymin = LOWER_ESTRIPS, ymax = UPPER_ESTRIPS)) +
    labs(title=paste0("WAVE ", wavenum, " Estimated Angler Trips"), y="Est. Angler Trips (numbers)") +
    facet_grid(vars(MODE_FX_F), vars(AREA_X_F), scales="free_y") +
    theme_bw()+
    scale_x_discrete(guide=guide_axis(n.dodge = 2))
  print(p)
}

#Loops through each species and produces a graph for each wave
pdf("EstTrips.pdf")
for(w in waves){
  effplot(w)
}
dev.off() #closes the PDF device
}