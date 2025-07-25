# devtools::install_github("dendroica/mriptools")
# devtools::update_packages("mriptools")
library(mriptools)
# set the FIPS code for your state
# GA=13, SC=45, NC=37, VA=51, MD=24, DE=10, NJ=34, NY=36, CT=9, RI=44, MA=25, NH=33, ME=23

# These variables are used in the graphing outputs later. You can change the species of interest for your state, the waves included,
# the areas, and the modes that you would like included on your graphs
# You might want to watch how many species are included as it may take a little bit for the graphing function to loop through a ton of species

myspecies <- c(
  "ATLANTIC CROAKER",
  "BLACK DRUM",
  "BLACK SEA BASS",
  "BLUEFISH",
  "COBIA",
  "DOLPHIN",
  "RED DRUM",
  "STRIPED BASS",
  "SUMMER FLOUNDER",
  "TAUTOG"
)

mrip(
  styr = 2017,
  endyr = 2023,
  y_prelim = 2024,
  species = myspecies,
  waves = c(2, 3, 4, 5, 6),
  areas = c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)"),
  modes = c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE"),
  state = 24,
  outdir = "~/output/mrip_ex",
  indir="~/data/mrip_old") # indir="~/data/mrip_old",
