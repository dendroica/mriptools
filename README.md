# R package: mriptools
This is a draft library for MRIP data exploration, including outlier analysis. 

## Requirements  
The following software/R libraries are required to install the package...  

- R (version >= 4.4.3)
- devtools
- ggplot2

## How to install  
Run the following line in R: `devtools::install_github("dendroica/mriptools")`

## How to update the package  
Run the following line in R: `devtools::update_packages("mriptools")`

## Instructions  
Below is an example R script to run the `mrip()` function. Set the FIPS code for your state:  
GA=13, SC=45, NC=37, VA=51, MD=24, DE=10, NJ=34, NY=36, CT=9, RI=44, MA=25, NH=33, ME=23  
Set the `outdir` variable to the folder where output plots and CSV files will be generated. The `indir` argument is optional, and points to a location with your downloaded files. If this option is not set, the default behavior is to pull the data directly from [the data repository online] (https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads)

If you choose to use input files stored locally on your computer, be sure that there are not duplicate files for a given year in the `indir` folder (e.g. a preliminary and final file for a given year). The remaining variables are used in the graphing outputs later. You can change the species of interest for your state, the waves included, the areas, and the modes that you would like included on your graphs. You might want to watch how many species are included as it may take a little bit for the graphing function to loop through a ton of species.

```
library(mriptools)

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
  endyr = 2024,
  y_prelim = 2025,
  species = myspecies,
  waves = c(2, 3, 4, 5, 6),
  areas = c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)"),
  modes = c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE"),
  state = 24,
  outdir = "~/output/mrip_ex"
) # indir="~/data/MRIP",
```
