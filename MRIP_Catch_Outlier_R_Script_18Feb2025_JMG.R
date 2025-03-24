#MRIP outlier code written by Rachel Sysak (NY DEC), Angela Giuliano (MD DNR), and Dawn Franco (GA DNR)
#Please reach out to them if there are any questions

#First you'll need to download the Catch Estimate by Wave csv files from the MRIP website
#They can be found here: https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads/
#You will need to download the catch and effort files for 2017-2023 as well as the 2024 preliminary file (updated following every wave)

####You should only need to change the variables in this section at the beginning to get the code to run###

#Set this to the folder where you would like the results of the outlier code to be output
#source("~/mrip_eda.R")
setwd("~/out")

#load in packages needed for analysis (you may need to install these in R if you don't have them already)
#set the FIPS code for your state
#GA=13, SC=45, NC=37, VA=51, MD=24, DE=10, NJ=34, NY=36, CT=9, RI=44, MA=25, NH=33, ME=23
state <- 24
#Create combined dataset for complete years (i.e. not prelim dataset for 2024)
styr <- 2017 #first year of your "historical" data files
endyr <- 2023 #last year of your "historical" data files, should be the first year before 

#set the year
y_prelim <- 2024

#These variables are used in the graphing outputs later. You can change the species of interest for your state, the waves included,
#the areas, and the modes that you would like included on your graphs 
#You might want to watch how many species are included as it may take a little bit for the graphing function to loop through a ton of species
species <- c("ATLANTIC CROAKER", "BLACK DRUM", "BLACK SEA BASS", "BLUEFISH", "COBIA", "DOLPHIN", "RED DRUM", "STRIPED BASS", "SUMMER FLOUNDER", "TAUTOG")
waves <- c(2,3,4,5,6)
areas <- c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)")
modes <- c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE")

mrip(styr, endyr, y_prelim, species, waves, areas, modes, state)

#TO DO: IMPORT EXTERNAL FUNCTIONS BETTER WITH ::
#ADD Imports: to DESCRIPTION accordingly