library(rsdmx)
library(tidyverse)

time

# First, let's check available datasets from OECD
oecd_url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/DAC..1000.100._T._T.D.Q._T..?startPeriod=2016"
try({
  datastructures <- readSDMX(oecd_url, isURL = TRUE)
  datastructures_list <- as.data.frame(datastructures)
  head(datastructures_list)
})
oecd_df <- as.data.frame(sdmx)