
library(targets)
library(tarchetypes)

# Stop annoying messaging when it downloads each file
options(googledrive_quiet = TRUE,
        clustermq.scheduler = "multiprocess") 

tar_option_set(packages = c(
  'dataRetrieval',
  'googledrive',
  'ncdf4',
  'nhdplusTools',
  'prism',
  'raster',
  'readxl',
  'sf',
  'terra',
  'tidyverse',
  'yaml'
), format='qs', deployment='main')

source('1_download.R')
source('2_process.R')
# source('3_model.R') # Leaving 3 as a placeholder for future work
source('4_visualize.R')

# Define what each value in the rasters mean
bloom_plume_class_xwalk <- tibble::tibble(
  val = 0:5,
  nm = c(
    "outsideAOI",
    "cloud",
    "openWater",
    "lightNearShoreSediment",
    "darkNearShoreSediment",
    "offShoreSediment"
  ),
  color = c("white", "grey", "#9fc5e8", "#edbd95", "#41240c", "#db7b2b")
)

# All outputs
c(p1_download, p2_process,
  p4_visualize)
