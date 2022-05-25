# main target script for calling all subsequent targets
library(targets)
library(tarchetypes)
library(tidyverse)

options(tidyverse.quiet = TRUE,
        clustermq.scheduler = "multicore")

source("0_config.R")
source("1_data.R") 
source("2_model.R") 

# complete list of targets
c(p0_targets_list, 
  p1_targets_list,
  p2_targets_list) 

