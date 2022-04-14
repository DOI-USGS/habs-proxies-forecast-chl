# main target script for calling all subsequent targets
library(targets)
library(tarchetypes)
library(tidyverse)

options(tidyverse.quiet = TRUE,
        clustermq.scheduler = "multicore")

source("0_config.R")

# complete list of targets
c(p0_targets_list)

