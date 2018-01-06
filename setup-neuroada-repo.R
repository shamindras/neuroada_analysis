#-------------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------------

# install.packages("here")
library(here)

# Load the utilities functions
source(file = file.path(here::here(), "setup-utils.R"))

# Define packages to be installed and loaded
my_pckgs <- c("tidyverse", "usethis", "magrittr", "lintr", "devtools")

# Install and load the required packages
ipak(pkg = my_pckgs)

# Create as a project - ONCE OFF, comment afterwards
usethis::create_project()

