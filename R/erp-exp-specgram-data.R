# Clean up
# TODO: Delete the rm line
rm(list = ls())
cat("\014")
dev.off()

library(here)
library(tidyverse)
library(magrittr)
devtools::install_github("shamindras/neuroada")

#*******************************************************************************
# CORE VARIABLES AND OUTPUT CREATION
#*******************************************************************************

#-------------------------------------------------------------------------------
# Define USER VARIABLES
#-------------------------------------------------------------------------------

# User variables
N_DATA_TYPE <- "localizer_erp"
N_PATIENT_NUM <- 35
SESSION_INDEX <- 1
TASK <- "category"
FILTER <- "raw"
# TODO: This should be the max channel index
#       We want all channels less than or equal to this amount
CHANNEL_INDEX <- 115
CORE_CAT <- "Faces"
ALL_ERP_TIME_INT <- 1:1500
BIN_WIDTH <- 15
BATCH_IDX <- 6

# Output directory to unzip the tar file
out_dir_path <- stringr::str_c("P", N_PATIENT_NUM, "_specgram") %>%
                    here::here("data", N_DATA_TYPE, .)
out_dir_path

#-------------------------------------------------------------------------------
# All Channels: Read all ERP Channel Data with Labels
#-------------------------------------------------------------------------------

erp_cat_session1 <- neuroada::erp_create_df_labeled(n_data_type = "localizer_erp",
                                                    n_patient_num = N_PATIENT_NUM,
                                                    session_index = 1,
                                                    task = TASK,
                                                    filter = FILTER,
                                                    channel_index = CHANNEL_INDEX)

#-------------------------------------------------------------------------------
# Get the filtered Data for a single category
#-------------------------------------------------------------------------------

# Extract output to a specific category. This will produce a data frame
# of the (number of category trials)*(number of millseconds recorded)
get_erp_trials_cat <- erp_cat_session1[[1]] %>%
                        neuroada::filter_rows(df = .,
                                              cat_idx = CORE_CAT) %>%
                        dplyr::select(-(lab_category:task_type))


# Export the file to CSV
out_csv_path <- file.path(out_dir_path, "P35-chn-01-ses-01-faces-trials.csv")
readr::write_csv(x = get_erp_trials_cat,
                 path = out_csv_path)
