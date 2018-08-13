# Clean up
# TODO: Delete the rm line
# rm(list = ls())
cat("\014")

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

# Thresholds for plotting
LOWER_ABS_THR <- -0.2
UPPER_ABS_THR <- 2.5
LOWER_ABS_DIFF_THR <- -0.5
UPPER_ABS_DIFF_THR <- 0.5

#-------------------------------------------------------------------------------
# All Channels: Read all ERP Channel Data with Labels
#-------------------------------------------------------------------------------

system.time(erp_cat_session1 <- neuroada::erp_create_df_labeled(n_data_type = "localizer_erp",
                                                                n_patient_num = N_PATIENT_NUM,
                                                                session_index = 1,
                                                                task = TASK,
                                                                filter = FILTER,
                                                                channel_index = CHANNEL_INDEX))

#-------------------------------------------------------------------------------
# Create the Covariance and the Correlation Matrices
#-------------------------------------------------------------------------------

# For a single session
system.time(erp_cov_corr_mats_session <-
                neuroada::erp_create_cat_all_mats(erp_df_labeled = erp_cat_session1,
                                                  core_cat = CORE_CAT,
                                                  all_erp_time_int = ALL_ERP_TIME_INT,
                                                  bin_width = BIN_WIDTH))

#-------------------------------------------------------------------------------
# SAVE CORRELATION MATRICES
#-------------------------------------------------------------------------------

base::save(list = erp_cov_corr_mats_session, file = "~/")
# length(erp_cov_corr_mats_session$erp_cormats_us_cat)
# dim(erp_cov_corr_mats_session$erp_cormats_us_cat[[1]])

save_mats <- function(imat, save_dir, save_prefix, save_id){
    save_path <- file.path(save_dir,
                           glue::glue("{save_prefix}-\\
                                      {stringr::str_pad(string = save_id,
                                      width = 2,
                                      pad = '0')}.mat"))
    R.matlab::writeMat(save_path, mat = imat)
}

# Baseline Differences Unsmoothed CATEGORY - non ABS, non-FISHER transformed

erp_cormats_us_cat_nonaft <- erp_cov_corr_mats_session$erp_covmats_us_cat %>%
    purrr::map(.x = ., ~ neuroada::erp_cov2corr(inp_covmat = .x,
                                      abs = FALSE,
                                      fisher = FALSE))

erp_cormats_us_cat_nonaft %>%
    purrr::iwalk(.x = ., ~save_mats(imat = .x,
                                    save_dir = here::here("R", "pisces", "data"),
                                    save_prefix = "erp-cormats-us-cat-nonaft",
                                    save_id = .y))


# Baseline Differences Unsmoothed CATEGORY
erp_cov_corr_mats_session$erp_cormats_us_cat %>%
    purrr::iwalk(.x = ., ~save_mats(imat = .x,
                                    save_dir = here::here("R", "pisces", "data"),
                                    save_prefix = "erp-cormats-us-cat",
                                    save_id = .y))

# Baseline Differences Smoothed CATEGORY
erp_cov_corr_mats_session$erp_cormats_sm_cat %>%
    purrr::iwalk(.x = ., ~save_mats(imat = .x,
                                    save_dir = here::here("R", "pisces", "data"),
                                    save_prefix = "erp-cormats-sm-cat",
                                    save_id = .y))

# Baseline Differences Smoothed ALL
erp_cov_corr_mats_session$erp_cormats_us_all %>%
    purrr::iwalk(.x = ., ~save_mats(imat = .x,
                                    save_dir = here::here("R", "pisces", "data"),
                                    save_prefix = "erp-cormats-us-all",
                                    save_id = .y))

# Baseline Differences Unsmoothed ALL
erp_cov_corr_mats_session$erp_cormats_sm_all %>%
    purrr::iwalk(.x = ., ~save_mats(imat = .x,
                                    save_dir = here::here("R", "pisces", "data"),
                                    save_prefix = "erp-cormats-sm-all",
                                    save_id = .y))


#-------------------------------------------------------------------------------
# DIFFERENCES TO BASELINE CORRELATION MATRICES: Computation
#-------------------------------------------------------------------------------

# # Baseline Differences Unsmoothed CATEGORY to INTERNAL
# avg_cormat_us_cat_int <- create_avg_cormats(baseline_src_mat = NULL
#                                             , cormats = erp_cov_corr_mats_session$erp_cormats_us_cat
#                                             , avg_n = 500)
#
# # Baseline Differences Unsmoothed CATEGORY to ALL
# avg_cormat_us_cat_all <- create_avg_cormats(baseline_src_mat = erp_cov_corr_mats_session$erp_cormats_us_all
#                                             , cormats = erp_cov_corr_mats_session$erp_cormats_us_cat
#                                             , avg_n = 500)
#
# # Baseline Differences Smoothed CATEGORY to INTERNAL
# avg_cormat_sm_cat_int <- create_avg_cormats(baseline_src_mat = NULL
#                                             , cormats = erp_cov_corr_mats_session$erp_cormats_sm_cat
#                                             , avg_n = 500)
#
# # Baseline Differences Smoothed CATEGORY to ALL
# avg_cormat_sm_cat_all <- create_avg_cormats(baseline_src_mat = erp_cov_corr_mats_session$erp_cormats_sm_all
#                                             , cormats = erp_cov_corr_mats_session$erp_cormats_sm_cat
#                                             , avg_n = 500)


