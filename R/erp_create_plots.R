# Clean up
# TODO: Delete the rm line
rm(list = ls())
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
# DIFFERENCES TO BASELINE CORRELATION MATRICES: Computation
#-------------------------------------------------------------------------------

# Baseline Differences Unsmoothed CATEGORY to INTERNAL
avg_cormat_us_cat_int <- create_avg_cormats(baseline_src_mat = NULL
                                            , cormats = erp_cov_corr_mats_session$erp_cormats_us_cat
                                            , avg_n = 500)

# Baseline Differences Unsmoothed CATEGORY to ALL
avg_cormat_us_cat_all <- create_avg_cormats(baseline_src_mat = erp_cov_corr_mats_session$erp_cormats_us_all
                                            , cormats = erp_cov_corr_mats_session$erp_cormats_us_cat
                                            , avg_n = 500)

# Baseline Differences Smoothed CATEGORY to INTERNAL
avg_cormat_sm_cat_int <- create_avg_cormats(baseline_src_mat = NULL
                                            , cormats = erp_cov_corr_mats_session$erp_cormats_sm_cat
                                            , avg_n = 500)

# Baseline Differences Smoothed CATEGORY to ALL
avg_cormat_sm_cat_all <- create_avg_cormats(baseline_src_mat = erp_cov_corr_mats_session$erp_cormats_sm_all
                                            , cormats = erp_cov_corr_mats_session$erp_cormats_sm_cat
                                            , avg_n = 500)

#-------------------------------------------------------------------------------
# CREATE PLOTS
#-------------------------------------------------------------------------------

ls(erp_cov_corr_mats_session)
# Correlation Matrices UNSMOOTHED cat
erp_cormats_us_ts_cat_plt <- purrr::map2(.x = erp_cov_corr_mats_session$erp_cormats_us_cat[ALL_ERP_TIME_INT]
                                         , .y = ALL_ERP_TIME_INT
                                         , ~ create_corr_ts(inp_cormat = .x
                                                            , cat_idx = CORE_CAT
                                                            , time_ms_idx = .y
                                                            , abs = TRUE
                                                            , fisher = TRUE
                                                            , plot_col_range = c(-0.2, 2.5)
                                                            , smoothed = FALSE
                                                            , diff = FALSE))

# Correlation Matrices SMOOTHED cat
erp_cormats_sm_ts_cat_plt <- purrr::map2(.x = erp_cov_corr_mats_session$erp_cormats_sm_cat[ALL_ERP_TIME_INT]
                                         , .y = ALL_ERP_TIME_INT
                                         , ~ create_corr_ts(inp_cormat = .x
                                                            , cat_idx = CORE_CAT
                                                            , time_ms_idx = .y
                                                            , abs = TRUE
                                                            , fisher = TRUE
                                                            , plot_col_range = c(-0.2, 2.5)
                                                            , smoothed = TRUE
                                                            , diff = FALSE))

# Differences Correlation Matrices UNSMOOTHED cat INTERNAL
avg_cormat_us_cat_int_plt <- purrr::map2(.x = avg_cormat_us_cat_int[ALL_ERP_TIME_INT]
                                         , .y = ALL_ERP_TIME_INT
                                         , ~ create_corr_ts(inp_cormat = .x
                                                            , cat_idx = CORE_CAT
                                                            , time_ms_idx = .y
                                                            , abs = TRUE
                                                            , fisher = TRUE
                                                            , plot_col_range = c(-0.5, 0.5)
                                                            , smoothed = FALSE
                                                            , diff = TRUE))

# Differences Correlation Matrices SMOOTHED cat INTERNAL
avg_cormat_sm_cat_int_plt <- purrr::map2(.x = avg_cormat_sm_cat_int[ALL_ERP_TIME_INT]
                                         , .y = ALL_ERP_TIME_INT
                                         , ~ create_corr_ts(inp_cormat = .x
                                                            , cat_idx = CORE_CAT
                                                            , time_ms_idx = .y
                                                            , abs = TRUE
                                                            , fisher = TRUE
                                                            , plot_col_range = c(-0.5, 0.5)
                                                            , smoothed = TRUE
                                                            , diff = TRUE))

# Differences Correlation Matrices UNSMOOTHED cat ALL
avg_cormat_us_cat_all_plt <- purrr::map2(.x = avg_cormat_us_cat_all[ALL_ERP_TIME_INT]
                                         , .y = ALL_ERP_TIME_INT
                                         , ~ create_corr_ts(inp_cormat = .x
                                                            , cat_idx = CORE_CAT
                                                            , time_ms_idx = .y
                                                            , abs = TRUE
                                                            , fisher = TRUE
                                                            , plot_col_range = c(-0.5, 0.5)
                                                            , smoothed = FALSE
                                                            , diff = TRUE))

# Differences Correlation Matrices SMOOTHED cat ALL
avg_cormat_sm_cat_all_plt <- purrr::map2(.x = avg_cormat_sm_cat_all[ALL_ERP_TIME_INT]
                                         , .y = ALL_ERP_TIME_INT
                                         , ~ create_corr_ts(inp_cormat = .x
                                                            , cat_idx = CORE_CAT
                                                            , time_ms_idx = .y
                                                            , abs = TRUE
                                                            , fisher = TRUE
                                                            , plot_col_range = c(-0.5, 0.5)
                                                            , smoothed = FALSE
                                                            , diff = TRUE))

#-------------------------------------------------------------------------------
# SAVE GGPLOTS
#-------------------------------------------------------------------------------

# Correlation Matrices UNSMOOTHED cat
purrr::walk2(.x = ALL_ERP_TIME_INT, .y = erp_cormats_us_ts_cat_plt[ALL_ERP_TIME_INT]
             , ~ erp_save_plot(time_idx = .x
                               , n_patient_num = N_PATIENT_NUM
                               , batch_idx = BATCH_IDX
                               , cat_idx = CORE_CAT
                               , session_idx = SESSION_INDEX
                               , smoothed = FALSE
                               , out_dir = here::here("plots")
                               , diff_type = "non"
                               , plot_gg = .y
                               , thr_type = NULL))

# Correlation Matrices SMOOTHED cat
purrr::walk2(.x = ALL_ERP_TIME_INT, .y = erp_cormats_sm_ts_cat_plt[ALL_ERP_TIME_INT]
             , ~ erp_save_plot(time_idx = .x
                               , n_patient_num = N_PATIENT_NUM
                               , batch_idx = BATCH_IDX
                               , cat_idx = CORE_CAT
                               , session_idx = SESSION_INDEX
                               , smoothed = TRUE
                               , out_dir = here::here("plots")
                               , diff_type = "non"
                               , plot_gg = .y
                               , thr_type = NULL))

# Differences Correlation Matrices UNSMOOTHED cat INTERNAL
purrr::walk2(.x = ALL_ERP_TIME_INT, .y = avg_cormat_us_cat_int_plt[ALL_ERP_TIME_INT]
             , ~ erp_save_plot(time_idx = .x
                               , n_patient_num = N_PATIENT_NUM
                               , batch_idx = BATCH_IDX
                               , cat_idx = CORE_CAT
                               , session_idx = SESSION_INDEX
                               , smoothed = FALSE
                               , out_dir = here::here("plots")
                               , diff_type = "int"
                               , plot_gg = .y
                               , thr_type = NULL))

# Differences Correlation Matrices SMOOTHED cat INTERNAL
purrr::walk2(.x = ALL_ERP_TIME_INT, .y = avg_cormat_sm_cat_int_plt[ALL_ERP_TIME_INT]
             , ~ erp_save_plot(time_idx = .x
                               , n_patient_num = N_PATIENT_NUM
                               , batch_idx = BATCH_IDX
                               , cat_idx = CORE_CAT
                               , session_idx = SESSION_INDEX
                               , smoothed = TRUE
                               , out_dir = here::here("plots")
                               , diff_type = "int"
                               , plot_gg = .y
                               , thr_type = NULL))

# Differences Correlation Matrices UNSMOOTHED cat ALL
purrr::walk2(.x = ALL_ERP_TIME_INT, .y = avg_cormat_us_cat_all_plt[ALL_ERP_TIME_INT]
             , ~ erp_save_plot(time_idx = .x
                               , n_patient_num = N_PATIENT_NUM
                               , batch_idx = BATCH_IDX
                               , cat_idx = CORE_CAT
                               , session_idx = SESSION_INDEX
                               , smoothed = FALSE
                               , out_dir = here::here("plots")
                               , diff_type = "all"
                               , plot_gg = .y
                               , thr_type = NULL))

# Differences Correlation Matrices SMOOTHED cat ALL
purrr::walk2(.x = ALL_ERP_TIME_INT, .y = avg_cormat_sm_cat_all_plt[ALL_ERP_TIME_INT]
             , ~ erp_save_plot(time_idx = .x
                               , n_patient_num = N_PATIENT_NUM
                               , batch_idx = BATCH_IDX
                               , cat_idx = CORE_CAT
                               , session_idx = SESSION_INDEX
                               , smoothed = TRUE
                               , out_dir = here::here("plots")
                               , diff_type = "all"
                               , plot_gg = .y
                               , thr_type = NULL))