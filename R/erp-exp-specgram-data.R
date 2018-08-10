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
N_PATIENT_NUM <- 36
SESSION_INDEX <- 2
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
OUT_DIR_PATH <- stringr::str_c("P", N_PATIENT_NUM, "_specgram") %>%
                    here::here("data", N_DATA_TYPE, .,
                               TASK,
                               stringr::str_c("session_0", SESSION_INDEX),
                               stringr::str_to_lower(CORE_CAT))
OUT_DIR_PATH

#-------------------------------------------------------------------------------
# All Channels: Read all ERP Channel Data with Labels
#-------------------------------------------------------------------------------

erp_cat_session <- neuroada::erp_create_df_labeled(n_data_type = "localizer_erp",
                                                   n_patient_num = N_PATIENT_NUM,
                                                   session_index = SESSION_INDEX,
                                                   task = TASK,
                                                   filter = FILTER,
                                                   channel_index = CHANNEL_INDEX)

#-------------------------------------------------------------------------------
# Get the filtered Data for a single category
#-------------------------------------------------------------------------------

#' Creates the CSV name for the exported specgram data
#'
#' @param patient_num
#' @param session_idx
#' @param channel_idx
#' @param cat_name
#'
#' @return
#' @export
create_csv_name <- function(patient_num, session_idx, channel_idx, cat_name){

    patient_ref <- stringr::str_c("P", patient_num)
    out_csv_name <- patient_ref %>%
                        stringr::str_c(., "ses", stringr::str_pad(session_idx, 2,pad = "0"),
                                       "chn", stringr::str_pad(channel_idx, 2,pad = "0"),
                                       cat_name, sep = "-") %>%
                        stringr::str_c(., ".csv", sep = "")
    base::return(out_csv_name)
}

#' Creates the full CSV path for the exported specgram data
#'
#' @param out_dir_path
#' @param patient_num
#' @param session_idx
#' @param channel_idx
#' @param cat_name
#'
#' @return
#' @export
create_csv_path <- function(out_dir_path, patient_num, session_idx, channel_idx,
                            cat_name){
    out_csv_path <- create_csv_name(patient_num = patient_num, session_idx = session_idx,
                    channel_idx = channel_idx, cat_name = cat_name) %>%
        base::file.path(out_dir_path, .)
    base::return(out_csv_path)
}

#' Filter the spectrogram data to the relevant category for the data from
#' the specific channel for a given patient
#'
#' @param erp_cat_session_channel
#' @param channel_idx
#' @param cat_idx
#'
#' @return
#' @export
erp_specgram_filter_cat <- function(erp_cat_session_channel,
                                    channel_idx, cat_idx){
    get_erp_trials_cat <- erp_cat_session_channel %>%
                            neuroada::filter_rows(df = ., cat_idx = cat_idx) %>%
                            dplyr::select(-(lab_category:task_type))
    base::return(get_erp_trials_cat)
}

#' Exports the specgram CSV file for a given channel for a given patient
#'
#' @param erp_channel_df
#' @param out_dir_path
#' @param patient_num
#' @param session_idx
#' @param channel_idx
#' @param cat_name
#'
#' @return
#' @export
erp_specgram_write_csv <- function(erp_channel_df, out_dir_path, patient_num,
                                   session_idx, channel_idx, cat_name){

    out_csv_path <- create_csv_path(out_dir_path = out_dir_path,
                                    patient_num = patient_num,
                                    session_idx = session_idx,
                                    channel_idx = channel_idx,
                                    cat_name = cat_name)

    readr::write_csv(x = erp_channel_df, path = out_csv_path)

}

# Extract output to a specific category. This will produce a data frame
# of the (number of category trials)*(number of millseconds recorded)
get_erp_trials_cat <- erp_cat_session %>%
                        purrr::map2(.x = .,
                                    .y = base::seq.int(from = 1,
                                                       to = base::length(.),
                                                       by = 1),
                                    ~erp_specgram_filter_cat(
                                        erp_cat_session_channel = .x,
                                        channel_idx = .y,
                                        cat_idx = CORE_CAT))

# Export the file to CSV
get_erp_trials_cat %>% purrr::walk2(.x = .,
                                   .y = base::seq.int(from = 1,
                                   to = base::length(.),
                                   by = 1),
                ~erp_specgram_write_csv(erp_channel_df = .x,
                                        out_dir_path = OUT_DIR_PATH,
                                        patient_num = N_PATIENT_NUM,
                                        session_idx = SESSION_INDEX,
                                        channel_idx = .y,
                                        cat_name = CORE_CAT))