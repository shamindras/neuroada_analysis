# Clean up
# TODO: Delete the rm line
rm(list = ls())
cat("\014")
dev.off()

library(here)
library(tidyverse)
library(magrittr)
# library(signal)
library(pryr)
devtools::install_github("shamindras/neuroada")

#-------------------------------------------------------------------------------
# Define Custom Functions
#-------------------------------------------------------------------------------

#' Title
#'
#' @param inp_erp_filt_df_lab
#' @param time_int
#' @param cat_idx
#'
#' @return
#' @export
erp_create_channel_bind <- function(inp_erp_filt_df_lab, time_int = NULL) {

    erp_channel_bind <- base::list()

    if(is.null(time_int)){
        # TODO: remove the hardcoded 6 label cols here
        # Remove all columns that don't have an X\\d+ in them
        time_int <- 1:(ncol(inp_erp_filt_df_lab[[1]]) - 6)
    }

    for (i in time_int) {

        # Set up time based values
        time_val <- i
        print(time_val)
        col_val <- stringr::str_c("X", time_val, sep = "")
        print(col_val)

        # Create the correlation matrix
        # Create the labels for the channels
        channel_labs <- stringr::str_c("C",
                                       seq(from = 1,
                                           to = length(inp_erp_filt_df_lab)),
                                       sep = "")

        # Extract the X_i column
        corr_base <- purrr::map(.x = inp_erp_filt_df_lab
                                , ~ neuroada::extract_column(
                                    df = .x
                                    , colname = col_val
                                )) %>%
            purrr::map_dfc(., cbind) %>%
            magrittr::set_colnames(channel_labs)

        erp_channel_bind[[length(erp_channel_bind) + 1]] <- corr_base
    }
    base::return(erp_channel_bind)
}


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

#-------------------------------------------------------------------------------
# All Channels: Read all ERP Channel Data with Labels
#-------------------------------------------------------------------------------

erp_session <- neuroada::erp_create_df_labeled(n_data_type = "localizer_erp",
                                               n_patient_num = N_PATIENT_NUM,
                                               session_index = 1,
                                               task = TASK,
                                               filter = FILTER,
                                               channel_index = CHANNEL_INDEX)

#-------------------------------------------------------------------------------
# Filter to specific category
#-------------------------------------------------------------------------------

# CATEGORY : Filtered
# Extract output to a specific category. This will produce a data frame
# of the (number of category trials)*(number of millseconds recorded)
erp_filt_df_cat <- erp_session %>%
                        neuroada::erp_create_filter_df(inp_erp_df_lab = .,
                                             # cat_idx = core_cat)
                                             cat_idx = CORE_CAT)

length(erp_filt_df_cat)
dim(erp_filt_df_cat[[1]])

# ALL : Unfiltered
erp_filt_df_all   <- erp_session %>%
                        neuroada::erp_create_filter_df(inp_erp_df_lab = .,
                                                       cat_idx = NULL)

length(erp_filt_df_all)
dim(erp_filt_df_all[[1]])

#-------------------------------------------------------------------------------
#  Produce Millisecond level dataframes
#-------------------------------------------------------------------------------

erp_filt_df_cat_ms <- erp_create_channel_bind(erp_filt_df_cat,
                                              time_int = ALL_ERP_TIME_INT)

length(erp_filt_df_cat_ms)
dim(erp_filt_df_cat_ms[[1]])

erp_filt_df_all_ms <- erp_create_channel_bind(erp_filt_df_all,
                                              time_int = ALL_ERP_TIME_INT)

length(erp_filt_df_all_ms)
dim(erp_filt_df_all_ms[[1]])

# base::save(erp_filt_df_cat_ms, file="~/Desktop/erp_filt_df_cat_ms.RData")
# base::save(erp_filt_df_all_ms, file="~/Desktop/erp_filt_df_all_ms.RData")

#-------------------------------------------------------------------------------
# For pre-stim matrices - just do a rowbind to get 500*numtrials - the X matrix
#-------------------------------------------------------------------------------

erp_create_rbind <- function(inp_erp_channel_df, n = 500){

    rem_length <- length(inp_erp_channel_df) - n
    first_idx <- base::seq.int(from = 1, by = 1, length.out = n)
    last_idx <- base::seq.int(from = n + 1, by = 1, length.out = rem_length)
    rem_idx <- base::seq.int(from = 2, by = 1, length.out = rem_length)
    # print(rem_length)
    # print(first_idx)
    # print(last_idx)
    # print(rem_idx)

    erp_channel_firstn <- inp_erp_channel_df[first_idx] %>%
                            purrr::map_df(.x = ., .f = rbind)

    new_list <- list()
    new_list[[1]] <- erp_channel_firstn
    new_list[rem_idx] <- inp_erp_channel_df[last_idx]

    base::return(new_list)
}


aux_convert_df_matrix <- function(inp_df){
    out_mat <- inp_df %>%
                    base::as.matrix()
    base::dimnames(out_mat) <- NULL
    base::return(out_mat)
}


test1 <- erp_create_rbind(inp_erp_channel_df = erp_filt_df_cat_ms, n = 500)
test2 <- test1 %>%
            purrr::map(.x = ., ~ aux_convert_df_matrix(inp_df = .x))

length(test2)
test3 <- test2[1:2]
length(test3)

class(check_df)
check_df2 <- check_df %>%
                base::as.matrix()

dimnames(check_df2) <- NULL
dimnames(check_df2)

class(check_df2)

length(test1)
purrr::map(.x = test1, ~ dim(.x))
purrr::map(.x = test2, ~ dim(.x))
purrr::map(.x = test2, ~ class(.x))
dim(test1)
as.matrix(test1)
dim(test1)
typeof(test1)
dim(erp_filt_df_cat_ms[[1]])
a <- list(rep(1, 10), rep(2, 20), rep(3, 15), rep(4, 3), rep(5, 16))
a
b <- a[1:3]
b[4:5] <- a[4:5]
b
a
