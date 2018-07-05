# Clean up
# TODO: Delete the rm line
# rm(list = ls())
cat("\014")
dev.off()

# Set seed for reproducibility
set.seed(1265)

library(here)
library(tidyverse)
library(magrittr)
# library(signal)
library(pryr)
devtools::install_github("shamindras/neuroada")

# Load the differential networks paper code
dyn.load(here::here("R", "dpm.so"))
source(here::here("R", "dpm.R"))
library(MASS)

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

# Load in the data manually i.e. if comp crashes, so we don't need
# to recreate it again. Takes 1hr!
base::load(file="~/Desktop/erp_filt_df_cat_ms.RData")
base::load(file="~/Desktop/erp_filt_df_all_ms.RData")

erp_create_rbind <- function(inp_erp_channel_df, n = 500){

    rem_length <- length(inp_erp_channel_df) - n
    first_idx <- base::seq.int(from = 1, by = 1, length.out = n)
    last_idx <- base::seq.int(from = n + 1, by = 1, length.out = rem_length)
    rem_idx <- base::seq.int(from = 2, by = 1, length.out = rem_length)

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

aux_select_colnums <- function(inp_mat, col_idx){
    out_mat <- inp_mat[, col_idx]
    base::return(out_mat)
}

aux_fit_dpm <- function(x_mat, y_mat, nlambda = 2, tuning="aic", folds = 3,
                        y_idx) {
    start_msg <-  stringr::str_c("Fitting poststim matrix", y_idx,
                                 "...", sep = " ")
    if (tuning == "aic") {
        print(start_msg)
        fit <- dpm(y_mat, x_mat, nlambda = nlambda, tuning = tuning)
    } else {
        print(start_msg)
        fit <- dpm(y_mat, x_mat, nlambda = nlambda, tuning = tuning, folds = folds)
    }
    base::return(fit)
}

run_fit_dpm <- function(pre_stim_mat, post_stim_mats, post_stim_idx,
                        nlambda = 2, tuning="aic", folds = 3){
    num_poststim <- base::length(post_stim_mats)
    seq_num_poststim <- base::seq.int(from = 1, to = num_poststim, by = 1)
    all_fits <- post_stim_mats %>%
        purrr::map2(.x = ., .y = seq_num_poststim,
                    ~ aux_fit_dpm(x_mat = pre_stim_mat,
                                y_mat = .x,
                                nlambda = nlambda,
                                tuning = tuning,
                                folds = folds,
                                y_idx = .y))
    # end_msg <- stringr::str_c("Fitted poststim matrix", post_stim_idx, "...", sep = " ")
    # print(end_msg)
    base::return(all_fits)
}

ext_list_element <- function(l, elem_idx = 5){
    base::return(l$dpm[[5]])
}

create_tidy_cormat <- function(cormat){
    cor_tidy <- as.data.frame(cormat) %>%
        dplyr::mutate(channel_y = factor(row.names(.)
                                         , levels=row.names(.))) %>%
        tidyr::gather(key = channel_x
                      , value = value, -channel_y
                      , na.rm = TRUE, factor_key = TRUE)
    base::return(cor_tidy)
}


create_corr_ts <- function(inp_cormat, cat_idx, time_ms_idx
                           , plot_col_range = c(-0.0001, 0.0001)){
    print(stringr::str_c("Creating plot for ms:", time_ms_idx, "...",
                         sep = " "))
    plot_title <- stringr::str_c("Difference to base precision matrices ms:",
                                 time_ms_idx, sep = " ")
    p1 <- ggplot2::ggplot(data = inp_cormat
                          , aes(channel_x, channel_y, fill = value)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(low = "blue", high = "red"
                                      , mid = "white",
                                      midpoint = 0, limit = plot_col_range
                                      , space = "Lab",
                                      name = "Legend") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_text(angle = 90
                                                  , vjust = 1,
                                                  size = 6
                                                  , hjust = 1)
                       , axis.text.y = element_text(size = 6)) +
        ggplot2::labs(title = plot_title
                      , subtitle = "Across Neural Channels"
                      , x = "Neural Channel"
                      , y = "Neural Channel") +
        ggplot2::coord_fixed()

    base::return(p1)
}

# Faces
base_dndf <- erp_create_rbind(inp_erp_channel_df = erp_filt_df_cat_ms, n = 500)
base_dnmat <- base_dndf %>%
                purrr::map(.x = ., ~ aux_convert_df_matrix(inp_df = .x))
# Filter out the first 25 cols for each matrix
base_dnmat_cols <- base_dnmat %>%
                    purrr::map(.x = ., ~ aux_select_colnums(inp_mat = .x,
                                                            col_idx = 1:25))

pre_stim_mat <- base_dnmat_cols[[1]]
post_stim_mats <- base_dnmat_cols[2:1001]
post_stim_idx <- base::seq.int(from = 1, to = base::length(post_stim_mats),
                               by = 1)
all_fits <- run_fit_dpm(pre_stim_mat = pre_stim_mat,
                        post_stim_mats = post_stim_mats,
                        post_stim_idx = post_stim_idx,
                        nlambda = 10, tuning="aic", folds = 3)

all_fits_ext <- all_fits %>%
                    purrr::map(.x = ., ~ ext_list_element(l = .x, elem_idx = 5))

tidy_cormats <- all_fits_ext %>%
                    purrr::map(.x = , ~create_tidy_cormat(cormat = .x))

all_fits_ext_plots <- tidy_cormats %>%
                        purrr::map2(.x = ., .y = base::seq.int(from = 1,
                                                               to = base::length(.),
                                                               by = 1),
                                    ~ create_corr_ts(inp_cormat = .x,
                                                   cat_idx = CORE_CAT,
                                                   time_ms_idx = 500 + .y,
               plot_col_range = c(-0.0009, 0.0009)))

dev.off()
all_fits_ext_plots[[170]]
all_fits_ext_plots[[171]]
all_fits_ext_plots[[172]]
all_fits_ext_plots[[173]]
all_fits_ext_plots[[174]]
all_fits_ext_plots[[175]]
all_fits_ext_plots[[176]]
all_fits_ext_plots[[177]]
all_fits_ext_plots[[178]]
all_fits_ext_plots[[180]]
all_fits_ext_plots[[181]]

max(all_fits_ext[[180]])
min(all_fits_ext[[180]])

max(all_fits_ext[[1]])
min(all_fits_ext[[1]])


max(all_fits_ext[[1]])
min(all_fits_ext[[1]])

all_fits_ext[[1]]

# empirical precision matrices and differences. Make sure that they agree
# when lambda approx 0 (exactly zero may break solver), try other lambdas. Look at how they generate their
# sequence of lambdas
# Try downsampling as well

all_fits_ext[[185]] - all_fits_ext[[1]]
# all_fits_ext[[185]] - all_fits_ext[[185]]

#-------------------------------------------------------------------------------
# For pre-stim matrices - downsample the 500 pre-stim matrices data to 83 trials
#-------------------------------------------------------------------------------

# Start with the same pre-stim matrices as before
pre_stim_mat <- base_dnmat_cols[[1]]
post_stim_mats <- base_dnmat_cols[2:1001]
post_stim_idx <- base::seq.int(from = 1, to = base::length(post_stim_mats),
                               by = 1)

# Let's downsample the pre-stim matrix to match
pre_stim_mat_ds <-
all_fits <- run_fit_dpm(pre_stim_mat = pre_stim_mat,
                        post_stim_mats = post_stim_mats,
                        post_stim_idx = post_stim_idx,
                        nlambda = 10, tuning="aic", folds = 3)

all_fits_ext <- all_fits %>%
    purrr::map(.x = ., ~ ext_list_element(l = .x, elem_idx = 5))

tidy_cormats <- all_fits_ext %>%
    purrr::map(.x = , ~create_tidy_cormat(cormat = .x))

all_fits_ext_plots <- tidy_cormats %>%
    purrr::map2(.x = ., .y = base::seq.int(from = 1,
                                           to = base::length(.),
                                           by = 1),
                ~ create_corr_ts(inp_cormat = .x,
                                 cat_idx = CORE_CAT,
                                 time_ms_idx = 500 + .y,
                                 plot_col_range = c(-0.0009, 0.0009)))

dev.off()
all_fits_ext_plots[[170]]
all_fits_ext_plots[[171]]
all_fits_ext_plots[[172]]
all_fits_ext_plots[[173]]
all_fits_ext_plots[[174]]
all_fits_ext_plots[[175]]
all_fits_ext_plots[[176]]
all_fits_ext_plots[[177]]
all_fits_ext_plots[[178]]
all_fits_ext_plots[[180]]
all_fits_ext_plots[[181]]


