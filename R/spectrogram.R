# Clean up
# TODO: Delete the rm line
rm(list = ls())
cat("\014")

library(here)
library(tidyverse)
library(magrittr)
library(signal)
library(pryr)
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
# Core SPECTROGRAM functions
#-------------------------------------------------------------------------------

#' Creates a spectrogram from the univariate time series x
#' This function is created using code from Peter Elliot (CMU Statistics)
#' @param inp_trial
#' @param is the sampling rate (samples per second)
#' @param n is the number of time points to use per Fourier transform
#' @param overlap overlap controls the jump from bin to bin the time resolution
#' of the spectrogram is (n - overlap)
#' @return s is a list with three values
#'             s$t is a vector of times associated with the beginning of each bin
#'             s$f is a vector of frequencies that are measured for each bin.
#'                 s$f spans 0 to Fs/2 and has length n/2
#'             s$S is a matrix of intensities - Frequency x Time
#' @export
erp_create_specgram_trial <- function(inp_trial, Fs = 1000,
                                      n = 128, overlap = 96){
    sgram <- signal::specgram(x = inp_trial, Fs = Fs,
                              n = n, overlap = overlap)
    base::return(sgram)
}


#' Extracts the matrix of complex valued spectrogram entries
#'
#' @param inp_sgram The input spectrogram object as created from the
#' \code{signal::specgram} function
#'
#' @return a matrix of complex value spectrogram entries from the input
#' spectrogram object
#' @export
extract_specgram_mat <- function(inp_sgram){
    base::return(inp_sgram$S)
}


#' Create and store the spectrogram plot using a customized `copper` color
#' palette
#' This function is created using code from Peter Elliot (CMU Statistics)
#'
#' @param inp_sgram The input spectrogram object as created from the
#' \code{signal::specgram} function
#'
#' @return
#' @export
erp_plot_specgram_trial <- function(inp_sgram){

    # custom color scheme for displaying the spectrogram
    # low intensity --> black
    # high intensity --> goldenrod
    # bias controls spacing for high intensities (higher bias, more spacing)
    # copper is now a function that takes a number and returns a
    # vector of colors following the prescribed gradient
    copper <- grDevices::colorRampPalette(c("black", "navy blue", "goldenrod"),
                                          bias=2)

    # plots the spectrogram with restricted frequencies using the
    # specified colors
    plt_pryr %<a-% {
        graphics::plot(inp_sgram, col = copper(512), ylab = "Frequency")
        title("ERP Spectrogram")
        # adds a line marking stimulus onset (t = 500ms)
        abline(v=0.5, col="yellow", lwd=2)
    }

    base::return(plt_pryr)
}

#-------------------------------------------------------------------------------
# Run the Spectrogram Code for a single trial
#-------------------------------------------------------------------------------

# Extract output to a specific category. This will produce a data frame
# of the (number of category trials)*(number of millseconds recorded)
get_erp_trials_cat <- erp_cat_session1[[1]] %>%
                        neuroada::filter_rows(df = .,
                                              cat_idx = CORE_CAT) %>%
                        dplyr::select(-(lab_category:task_type))

# We can extract each trial recording into a separate vector and load
# all of them into a single list
get_erp_trials_catv <- get_erp_trials_cat %>%
                            purrr::pmap(.l = ., c, use.names = FALSE)

test_trials <- get_erp_trials_catv[1:5]
test_trials

# Create the spectrogram for each trial
p <- test_trials %>%
        purrr::map(.x = ., ~ erp_create_specgram_trial(inp_trial = .x))

p2 <- p %>% purrr::map(.x = ., ~ erp_plot_specgram_trial(inp_sgram = .x))
dev.off()

