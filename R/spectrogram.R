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
        purrr::map(.x = ., ~ neuroada::erp_create_specgram_trial(inp_trial = .x))
dim(p[[1]]$S)
p[[1]]$S
x <- abs(p[[1]]$S)
dim(x)
plot(rowMeans(log(x^2)), type = 'l')
lines(idx, fit$fitted.values, col='red')
y = rowMeans(log(x^2))
idx = 1:64
fit = lm(y ~ I(1/idx))
curve(1/(1:64))
p2 <- p %>% purrr::map(.x = ., ~ neuroada::erp_plot_specgram_trial(inp_sgram = .x))
dev.off()

neuroada::erp_create_specgram_trial
