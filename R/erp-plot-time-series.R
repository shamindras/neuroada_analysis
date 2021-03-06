# Clean up
# TODO: Delete the rm line
rm(list = ls())
cat("\014")
dev.off()

library(here)
library(tidyverse)
library(magrittr)
library(skimr)
library(plotly)
library(glue)
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

add_trial_idx <- function(df){
    out <- df %>%
        dplyr::mutate(trial_idx = dplyr::row_number()) %>%
        dplyr::select(lab_category, lab_cat_timestamps, session_idx,
                      channel_idx, filt_type, task_type,
                      trial_idx, dplyr::everything())
    base::return(out)
}

erp_cat_session1 <-
    neuroada::erp_create_df_labeled(n_data_type =
                                        "localizer_erp",
                                    n_patient_num = N_PATIENT_NUM,
                                    session_index = 1,
                                    task = TASK,
                                    filter = FILTER,
                                    channel_index = CHANNEL_INDEX) %>%
    purrr::map(.x = ., ~add_trial_idx(df = .x)) # Append the trial_idx

dim(erp_cat_session1[[1]])
dplyr::glimpse(erp_cat_session1[[1]][, 1:10])
colnames(erp_cat_session1[[1]]) %>% head() %>% cat()

# Append the trial_idx
# erp_cat_session2 <- erp_cat_session1 %>%
#                         purrr::map(.x = ., ~add_trial_idx(df = .x))
# dim(erp_cat_session2[[1]])
# dplyr::glimpse(erp_cat_session2[[1]][, 1:10])
# colnames(erp_cat_session2[[1]]) %>% head(x = ., 7) %>% cat()
# max(erp_cat_session2[[1]]$trial_idx)

# Create a tidy version of the dataset
erp_labeled_comb <- erp_cat_session1 %>%
    purrr::map_df(rbind) %>% # row bind the channel dataframes together
    dplyr::mutate(channel_idx = as.factor(channel_idx),
                  session_idx = as.factor(session_idx),
                  lab_category = as.factor(lab_category),
                  trial_idx = as.factor(trial_idx))
dim(erp_labeled_comb)
dplyr::glimpse(erp_labeled_comb[, 1:10])
head(erp_labeled_comb[, 1:10]) %>% View()

# Create a massive transpose
erp_labeled_comb_exp <- erp_labeled_comb %>%
    tidyr::gather(ms, erp_voltage, X1:X1500) %>%
    dplyr::mutate(ms = as.integer(stringr::str_replace(string = ms
                                                       , pattern = "X"
                                                       , replacement = "")))
dim(erp_labeled_comb_exp)
dplyr::glimpse(head(erp_labeled_comb_exp))
View(head(erp_labeled_comb_exp, 1510))

p1 <- erp_labeled_comb_exp %>%
    dplyr::filter(lab_category == CORE_CAT) %>%
    dplyr::group_by(channel_idx, ms) %>%
    dplyr::summarise(erp_volt = mean(erp_voltage)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(data = ., mapping = aes(x = ms, y = erp_volt
                                            , color = channel_idx))
p2 <- p1 + ggplot2::geom_line() +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(title = glue::glue("{CORE_CAT} - Patient {N_PATIENT_NUM}, Session {SESSION_INDEX}"))
p2
plotly::ggplotly(p2)

head(erp_labeled_comb_exp$trial_idx)

test3 <- erp_labeled_comb_exp %>%
    dplyr::filter(lab_category == CORE_CAT,
                  channel_idx == 25,
                  as.integer(trial_idx) <= 40)
dim(test3)
View(test3)

CHANNEL <- 26
p3 <- erp_labeled_comb_exp %>%
    dplyr::filter(lab_category == CORE_CAT, channel_idx == CHANNEL, as.integer(trial_idx) <= 100) %>%
    dplyr::group_by(trial_idx, ms) %>%
    dplyr::summarise(erp_volt = mean(erp_voltage)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(data = ., mapping = aes(x = ms, y = erp_volt
                                            , color = trial_idx))

p3
p4 <- p3 + ggplot2::geom_line() +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(title = glue::glue("{CORE_CAT} - Patient {N_PATIENT_NUM}, Session {SESSION_INDEX}, Channel {CHANNEL}"))
plotly::ggplotly(p4)

typeof(erp_labeled_comb_exp$trial_idx)
#-------------------------------------------------------------------------------
# Compare to CSV readin
#-------------------------------------------------------------------------------

dim(erp_cat_session1[[1]])
dplyr::glimpse(erp_cat_session1[[1]][, 1:10])
colnames(erp_cat_session1[[1]]) %>% head() %>% cat()


test_faces <- erp_labeled_comb_exp %>%
    dplyr::filter(lab_category == CORE_CAT)

dim(test_faces)
    head(test_faces, 1000) %>% View()

# test_faces %>%
#     dplyr::group_by(trial_idx) %>%
#     dplyr::summarize(n = n()) %>%
#     dplyr::ungroup()

# Read in the first channel from the
test_faces2 <- erp_cat_session1[[1]] %>%
                   dplyr::filter(lab_category == CORE_CAT)
head(test_faces2) %>% View()

test_exp_csv_nm <- "P35-ses-01-chn-01-Faces.csv"
test_exp_csv_pth <- here::here("data", "localizer_erp", "P35_specgram", "category",
                           "session_01", "faces", test_exp_csv_nm)
test_exp_csv <- readr::read_csv(test_exp_csv_pth)
head(test_exp_csv) %>% View()

test_faces2_subset <- test_faces2 %>%
                        select(X1:X1500)
View(test_faces2_subset %>% head() %>% dplyr::select(1:20))
View(test_exp_csv %>% head() %>% dplyr::select(1:20))

base::identical(test_exp_csv, test_faces2_subset)
