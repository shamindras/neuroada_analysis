# Clean up
# TODO: Delete the rm line
rm(list = ls())
cat("\014")
dev.off()

library(here)
library(tidyverse)
library(magrittr)
library(readxl)
devtools::install_github("shamindras/neuroada")

#*******************************************************************************
# CORE VARIABLES AND OUTPUT CREATION
#*******************************************************************************

#-------------------------------------------------------------------------------
# Define USER VARIABLES
#-------------------------------------------------------------------------------

create_adjacent_pairs_chnls <- function(n_data_type,
                                        n_patient_num,
                                        file_ext){
    # Source directory path
    src_dir_path <- stringr::str_c("P", n_patient_num,
                                   "_Electrode_Key", file_ext) %>%
        here::here("data", n_data_type, .)
    src_dir_path
    file.exists(src_dir_path)

    # Output directory to unzip the tar file
    out_dir_path <- stringr::str_c("P", n_patient_num,
                                   "_Electrode_Key_adj_pairs", ".csv") %>%
        here::here("data", n_data_type, .)

    #-------------------------------------------------------------------------------
    # Create Adjacent pairs
    #-------------------------------------------------------------------------------

    pat_xl <- readxl::read_excel(path = src_dir_path,
                                 sheet = 1,
                                 range = "G3:H500") %>%
        stats::na.omit() %>%
        magrittr::set_colnames(x = .,
                               value = c("chnl_plt_num_ref", "chnl_num")) %>%
        dplyr::mutate(chnl_plt_ref =
                          stringr::str_replace_all(string = chnl_plt_num_ref,
                                                   pattern = "[:digit:]",
                                                   "")) %>%
        dplyr::select(chnl_plt_ref, everything())

    pat_xl2 <- pat_xl %>%
        dplyr::group_by(chnl_plt_ref) %>%
        dplyr::mutate(chnl_num_prev = lag(chnl_num)) %>%
        stats::na.omit()

    pat_xl3 <- pat_xl2 %>%
        magrittr::set_colnames(x = .,
                               value = c("chnl_plt_ref",
                                         "chnl_plt_num_ref",
                                         "chnl_num_prev",
                                         "chnl_num")) %>%
        dplyr::select(chnl_plt_ref, chnl_plt_num_ref,
                      chnl_num, chnl_num_prev)

    pat_xl4 <- pat_xl2 %>%
                dplyr::bind_rows(pat_xl3) %>%
                magrittr::set_colnames(x = .,
                                       value = c("chnl_plt_ref",
                                                 "chnl_plt_num_ref",
                                                 "chn_x",
                                                 "chn_y")) %>%
                # We want to 0-index the channels for Python
                dplyr::mutate(chn_x = chn_x - 1,
                              chn_y = chn_y - 1) %>%
                readr::write_csv(out_dir_path)
}

# User variables
N_DATA_TYPE <- "electrode_keys"
FILE_EXT <- ".xlsx"
all_n_patient_nums <- c(32, 34, 35, 37, 41)

# Create all patient adjacent channel pairs
all_n_patient_nums %>%
    purrr::walk(.x = ., ~create_adjacent_pairs_chnls(n_data_type=N_DATA_TYPE,
                                                     n_patient_num=.x,
                                                     file_ext=FILE_EXT))