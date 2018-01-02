#!/usr/bin/env Rscript

#-------------------------------------------------------------------------------
# PARSE command line arguments using `argparse`
#-------------------------------------------------------------------------------

# The following code is adapted from:
# https://github.com/trevorld/argparse/blob/master/exec/example.R

# Load the irb package and supporting libraries
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(here))

# create parser object
parser <- ArgumentParser()

# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument("-f", "--filename", type="character",
                    help=glue::glue("The new filename for the Rmd review file \\
                                    that will be created"))

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()

# Display the user arguments back to them just for clarity
# This will help them with defaults
print("creating the new review file with the following inputs:\n")
print(args)

# User input end date - convert to date format
inp_filename <- base::as.character(args[['filename']])

#-------------------------------------------------------------------------------
# Load the setup utils code
#-------------------------------------------------------------------------------

SETUP_UTILS_SRC <- file.path(here::here(), "setup-utils.R")
source(SETUP_UTILS_SRC)

# Clean the user filename
cleaned_filename <- create_valid_filename(inp_filename)
user_msg_01 <- glue::glue("Cleaned User filename:\n{cleaned_filename}")
print(user_msg_01)

template_filepath <- file.path(here::here()
                               , "reviews"
                               , "template-paper-review-tufte.Rmd")
new_report_filepath <- file.path(here::here()
                                 , "reviews"
                                 , stringr::str_c(cleaned_filename
                                                  , ".Rmd", sep = ""))

#-------------------------------------------------------------------------------
# Copy the TEMPLATE or give user appropriate error message
#-------------------------------------------------------------------------------

if(file.exists(new_report_filepath)){
    user_msg_02 <-
        glue::glue(">> ERROR: The new file to be created already exists: \\
                   \n{new_report_filepath}.\\
                   \n>> Please DELETE it or re-run with a new filename!")
    print(user_msg_02)
} else {
    base::file.copy(from = template_filepath,
                    to = new_report_filepath)
    user_msg_03 <-
        glue::glue(">> The following file has been created: \\
                   \n {new_report_filepath}. \\
                   \n>> Feel free to start reviewing it!")
    print(user_msg_03)
}