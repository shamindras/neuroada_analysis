#-------------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------------
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(magrittr))

#-------------------------------------------------------------------------------
# Install and load multiple R packages.
#-------------------------------------------------------------------------------

#' Install and load multiple R packages.
#' check to see if packages are installed.
#' Install them if they are not, then load them into the R session.
#' source: \url{https://gist.github.com/stevenworthington/3178163}
#' @param pkg character: list of R packages
#' @export
#'
#' @examples
#' pckgs <- c("here", "tidyverse", "glue", "secret", "RPostgreSQL")
#' ipak(pckgs)
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

#' Creates a valid filename from a user input
#'
#' @param inp_filename (character) : Creates a valid version of the user input
#' filename by first removing punctuation and replacing with spaces, and then
#' trimming and replacing all continuous spaces with underscores
#' @export
create_valid_filename <- function(inp_filename){
    inp_filename %>%
        stringr::str_replace_all(string = ., pattern = "[[:punct:]]"
                                 , replacement = " ") %>%
        # Replace "_" with " "
        stringr::str_replace_all(string = ., pattern = "\\_"
                                 , replacement = " ") %>%
        # Replace "." with " "
        stringr::str_replace_all(string = ., pattern = "\\."
                                 , replacement = " ") %>%
        # Replace multiple spaces with a single space
        stringr::str_replace_all(string = ., pattern = "\\s+"
                                 , replacement = " ") %>%
        # Trim leading and trailing spaces
        stringr::str_trim(string = ., side = "both") %>%
        # Replace " " with "-"
        stringr::str_replace_all(string = ., pattern = "\\s+"
                                 , replacement = "-") %>%
        base::return()
}

#-------------------------------------------------------------------------------
# Setup SECRET credentials
#-------------------------------------------------------------------------------

#' Prompts users for CMU sculptor database to be stored in the vault
#'
#' @param cred_desc (character) : A quick description of the credential at the
#' prompt
#' @param cred_varname (character) : the variable name to be assigned in R
#' for the credential value
#' @param hide_value (logical) : default \code{FALSE} used to toggle whether
#' the value entered by the user should be displayed back to them in a
#' friendly manner
#'
#' @export
input_credential_parse <- function(cred_desc, cred_varname,
                                   hide_value = FALSE){

    # User Prompt for the credential
    inp_prompt <- glue::glue("Enter your {toupper(cred_desc)}: ")
    cat("\n")
    cat(inp_prompt)

    # Assign the credential to the variable name passed by the user
    # Place it in the global environment so that it can be accessed
    # later and added to the vault
    assign(x = cred_varname, value = readLines("stdin", n = 1)
           , envir = .GlobalEnv)

    # Display message to the user of value passed
    if(!hide_value){
        cat(glue::glue("The {base::toupper(cred_desc)} has been stored in the variable \\
                       named '{toupper(cred_varname)}', with the value of: "))
        cat(get(cred_varname))
        cat("\n")
    } else{
        cat(glue::glue("The {toupper(cred_desc)} has been stored in the variable \\
                       '{toupper(cred_varname)}':"))
        cat("\n")
    }
}

