#!/usr/bin/env Rscript

#' Read a codeset from a CSV file
#' @md-
#'
#' This function uses the codeset name to build a path to a CSV file
#' in the data request package containing the codeset data.  It
#' expects the file to be in the `specs` directory, to have a header,
#' and to contain four columns:
#'
#'  * concept_id - integer
#'  * concept_name - string
#'  * concepd_code - string
#'  * vocabulary_id - string
#'
#' @param name The name of the codeset.  Typically just the file name
#'     without `.csv` suffix, but if for some reason you've expanded
#'     the directory tree under `specs`, you may prefix it with
#'     subdirectories.
#'
#' @return A [tibble::tibble()] contanining the codeset
read_codeset <- function(name) {
    path <- file.path(config('base_dir'), config('subdirs')$spec_dir,
                      paste0(name, '.csv'))
    read_csv(path, col_names = TRUE, col_types = 'iccc')
}

#' Create a db table with a codeset as per the query
#' @md
#'
#' Reads the named codeset from a CSV file, and creates a like-named
#' intermediate table in the database with the contents.  The table is
#' indexed on `concept_id`.
#'
#' @param name The name of the codeset
#' @param table_name An optional name for the table, if you want it to
#'     differ from `name`.  Defaults to `name`.
#' @param db A database connection; you will rarely need to change
#'     this from the default.  Defaults to `config('db_src')`.
#'
#' @return A [dplyr::tbl()] pointing to the table
load_codeset <- function(name,
                         table_name = name,
                         db = config('db_src')) {
    copy_to(db,
            read_codeset(name),
            name = intermed_name(table_name),
            temporary = ! config('retain_intermediates'),
            overwrite = TRUE,
            indexes = list('concept_id'))
}

#' codeset_1 contains any codeset relevant to the study.
#' @md
#' 
#' @return A [dplyr::tbl()] in `db` containing the concepts
codeset_1 <- function() load_codeset('codeset_1')


#'
#' codeset_2 contains any codeset relevant to the study.
#' 
#'
#' @return A [dplyr::tbl()] in `db` containing the concepts
codeset_2 <- function() load_codeset('codeset_2')


#' 
#' codeset_3 contains any codeset relevant to the study 
#' 
#' @return A [dplyr::tbl()] in `db` containing the concepts
codeset_3 <- function() load_codeset('codeset_3')

