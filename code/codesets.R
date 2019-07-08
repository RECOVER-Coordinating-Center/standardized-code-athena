#!/usr/bin/env Rscript

#' Read a codeset from a CSV file
#' @md-
#'
#' This function uses the codeset name to build a path to a CSV file
#' in the data request package containing the codeset data.  The default
#' expects the file to be in the `specs` directory, to have a header,
#' and to contain four columns:
#'
#'  * concept_id - integer
#'  * concept_name - string
#'  * concepd_code - string
#'  * vocabulary_id - string
#'
#' @param name The name of the codeset.  Typically just the file name
#'   without `.csv` suffix, but if for some reason you've expanded the
#'   directory tree under `specs`, you may prefix it with
#'   subdirectories.
#' @param col_types A column specification compatible with
#'   [readr::read_csv()].  Defaults to `'iccc'`
#'
#' @return A [tibble::tibble()] contanining the codeset
read_codeset <- function(name,
                         col_types = 'iccc') {
    path <- file.path(config('base_dir'), config('subdirs')$spec_dir,
                      paste0(name, '.csv'))
    read_csv(path, col_names = TRUE, col_types = col_types)
}

#' Create a db table with a codeset as per the query
#' @md
#'
#' Reads the named codeset from a CSV file, and creates a like-named
#' intermediate table in the database with the contents.  The table is
#' indexed on `concept_id`.
#'
#' Once a codeset is loaded, it is cached by name, and future calls to
#' load_codeset() with the same name will return the cached table.
#'
#' @param name The name of the codeset
#' @param col_types A column specification compatible with
#'   [readr::read_csv()] to be used when reading the codeset file.
#'   Defaults to `'iccc'`
#' @param table_name An optional name for the table, if you want it to
#'   differ from `name`.  Defaults to `name`.
#' @param db A database connection; you will rarely need to change
#'   this from the default.  Defaults to `config('db_src')`.
#'
#' @return A [dplyr::tbl()] pointing to the table
load_codeset <- function(name,
                         col_types = 'iccc',
                         table_name = name,
                         db = config('db_src')) {

  if (is.null(config('_codesets'))) config('_codesets', list())
  cache <- config('_codesets')
  if (! is.null(cache[[name]])) return(cache[[name]])

  codes <-
    copy_to_new(db,
                read_codeset(name, col_types),
                name = intermed_name(table_name),
                temporary = ! config('retain_intermediates'),
                overwrite = TRUE,
                indexes = list('concept_id'))
  cache[[name]] <- codes
  codes
}
