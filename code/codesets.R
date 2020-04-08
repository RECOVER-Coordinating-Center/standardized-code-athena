#!/usr/bin/env Rscript

#' Read a codeset from a CSV file
#'
#' This function uses the codeset name to build a path to a CSV file
#' in the data request package containing the codeset data.
#'
#' The default expects the file to be in the spec_dir directory, to have a
#' header, and to contain four columns:
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
#'   [readr::read_csv()].
#' @param full_path A Boolean value indicating whether `name` represents a full
#'   path or a relative file name that should be expanded.
#'
#' @return A tbl contanining the codeset
#' @md
read_codeset <- function(name,
                         col_types = 'iccc',
                         full_path = FALSE) {
  path <-
    if_else(full_path, name,
            file.path(config('base_dir'), config('subdirs')$spec_dir,
                      paste0(name, '.csv')))
    read_csv(path, col_names = TRUE, col_types = col_types)
}

#' Create a db table with a codeset as per the query
#'
#' Reads the named codeset from a CSV file, and creates a like-named
#' intermediate table in the database with the contents.
#'
#' You will typically want to construct an index on the principal code column,
#' and if the codeset is very large, columns you will use to subset it
#' during use.
#'
#' Once a codeset is loaded, it is cached by name, and future calls to
#' load_codeset() with the same name will return the cached table.
#'
#' @inheritParams read_codeset
#' @param table_name An optional name for the table, if you want it to
#'   differ from `name`.
#' @param indexes A list of columns on which indexes should be created.
#' @param db A connection to the database into which to load the codeset.
#'
#' @return A tbl pointing to the table in the database
#' @md
load_codeset <- function(name,
                         col_types = 'iccc',
                         table_name = name,
                         indexes = list('concept_id'),
                         full_path = FALSE,
                         db = config('db_src')) {

  if (is.null(config('_codesets'))) config('_codesets', list())
  cache <- config('_codesets')
  if (! is.null(cache[[name]])) return(cache[[name]])

  codes <-
    copy_to_new(db,
                read_codeset(name, col_types),
                name = table_name,
                overwrite = TRUE,
                indexes = indexes)
  cache[[name]] <- codes
  config('_codesets', cache)
  codes
}
