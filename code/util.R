#!/usr/bin/env Rscript

.qual_tbl <- function(name, db, schema_tag) {
  if (! is.na(config(schema_tag))) {
    con <- if (inherits(db, 'src_dbi')) db$con else db
    name <- dbplyr::in_schema(config(schema_tag),
                              DBI::dbQuoteIdentifier(con, name))
  }

  tbl(db, name)
}



#' Connect to a CDM data table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
cdm_tbl <-
  function(name, db = config('db_src'))
    .qual_tbl(config('table_names')[[name]], db, 'cdm_schema')


#' Connect to a CDM vocabulary table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
vocabulary_tbl <-
  function(name, db = config('db_src'))
    .qual_tbl(config('table_names')[[name]], db, 'vocabulary_schema')

#' Connect to a result table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
results_tbl <-
  function(name, db = config('db_src'))
    .qual_tbl(config('table_names')[[name]], db, 'results_schema')

#' Construct a (possibly schema-qualified) intermediate result table name
#' @md
#'
#' It the name passed in is already the result of a call to
#' [dbplyr::in_schema()], then no further modification is made.
#'
#' @param name The name of the table.  Defaults to a random string.
#' @param temp Logical indicating whether the table is temporary.
#'     Defaults to the negation of `config(retain_intermed)`.
#'
#' @return A [dplyr::tbl()]] pointing to the table
intermed_name <- function(name = paste0(sample(letters, 12, replace = TRUE),
                                        collapse = ""),
                          temporary = ! config('retain_intermediates')) {
  if ( ! temporary  & ! is.na(config('result_schema')) &
       ! any(class(name) == 'ident_q') )
        dbplyr::in_schema(config('result_schema'), name)
    else name
}

#' Count unique persons in a resultset. This will be used as one of the outputs
#' for `append_sum` functions in the driver file.
#' @md
#'
#' @param rset The [dplyr::tbl()] describing the resultset
#'
#' @return The number of distinct persons (`person_id`s)
pers_ct <- function(rs) {
  ct <- rs %>%
    summarise(pers_ct = n_distinct(person_id)) %>%
    collect()
  as.data.frame(ct)[['pers_ct']]
}

#' Create a new tibble for results and progress info
#' @md
#'
#' This function creates a [tibble::tibble()] with the information
#' passed to it, plus three additional columns: `site`, which is the
#' name of the current site from `site_info.R`, `stamp`, which
#' contains the ISO-8601 current time, and `used`, which contains a
#' summary of CPU utilization to this point.
#'
#' Additional rows can be added to this structure by [append_sum()].
#'
#' @param ... Name-value pairs to be added to the tibble
#'
#' @return A tibble as described above
init_sum <- function(...)
    tibble(!!! dots_list(...),
           site = config('site'),
           stamp = Sys.time(),
           used = paste(proc.time(), collapse = ';'))


#' Add rows to a summary tibble
#' @md
#'
#' This function adds rows to an existing tibble, filling in values
#' for the three extra columns created by [init_sum()].
#'
#' @param cur The [tibble::tibble()] to which to add
#' @param ... Additional name-value pairs to add
#'
#' @return The augmented [tibble::tibble()]
append_sum <- function(cur, ...) {
    dplyr::union(tibble(!!! dots_list(...),
                        site = config('site'),
                        stamp = Sys.time(),
                        used = paste(proc.time(), collapse = ';')),
                 cur)
}

#' Test whether a table exists in a database, schema-aware
#' @md
#'
#' This function has the same purpose as DBI::dbExistsTable() or
#' dbplyr::db_drop_table(), but neither of these are able to test a
#' table name created by dbplyr::in_schema().
#'
#' @param db A [dplyr::src()] or DBI connection
#' @param table The name of the table, or an object created by
#'   [dbplyr::in_schema()], or a DBI-style path vector
#'
#' @return TRUE if the table exists, and FALSE otherwise.
db_exists_table <- function(db, table) {
  con <- if (inherits(db, 'src_dbi')) db$con else db
  if (any(class(table) == 'ident_q')) {
    table <- unlist(strsplit(table, '.', fixed = TRUE))
  }
  DBI::dbExistsTable(con, table)
}

#' Like compute(), but insure the target doesn't exist
#' @md
#'
#' One of the limitations of [dplyr::compute()] as applied to database
#' tables is that it does not support the `overwrite` parameter, so
#' calls are not idempotent: if the target table already exists, an
#' error occurs.  This function simply emulates the behavior of
#' [dbplyr::copy_to()]'s `overwrite` option in dropping the target
#' table if it exists.  Note that there is no `overwrite` vs `append`
#' option here; the target is always created anew.
#'
#' @param tblx The [dplyr::tbl()] expression to compute
#' @param name The name to give the table
#' @param ... Other arguments passed to [dplyr::compute()]
#'
#' @return A [dplyr::tbl()] resulting from [dplyr::compute()].
compute_new <- function(tblx,
                        name = paste0(sample(letters, 12, replace = TRUE),
                                      collapse = ""),
                        temporary = ! config('retain_intermediates'),
                        ...) {
    con <- if (inherits(tblx$src, 'src_dbi')) tblx$src$con else tblx$src
    if (db_exists_table(con, name)) dplyr::db_drop_table(con, name)
    if (! temporary ) name <- intermed_name(name, temporary)
    dplyr::compute(tblx, name = name, temporary = temporary, ...)
}

