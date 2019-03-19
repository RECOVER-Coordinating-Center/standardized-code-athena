#!/usr/bin/env Rscript

.qual_tbl <- function(name, db, schema_tag) {
  name_map <- config('table_names')
  name <- ifelse(hasName(name_map, name), name_map[[name]], name)
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
    .qual_tbl(name, db, 'cdm_schema')

#' Connect to a CDM vocabulary table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
vocabulary_tbl <-
  function(name, db = config('db_src'))
    .qual_tbl(name, db, 'vocabulary_schema')

#' Connect to a result table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
results_tbl <-
  function(name, db = config('db_src'))
    .qual_tbl(name, db, 'results_schema')

#' Construct a (possibly schema-qualified) intermediate result table name
#' @md
#'
#' If the name passed in is already the result of a call to
#' [dbplyr::in_schema()], then no further modification is made.
#'
#' @param name The name of the table.  Defaults to a random string.
#' @param temporary Logical indicating whether the table is temporary.
#'     Defaults to the negation of `config(retain_intermed)`.
#'
#' @return The resultant name
intermed_name <- function(name = paste0(sample(letters, 12, replace = TRUE),
                                        collapse = ""),
                          temporary = ! config('retain_intermediates')) {
  if ( ! temporary  & ! is.na(config('results_schema')) &
       ! any(class(name) == 'ident_q') )
        dbplyr::in_schema(config('results_schema'), name)
    else name
}

#' Construct a name suitable for a temporary file
#' @md
#'
#' This is a convenience function that performs essentially the same
#' task as [intermed_name()], except that the returned name is always
#' intended for a temporary table.
#'
#' You should use this function only when you are certain as the
#' developer that you need to override the user's setting of
#' `retain_intermediates` and force the use of a temporary table
#' (e.g. for a large transient intermediate in a complex query).  In
#' general, it is better to respect the user's setting unless doing so
#' would result in an error (e.g. name collission) or a dangerous
#' expenditure of resources (e.g. preserving a very large but
#' transient resultset)
#'
#' @param name The name of the table.  Defaults to a random string.
#'
#' @return The resultant name
temp_name <- function(name = NA) {
  if (is.na(name)) intermed_name(temporary = TRUE)
  else intermed_name(name, temporary = TRUE)
}

#' Construct a name suitable for a permanent file
#' @md
#'
#' This is a convenience function that performs essentially the same
#' task as [intermed_name()], except that the returned name is always
#' intended for a persistnet (non-temporary) table.
#'
#' You should use this function only for final result tables, or less
#' commonly when you are certain as the developer that you need to
#' override the user's setting of `retain_intermediates` and force the
#' use of a persistent table (e.g. for an intermediate table you will
#' need to use in a future request).
#'
#' @param name The name of the table.  Defaults to a random string.
#'
#' @return The resultant name
results_name <- function(name = NA) {
  if (is.na(name)) intermed_name(temporary = FALSE)
  else intermed_name(name, temporary = FALSE)
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
    elts <- gsub('"', '', unlist(strsplit(table, '.', fixed = TRUE)))
    if (any(grepl('ora', class(con), ignore.case = TRUE))) {
      elts <- rev(elts)
      return(DBI::dbExistsTable(con, elts[1], schema = elts[2]))
    }
    else {
      table <- elts
    }
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
#' @param temporary A Boolean indicator of whether the table should be
#'   temporary.  Defaults to the opposite sense of
#'   `config('retain_intermediates)`.
#' @param ... Other arguments passed to [dplyr::compute()]
#'
#' @return A [dplyr::tbl()] resulting from [dplyr::compute()].
compute_new <- function(tblx,
                        name = paste0(sample(letters, 12, replace = TRUE),
                                      collapse = ""),
                        temporary = ! config('retain_intermediates'),
                        ...) {
    con <- if (inherits(tblx$src, 'src_dbi')) tblx$src$con else tblx$src
    name <- gsub('\\s+','_', name, perl = TRUE)
    if (! temporary ) name <- intermed_name(name, temporary)
    if (db_exists_table(con, name)) dplyr::db_drop_table(con, name)
    if (config('db_trace')) {
      show_query(tblx)
      explain(tblx)
      message(' -> ', name)
      start <- Sys.time()
      message(start)
    }
    rslt <- dplyr::compute(tblx, name = name, temporary = temporary, ...)
    if (config('db_trace')) {
      end  <- Sys.time()
      message(end, ' ==> ', format(end - start))
    }
    rslt
}

.output_csv <- function(data, name = NA, local = FALSE) {
  if (is.na(name)) name <- quo_name(enquo(data))
  dirs <- config('subdirs')
  collect(data) %>%
    write_csv(file.path(config('base_dir'),
                        ifelse(local, dirs$local_dir, dirs$result_dir),
                        paste0(name, '.csv')))
}

#' Like collect(), but allow for debug output
#' @md
#'
#' This function is a simple wrapper for [dplyr::collect()], but
#' allows for generation of trace output when running in debug mode.
#'
#' @param tblx The [dplyr::tbl()] expression to collect
#' @param ... Other arguments passed to [dplyr::collect()]
#'
#' @return A [dplyr::tbl()] resulting from [dplyr::collect()].
collect_new <- function(tblx, ...) {
  if (config('db_trace')) {
    if (inherits(tblx, 'tbl_sql')) {
      show_query(tblx)
      explain(tblx)
    }
    message(' -> collect')
    start <- Sys.time()
    message(start)
  }
  rslt <- collect(tblx)
  if (config('db_trace')) {
    end  <- Sys.time()
    message(end, ' ==> ', format(end - start))
  }
  rslt
}

#' Output contents of a tbl
#' @md
#'
#' Write the contents of a tbl to either a CSV file in the appropriate
#' results directory, or to the database, for future reference.
#'
#' @param data The tbl to write.
#' @param name The name (only) of the ouptut.  Defaults to the name of
#'   `data`.
#' @param file An indicator of whether the data should be written out
#'   as a CSV file.  If it is NA, no file will be written.
#'   If it is the string `local`, a CSV file will be written in the
#'   local output directory.  If it has any other value, a CSV file
#'   will be written in the results output directory.  Defaults to
#'   results` if in production mode, NA otherwise.
#' @param db A Boolean indicator of whether the data should be written
#'   to a results table in the database.  Defaults to TRUE if `file`
#'   is NA, and FALSE otherwise.
#' @param ... Additional arguments passed to the database table
#'   creation function.
#'
#' @return The result of the operation, typically a copy of data,
#'   possibly collect()ed.
output_tbl <- function(data, name = NA,
                       file = ifelse(config('execution_mode') ==
                                       'production', 'results', NA),
                       db = ifelse(!is.na(file), FALSE, TRUE), ...) {
  if (is.na(name)) name <- quo_name(enquo(data))

  if ( !is.na(file)) {
    local <- if (file == 'local') TRUE else FALSE
    rslt <- .output_csv(data, name, local)
  }

  if (db) {
    if (exists('src', where = data) &&
          same_src(config('db_src'), data$src)) {
      rslt <- compute_new(data, results_name(name),
                          temporary = FALSE, ...)
    }
    else {
      rslt <- copy_to(config('db_src'), collect(data),
                      results_name(name),
                      temporary = FALSE,
                      overwrite = TRUE, ...)
    }
  }

  invisible(rslt)
}
