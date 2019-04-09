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
  if ( ! is.na(config('req_tag'))) {
    if (any(class(name) == 'ident_q') ) {
      # Best guess - works but relies on stringification of dbplyr::in_schema()
      parts <- regmatches(name, regexec('(.+)\\.(.+)', name))
      name <- dbplyr::in_schema(parts[[1]][2],
                                paste0(parts[[1]][3], config('req_tag'),
                                       collapse = ''))
    }
    else {
      name <- paste0(name, config('req_tag'), collapse = '')
    }
  }
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
#' dbplyr::db_has_table(), but neither of these are able to test a
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
  }
  else {
    elts <- table
  }
  if (any(grepl('ora', class(con), ignore.case = TRUE)) &&
      length(elts) > 1) {
    elts <- rev(elts)
    return(DBI::dbExistsTable(con, elts[1], schema = elts[2]))
  }
  else if (any(class(con) == 'PostgreSQLConnection') &&
           length(elts) == 1) {
    res <-
      dbGetQuery(con,
                 paste("select tablename from pg_tables where ",
                       "schemaname !='information_schema' and schemaname !='pg_catalog' ",
                       "and schemaname in (select schemas[nr] from ",
                       "(select *, generate_subscripts(schemas,1) as nr ",
                       "from (select current_schemas(true) as schemas) a ",
                       ") b where schemas[nr] <> 'pg_catalog') and tablename=",
                       DBI::dbQuoteLiteral(con, elts[1]), sep = ""))
    return(as.logical(dim(res)[1]))
  }
  else if (any(class(con) == 'PqConnection') &&
           length(elts) > 1) {
    table <- DBI::SQL(paste0(DBI::dbQuoteIdentifier(db, elts[1]),
                             '.',
                             DBI::dbQuoteIdentifier(db, elts[2])))
    return(DBI::dbExistsTable(con, table))
  }
  else {
    return(DBI::dbExistsTable(con, elts))
  }
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
    name <- intermed_name(name, temporary)
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

.output_csv <- function(data, name = NA, local = FALSE) {
  if (is.na(name)) name <- quo_name(enquo(data))
  dirs <- config('subdirs')
  collect_new(data) %>%
    write_csv(file.path(config('base_dir'),
                        ifelse(local, dirs$local_dir, dirs$result_dir),
                        paste0(name, '.csv')))
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
#' @param local A Boolean value indicating whether the data should be
#'   written to a local-only location, which is not typically returned
#'   with query results.  Defaults to FALSE.
#' @param file An indicator of whether the data should be written out
#'   as a CSV file.  If it is NA, no file will be written. If it has
#'   any other value, a CSV file will be written with that name (with
#'   a ".csv" suffic appended).  Defaults to results` if in production
#'   mode, NA otherwise.
#' @param db A Boolean indicator of whether the data should be written
#'   to a results table in the database.  Defaults to TRUE if `file`
#'   is NA, and FALSE otherwise.
#' @param ... Additional arguments passed to the database table
#'   creation function.
#'
#' @return The result of the operation, typically a copy of data,
#'   possibly collect()ed.
output_tbl <- function(data, name = NA, local = FALSE,
                       file = ifelse(config('execution_mode') ==
                                       'production', 'results', NA),
                       db = ifelse(!is.na(file), FALSE, TRUE), ...) {
  if (is.na(name)) name <- quo_name(enquo(data))

  if ( !is.na(file)) {
    rslt <- .output_csv(data, name, local)
  }

  if (db) {
    if (exists('src', where = data) &&
          same_src(config('db_src'), data$src)) {
      rslt <- compute_new(data, results_name(name),
                          temporary = FALSE, ...)
    }
    else {
      rslt <- copy_to(config('db_src'), collect_new(data),
                      results_name(name),
                      temporary = FALSE,
                      overwrite = TRUE, ...)
    }
  }

  invisible(rslt)
}
