#!/usr/bin/env Rscript

# Hacks necessary for Oracle support with current libraries

# OK, Oracle temporary tables aren't temporary; their contents are.
# So we need to clean them out when we're done.
.env_cleanup <- function() {
  dm <- config('ora_drop_me')
  if (length(dm) == 0) return(invisible(TRUE))
  
  db <- config('db_src')
  con <- if (inherits(db, 'src_dbi')) db$con else db
  vapply(unique(dm),
         function (t)
           tryCatch(DBI::dbExecute(con,
                                   paste0('DROP TABLE ',
                                          DBI::dbQuoteIdentifier(con,t))),
                    error = function(e) { message(t, ' - ', e); 0L }),
         FUN.VALUE = 0L)
  invisible(TRUE)
}


.env_setup <- function() {
  
  if (! any(class(config('db_src')) == 'OraConnection'))
    return(invisible(TRUE))
  
  # List of Oracle "temp" tables that need to be dropped
  config('ora_drop_me', character(0))
  
  assignInNamespace('db_save_query.DBIConnection',
                    function (con, sql, name, temporary = TRUE, ...) {
                      if (temporary) {
                        if (any(class(con) == 'OraConnection')) {
                          temptbl <- dbplyr::sql(' ')
                          config('ora_drop_me',
                                 append(config('ora_drop_me'), name))
                        }
                        else {
                          temptbl <- dbplyr::sql('TEMPORARY ')
                        }
                      }
                      tt_sql <- dbplyr::build_sql("CREATE ",
                                                  if (temporary) temptbl,
                                                  'TABLE ',
                                                  dbplyr::as.sql(name),
                                                  ' AS ', sql,
                                                  con = con)
                      DBI::dbExecute(con, tt_sql)
                      name
                    }, ns = 'dbplyr')
  
  assignInNamespace('db_begin.DBIConnection',
                    function (con, ...) {
                      if (any(class(con) == 'OraConnection'))
                        TRUE
                      else
                        dbBegin(con)
                    }, ns = 'dbplyr')
  
  assignInNamespace('db_drop_table.OraConnection',
                    function(con, tabname, force = FALSE, ...) {
                      schema <- NULL
                      # For Oracle databases, we have a conundrum
                      # when schema is explicit.  The dbplyr
                      # implementation stores schema and table as
                      # a dot-separated string, while the ROracle
                      # implementation requires schema be passed
                      # using a separate parameter and the table
                      # name be bare.  We try to accommodate this
                      # by assuming that if the dplyr table has
                      # class ident_q (used when there's a schema
                      # name), the first dot will be the schema
                      # separator, and we break apart the string
                      # accordingly.
                      if (any(class(con) == 'OraConnection') &
                          any(class(tabname) == 'ident_q') &
                          any(grepl('.', tabname, fixed = TRUE))) {
                        parts <- gsub('"', '', dbplyr::as.sql(tabname), fixed = TRUE)
                        parts <- regmatches(parts,
                                            regexec('(.+)\\.(.+)', parts,
                                                    perl = TRUE))[[1]]
                        tabname <- parts[3]
                        schema <- parts[2]
                      }
                      if (DBI::dbExistsTable(con, name = tabname, schema = schema)) {
                        ifex <- if (any(class(con) == 'OraConnection'))
                          dbplyr::sql(' ')
                        else
                          dbplyr::sql('IF EXISTS ')
                        sql <- dbplyr::build_sql(
                          "DROP TABLE ", if (force) ifex,
                          DBI::dbQuoteIdentifier(con, tabname),
                          con = con
                        )
                        DBI::dbExecute(con, sql)
                      }
                      else 0
                    }, ns = 'dbplyr')
  
  assignInNamespace('db_write_table.DBIConnection',
                    function(con, tabname, types, values, temporary = TRUE, ...) {
                      if (any(class(con) == 'OraConnection')) {
                        if (temporary) config('ora_drop_me',
                                              append(config('ora_drop_me'), tabname))
                        schema <- NULL
                        if (any(class(tabname) == 'ident_q') &
                            any(grepl('.', tabname, fixed = TRUE))) {
                          parts <- gsub('"', '', dbplyr::as.sql(tabname), fixed = TRUE)
                          parts <- regmatches(parts,
                                              regexec('(.+)\\.(.+)', parts,
                                                      perl = TRUE))[[1]]
                          tabname <- parts[3]
                          schema <- parts[2]
                        }
                        DBI::dbWriteTable(con, name = tabname, schema = schema,
                                          value = values, field.types = types,
                                          temporary = temporary, row.names = FALSE, ...)
                      }
                      else 
                        DBI::dbWriteTable(con,
                                          name = dbplyr::dbi_quote(dbplyr::as.sql(tabname), con),
                                          value = values, field.types = types,
                                          temporary = temporary, row.names = FALSE, ...)
                      tabname
                    }, ns = 'dbplyr')
  
  assignInNamespace('db_create_index.DBIConnection',
                    function(con, tabname, columns, name = NULL,
                             unique = FALSE, ...) {
                      assertthat::assert_that(is_string(tabname), is.character(columns))
                      
                      name <- name %||%
                        gsub('[^A-Za-z0-9_]+', '',
                             paste0(c(tabname, columns), collapse = "_"))
                      if (any(class(con) == 'OraConnection') &
                          nchar(name) > 30) {
                        name <- gsub('(.)\\1+', '\\1', name)
                        if (nchar(name) > 30)
                          name <- gsub('[aeiou]', '', name)
                        if (nchar(name) > 30)
                          name <-
                            paste0(sample(letters, 12, replace = TRUE),
                                   collapse = "")
                      }
                      fields <- dbplyr::escape(ident(columns), parens = TRUE, con = con)
                      sql <- dbplyr::build_sql(
                        "CREATE ", if (unique) sql("UNIQUE "), "INDEX ",
                        dbplyr::as.sql(name),
                        " ON ", dbplyr::as.sql(tabname), " ", fields,
                        con = con)
                      
                      dbExecute(con, sql)
                    }, ns = 'dbplyr')
  
  assignInNamespace('in_schema',
                    function(schema, table) {
                      ei <- function(x)
                        if (inherits(x, c('ident_q', 'SQL')))
                          x
                      else
                        dbplyr::escape(dbplyr::ident(x))
                      dbplyr::ident_q(paste0(ei(schema), '.', ei(table)))
                    }, ns = 'dbplyr')
  
  invisible(TRUE)
}
