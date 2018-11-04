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

#' Count unique elements in a resultset.
#' @md
#'
#' Given a column (e.g. a foreign key), returns as an integer the
#' number of unique elements in that column. This can be used as one
#' of the outputs for `append_sum` functions in the driver file.
#'
#' @param rset The [dplyr::tbl()] describing the resultset
#' @param id_col The name of the column in which to count elements.
#'   Defaults to `person_id`
#'
#' @return The number of distinct values
distinct_ct <- function(rs, id_col = 'person_id') {
  ct <- rs %>%
    summarise(dist_ct = n_distinct(!!id_col)) %>%
    collect()
  as.data.frame(ct)[['dist_ct']]
}

#' Create a sequence with which to replace IDs
#' @md
#'
#' This funcion creates an integer sequence matching distinct values
#' in a column of the input tbl. The resulting crosswalk can be used
#' to replace IDs in datasets when unique identifiers that are not
#' stable across datasets are needed.
#'
#' @param input A tbl containing the column of interest
#' @param id_col The name of the column containing IDs to be mapped.
#' @param rep_col The name of the column to contain the replacement
#'   integers.  Defaults to `rep_id`.
#'
#' @return A two-column tbl containing unique values from id_col and
#'   replacement values in rep_col
gen_xwalk <- function(input, id_col, rep_col = 'seq_id') {
  idq <- quo_name(id_col)
  req <- quo_name(rep_col)

  input %>% select(!!idq) %>% distinct(!!idq) %>%
    arrange_at(vars(!!idq)) %>% # Oracle insists on an ordering
    mutate( !!req := row_number())
}

#' Add or replace one ID column with another
#'
#' This function takes a crosswalk between the values in a specific
#' column of a tbl and a new value, and either adds a column with the
#' new values or replaces the old values with the new.  It is intended
#' for use replacing an ID that should be redacted out of the data
#' with a lower-risk ID.
#'
#' @param data The tbl containing the data
#' @param id_col The name of the column containing the ID to join.
#'   Defaults to the name of data suffixed by `_id`
#' @param xwalk The tbl containing crosswalk between id_col and new
#'   values.  Defaults to using a sequence of integers.
#' @param replace  If TRUE, replace the contents of id_col with the
#'   first non-id_col column of xwalk.  If FALSE, leave the original
#'   id_col in place.  Defaults to FALSE.
#'
#' @return The tbl with augmented or replaced IDs
new_id <- function(data,
                   id_col = paste0(deparse(substitute(data)), '_id'),
                   xwalk = select_at(data, vars(id_col)) %>%
                     mutate(seq_id = row_number()),
                   replace = FALSE) {

  xform <- data %>% left_join(xwalk, by = id_col)
  if (replace) {
    newcol <- grep(id_col, tbl_vars(xwalk),
                   invert = TRUE, value = TRUE, fixed = TRUE)[1]
    xform <- xform %>% select(-one_of(id_col)) %>%
      rename_at(vars(newcol), function (x) { id_col })
  }
  xform
}
