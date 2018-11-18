#!/usr/bin/env Rscript

# Top-level code for execution of data request
#'
#' The following are typical packages called at startup. Edit as
#' necessary for your script.
#'
#' The following are packages typically required for execution:

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))

# Required for execution using Rscript
suppressPackageStartupMessages(library(methods))

#'
#' The .load() function is optional. However, before any query hits
#'  the database, the function `.env_setup` must be run. The
#'  `.env_cleanup` should be called *after* the `.enf_setup` function.
#'  Both should be called ideally before any database activity.
#'
#'  Below is an example of how to operationalize this. The .load
#'  function loads all scripts and the .run function executes all
#'  scripts.
#'


.load <- function(here) {
    source(file.path(here, 'code', 'config.R'))
    source(file.path(here, 'code', 'req_info.R'))
    source(config('site_info'))
    source(file.path(here, config('subdirs')$code_dir, 'setup.R'))
    source(file.path(here, config('subdirs')$code_dir, 'util_tbl.R'))
    source(file.path(here, config('subdirs')$code_dir, 'util_cohort.R'))
    source(file.path(here, config('subdirs')$code_dir, 'codesets.R'))
    source(file.path(here, config('subdirs')$code_dir, 'cohorts.R'))
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                          'cohort_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'analyze_.+\\.R', full.names = TRUE))
      source(fn)

    .env_setup()

    for (def in c('retain_intermediates', 'results_schema')) {
      if (is.na(config(def)))
        config(def, config(paste0('default_', def)))
    }

    base_dir
}

.run  <- function(base_dir = config('base_dir')) {

  init_sum(cohort = 'Start', persons = 0)

  message('Insert message here')
  starter <- first_function(config('db_src')) %>%
    .compute_intermed('')  #if applicable

  # counts the number of persons for this part of the query
  append_sum(rslt,
             cohort = 'Name of first computation or cohort (if feasibility query)',
             persons = distinct_ct(starter))

  next_cohort <- function_call_for_study(starter) %>%
    .other_functions_if_applicable('') # if applicable
  append_sum(rslt,
             cohort = 'Name of second computation or cohort (if feasibility query)',
             persons = distinct_ct(next_cohort))


  more_functions_as_needed()


  # Simple output to CSV
  output_tbl(starter, name = 'base_cohort', file = TRUE, db = FALSE)

  # Alternatively, replace person_ids and write output
  pers_xwalk <- gen_xwalk(starter, 'person_id')
  output_tbl(new_id(starter, 'person_id', pers_xwalk, replace = TRUE),
             name = 'base_cohort', file = 'result', db = TRUE)
  output_tbl(new_id(next_cohort, 'person_id', pers_xwalk, replace = TRUE),
             name = 'next_cohort', file = 'result', db = TRUE)
  # Don't forget to save the crosswalk
  output_tbl(pers_xwalk, name = 'person_xwalk', file = 'local', db = TRUE)

  # Write step summary log to CSV and/or database, as determined by configuration
  output_sum()

  invisible(rslt)

}

#'
#' The run_request executes all the functions required to execute the query.
#' The `run.R` file calls on run_request and provides the parameters to execute
#' the query.
#'
#' @param base_dir Path to the top of the data request files.  This is
#'   typically specified in the `run.R`.
#' @param site_info Path to the file containing site-level
#'   configuration that is typically not request-specific.  Defaults
#'   to `site/site_info.R` below base_dir.
#'
#' @return The result of .run()

run_request <- function(base_dir) {
    base_dir <- .load(base_dir)
    on.exit(.env_cleanup())
    .run(base_dir)
}
