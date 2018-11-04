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


.load <- function(here, site_info) {
    source(file.path(here, 'code', 'config.R'))
    source(file.path(here, 'code', 'req_info.R'))
    source(site_info)
    source(file.path(here, config('subdirs')$code_dir, 'setup.R'))
    source(file.path(here, config('subdirs')$code_dir, 'util_tbl.R'))
    source(file.path(here, config('subdirs')$code_dir, 'util_cohort.R'))
    source(file.path(here, config('subdirs')$code_dir, 'codesets.R'))
    source(file.path(here, config('subdirs')$code_dir, 'cohorts.R'))
    for (f in list.files(file.path(here, config('subdirs')$code_dir),
                         'cohort_*.R', full.names = TRUE))
      source(f)

    .env_setup()
    on.exit(.env_cleanup())

    for (def in c('retain_intermediates', 'results_schema')) {
      if (is.na(config(def)))
        config(def, config(paste0('default_', def)))
    }

    base_dir
}

#'
#' If your code, in the `util.R` file, has time stamps to account for,
#' you should consider where to add those functions somewhere in the
#' output files in order to calculate computing time.  In this sample,
#' the two functions (found in the sample `util.R` file) are called
#' init_sum, to initiate the computation and create a table. The
#' append_sum adds rows to the table that computes the length of time
#' it takes for a given computation. In this sample below, these two
#' functions are started in a `rslt` function that then gets renamed
#' and called several times, based on the function it is meant to
#' compute. Every time `rslt` is called, it pulls the existing table,
#' executes the function, and gets assigned `rslt` again - with the
#' new time stamp appended to it.In addition to time logging, `rslt`
#' also includes patient counts every time it is called.
#'
#'

.run  <- function(base_dir = config('base_dir')) {

  rslt <- init_sum(cohort = 'Start',
                   persons = 0)

  message('Insert message here')
  starter <- first_function(config('db_src')) %>%
    .compute_intermed('')  #if applicable

  # counts the number of persons for this part of the query
  rslt <- append_sum(rslt,
                     cohort = 'Name of first computation or cohort (if feasibility query)',
                     persons = pers_ct(starter))

  next_cohort <- function_call_for_study(starter) %>%
    .other_functions_if_applicable('') # if applicable
  rslt <- append_sum(rslt,
                     cohort = 'Name of second computation or cohort (if feasibility query)',
                     persons = person_ct(next_cohort))


  more_functions_as_needed()

  write_csv(arrange(rslt, stamp),
            file.path(base_dir,config('subdirs')$result_dir,
                      paste0('counts_', config('site'), '.csv')))  # if writing to CSV

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

run_request <- function(base_dir,
                        site_info = file.path(base_dir, 'site', 'site_info.R')) {
    base_dir <- .load(base_dir, site_info)
    .run(base_dir)
}
