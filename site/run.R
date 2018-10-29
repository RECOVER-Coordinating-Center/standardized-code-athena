#!/usr/bin/env Rscript

#' PEDSnet data request package
#' @md
#'
#' In order to execute this data request, please do the following:
#'
#'  1. Make sure your database and R environments meet the
#'     requirements for executing this code, as described in the
#'     `README.pdf` file distributed with the package.
#'  2. Insure the appropriate information for your site is in the
#'     `site_info.R` file located in the same directory as this file.
#'     You can do so by editing the distributed file directly, or by
#'     copying over it a `site_info.R` file from a prior request
#'     executed against the same CDM version; there is no data
#'     specific to this request in `site_info.R`.
#'  3. Edit the 3 variables below to provide information about your
#'     execution of this request.
#'  4. Execute this file.
#'  5. Return the contents of the `results` directory to the PEDSnet
#'     DCC.

#' Path to the top-level directory of this request package
#' @md
#'
#' This path should point to the top-level directory of the unzipped
#' data request package.  It will be the parent directory of the
#' directory containing this file.  You can replace the value below
#' with a simple path string.  If you don't the program will make a
#' guess about where the package is located, and will stop with an
#' error if it can't.
base_dir <- '/path/to/this/request'

if (base_dir == '/path/to/this/request')
    base_dir <-
        {
            if (! is.na(Sys.getenv('PEDSNET_DATA_REQUEST_ROOT', unset = NA))) {
                file.path(Sys.getenv('PEDSNET_DATA_REQUEST_ROOT'), 'OBrien_vWD')
            }
            else {
                if (file.exists('run.R')) {
                    file.path(getwd(), '..')
                }
                else {
                    if (file.exists(file.path('site', 'run.R'))) {
                        getwd()
                    }
                    else {
                        stop("Can't find package base directory; ",
                             "set base_dir in site/run.R")
                    }
                }
            }
        }


#' Local database configuration information
#' @md
#'
#' The contents of this file describe how to connect to the CDM
#' database to be used for this data request.  Since the information
#' doesn't change for a given version of the CDM, you may elect to use
#' a single copy located elsewhere for multiple data requests.  You
#' can also choose to edit or copy over the version provided with the
#' request package.
site_info <- file.path(base_dir, 'site', 'site_info.R')

#' Request-specific retention of intermediate tables
#' @md
#'
#' This Boolean value can be used to override the `default_retain_intermediates`
#' setting from site_info.R.  If it is NA, the default is not overridden.
config('retain_intermediates', NA)

#' Request-specific results schema
#' @md
#'
#' Specific schema to which you want intermediate tables to be written
#' during processing of the request.
#' If it is `NA`, the default value from site_info.R is used.
config('results_schema', NA)

#################################################################
#
# End of site-specific code.  Please do not edit below this point
#
#################################################################


source(file.path(base_dir, 'code', 'driver.R'))

if ( is.na(Sys.getenv('PEDSNET_SKIP_REQUEST_EXECUTION', unset = NA)) )
    run_request(base_dir, site_info, retain_intermediates, results_schema)
