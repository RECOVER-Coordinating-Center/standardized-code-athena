#!/usr/bin/env Rscript

if (!exists('req_env')) req_env <- new.env(parent = emptyenv())

#' Retrieve an item of site-specific configuration
#'
#' @param label The name of the configuration datum
#' @param ... Optionally, a value to which to set the configuration
#'     datum.  Only a single value is read; the `...` formalism is
#'     used to make it possible to detect when a value is present.
#'
#' @return The (possibly new) configuration value, or `NULL` if it
#'     does not exist.
config <- function(label, ...) {
    dots <- list(...)
    if (length(dots) > 0)
        assign(label, dots[[1]], pos = req_env)
    get0(label, envir = req_env)
}
