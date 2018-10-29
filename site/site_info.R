#!/usr/bin/env Rscript

#' Site-specific information for data request execution.
#'
#' Please edit information as noted in lines beginning with `#>`
#' below.

#> Please alter to reflect your site name, one of 'DCC', 'BCH', or 'CCHMC'
config('site', 'DCC')

#> Please replace with code to establish a database connection at your
#> site. The connection must be able to reach CDM data, the
#> vocabularies, and any result schemata needed.  The connection may be
#> either a dplyr-style src_foo() object (as below), or a DBI-style
#> dbConnect() object (e.g. dbConnect(MySQL(), host = ...)).
config('db_src',
       { require(Argos);
         src_argos('argos_pedsnet_v26')
       })

#> Name of the schema, if any, to be prepended to CDM fact table names.
#> If `NA`, no schema qualifier is added.
config('cdm_schema', 'dcc_pedsnet')

#> Name of the schema, if any, to be prepended to vocabulary tables
#> If `NA`, no schema qualifier is added.
config('vocabulary_schema', 'vocabulary')

#' Schema in which to create intermediate and results tables
#' @md
#'
#' This value determines whether a schema name is added to names of
#' tables holding intermediate or final results.  If it is `NA`, no
#' explicit schema is used, and tables are created wherever the DBMS
#' places them.
config('default_results_schema', NA)

#' Whether or not to keep intermediate tables
#' @md
#'
#' This Boolean value determines whether tables holding codesets
#' or intermediate steps are retained after execution completes.  If
#' `FALSE`, they are created as temporary tables.
config('default_retain_intermediates', TRUE)

#' Execution mode
#' @md
#'
#' The intended execution mode for the reuqest.  Recognized values are:
#'
#' * `production` - Intended for results generation and distribution.
#' * `development` - Development still in progress
config('execution_mode', 'development')

#> Names of standard tables used in queries.  Please edit only the
#> right-hand side of each assignment.  Table names must be lower-case.
config('table_names',
       list(adt_occurrence = 'adt_occurrence',
            care_site = 'care_site',
            condition_occurrence = 'condition_occurrence',
            death = 'death',
            dose_era = 'dose_era',
            drug_era = 'drug_era',
            drug_exposure = 'drug_exposure',
            fact_relationship = 'fact_relationship',
            location = 'location',
            measurement = 'measurement',
            measurement_organism = 'measurement_organism',
            observation = 'observation',
            observation_period = 'observation_period',
            person = 'person',
            procedure_occurrence = 'procedure_occurrence',
            provider = 'provider',
            visit_occurrence = 'visit_occurrence',
            visit_payer = 'visit_payer',
            concept = 'concept',
            concept_ancestor = 'concept_ancestor',
            concept_relationship = 'concept_relationship'))

#> ##################### End of site-specific configuration
