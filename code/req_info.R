#!/usr/bin/env Rscript

#' Administrative information about request. 
#' The first @par@param should remain the same throughout requests. 
#' Request-specific information should be the second @par@param in 
#' the config() functions.
#' 

config('title',
       'Title of Project')
config('requester_name', 'Name of Requestor')
config('requester_site', 'Site Name')
config('cdm', 'cdm_used')
config('cdm_version', x.x)
config('enqueued_date', as.Date(''))

config('req_version', x.x)

# In most cases, these assignments should not need to be changed.
# But if for some reason you have altered the layout of the request
# package, you will need to edit these accordingly:
#
#  * code_dir = location of code for request
#  * spec_dir = location of data files needed to execute the request
#  * local_dir = location to which intermediate results may be saved
#    for local inspection
#  * result_dir = location to which results to be returned to the DCC
#    will be written
#
# N.B.  If you change code_dir, you must either source this file
# prior to util.R (the usual case), or edit util.R and driver.R to
# reflect the change.
config('subdirs',
       list(code_dir = 'code',
            site_dir = 'site',
            spec_dir = 'specs',
            local_dir = 'local',
            result_dir = 'results'))
