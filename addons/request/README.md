## Standard package addons

### Request addons

This directory contains code that is applicable to request processing for some,
but not most, queries.

### Using addon files

Each addon file begins with a comment bracketed by `### Addon` lines that
describes the purpose of the addon, and provides any specific instructions for
setup. 

To include a file in a data request,

1. copy it to the `code` directory (or other location as specified in the file),
1. make any modification to it or to other files as specified in the 
   file-specific instructions, and
1. in the uncommon event that the name of the file doesn't start with `util_`,
   `cohort_`, or `analyze_`, make sure that `driver.R` sources it.


