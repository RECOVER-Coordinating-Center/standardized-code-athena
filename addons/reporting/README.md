## Standard package addons

### Reporting addons

This directory contains code or templates that are applicable to to
visualization or summarization of results from a data request in some, but not
most, queries.

### Using addon files

Each addon file begins with a comment bracketed by `### Addon` lines that
describes the purpose of the addon, and provides any specific instructions for
setup. 

To include a file for reporting,

1. copy it to the `reporting` directory (or other location as specified in the
   file),
1. make any modification to it or to other files as specified in the
file-specific instructions, and
1. execute it as an R markdown document, or other file type, as appropriate.
