## Standard package addons

The standard request framework is constructed to simplify the process of data request execution and decrease the amount of boilerplate and repetition needed in writing query or analytic code. In some cases, reusable code will apply to only a subset of requests. In order to reduce the compilatiom overhead for requests that do not need this extra capacity, this type of code is made available through a mechanism called "addons".

An addon is conceptually simple: it is just a file containing functions that integrate with the package to perform a task.  For convenience, addons are divided into two groups.  Those in the `request` directory apply primarily to the execution of queries or analyses.  Thos in the `reporting` directory apply primarily to visualization or summarization of data.  This is not a bright line, so if you don't find what you want in one place, please check the other.  And, of course, if you write something you believe is reusable, please submit it as an addon.

Each addon file must begin with a comment bracketed by `### Addon` lines that
describes the purpose of the addon, and provides any specific instructions for
setup.   Unless this specifies otherwise, addons in the `request` directory should be copied into `code` for use, and their names should begin with `util_`(for addons that provide utility functions), `cohort_` (for addons that create parts of cohorts), or `analyze_` (for addons that perform portions of an analysis), while addons in the `reporting` directory should be copied to the main `reporting` directory for use.
