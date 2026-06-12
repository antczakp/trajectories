
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{trajectories}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{trajectories}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
trajectories::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-06-12 11:20:45 CEST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading trajectories
#> ── R CMD check results ──────────────────────────── trajectories 0.0.0.9000 ────
#> Duration: 34s
#> 
#> ❯ checking tests ...
#>   See below...
#> 
#> ❯ checking code files for non-ASCII characters ... WARNING
#>   Found the following files with non-ASCII characters:
#>     R/app_server.R
#>     R/app_ui.R
#>     R/fct_helpers.R
#>   Portable packages must use only ASCII characters in their R code and
#>   NAMESPACE directives, except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#>   Function 'tools::showNonASCIIfile' can help in finding non-ASCII
#>   characters in files.
#> 
#> ❯ checking DESCRIPTION meta-information ... NOTE
#>   Malformed Description field: should contain one or more complete sentences.
#> 
#> ❯ checking package subdirectories ... NOTE
#>   Problems with news in 'NEWS.md':
#>   No news entries found.
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   app_server: no visible binding for global variable 'type'
#>   app_server: no visible binding for global variable 'day'
#>   app_server: no visible binding for global variable 'organ'
#>   app_server: no visible binding for '<<-' assignment to '.df'
#>   compute_median_expression: no visible binding for global variable 'id'
#>   compute_median_expression: no visible binding for global variable
#>     'type'
#>   compute_median_expression: no visible binding for global variable 'day'
#>   compute_median_expression: no visible binding for global variable
#>     'datatype'
#>   compute_stats_expression: no visible binding for global variable 'id'
#>   compute_stats_expression: no visible binding for global variable 'day'
#>   compute_stats_expression: no visible binding for global variable 'type'
#>   compute_stats_expression: no visible binding for global variable
#>     'datatype'
#>   safe_stat: no visible global function definition for 't.test'
#>   Undefined global functions or variables:
#>     datatype day id organ t.test type
#>   Consider adding
#>     importFrom("stats", "t.test")
#>   to your NAMESPACE file.
#> 
#> ── Test failures ───────────────────────────────────────────────── testthat ────
#> 
#> > # This file is part of the standard setup for testthat.
#> > # It is recommended that you do not modify it.
#> > #
#> > # Where should you do additional test configuration?
#> > # Learn more about the roles of various files in:
#> > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#> > # * https://testthat.r-lib.org/articles/special-files.html
#> > 
#> > library(testthat)
#> > library(trajectories)
#> > 
#> > test_check("trajectories")
#> Saving _problems/test-golem_utils_ui-99.R
#> Saving _problems/test-golem_utils_ui-105.R
#> [ FAIL 2 | WARN 0 | SKIP 0 | PASS 74 ]
#> 
#> ══ Failed tests ════════════════════════════════════════════════════════════════
#> ── Failure ('test-golem_utils_ui.R:96:3'): Test undisplay works ────────────────
#> Expected `as.character(b)` to equal "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\">go</button>".
#> Differences:
#> actual vs expected
#> - "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\"><span class=\"action-label\">go</span></button>"
#> + "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\">go</button>"
#> 
#> ── Failure ('test-golem_utils_ui.R:102:3'): Test undisplay works ───────────────
#> Expected `as.character(b_undisplay)` to equal "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">go</button>".
#> Differences:
#> actual vs expected
#> - "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\"><span class=\"action-label\">go</span></button>"
#> + "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">go</button>"
#> 
#> 
#> [ FAIL 2 | WARN 0 | SKIP 0 | PASS 74 ]
#> Error:
#> ! Test failures.
#> Execution halted
#> 
#> 1 error ✖ | 1 warning ✖ | 3 notes ✖
#> Error:
#> ! R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in `loadNamespace()`:
#> ! there is no package called 'covr'
```
