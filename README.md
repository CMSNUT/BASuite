
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{BASuite}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{BASuite}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
BASuite::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-01-31 23:26:20 CST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading BASuite
#> Warning: replacing previous import 'autoReg::df2flextable' by
#> 'rrtable::df2flextable' when loading 'BASuite'
#> Warning: replacing previous import 'DT::dataTableOutput' by
#> 'shiny::dataTableOutput' when loading 'BASuite'
#> Warning: replacing previous import 'DT::renderDataTable' by
#> 'shiny::renderDataTable' when loading 'BASuite'
#> Warning: replacing previous import 'shinydashboard::taskItem' by
#> 'shinydashboardPlus::taskItem' when loading 'BASuite'
#> Warning: replacing previous import 'shinydashboard::dashboardHeader' by
#> 'shinydashboardPlus::dashboardHeader' when loading 'BASuite'
#> Warning: replacing previous import 'shinydashboard::box' by
#> 'shinydashboardPlus::box' when loading 'BASuite'
#> Warning: replacing previous import 'shinydashboard::messageItem' by
#> 'shinydashboardPlus::messageItem' when loading 'BASuite'
#> Warning: replacing previous import 'shinydashboard::dashboardSidebar' by
#> 'shinydashboardPlus::dashboardSidebar' when loading 'BASuite'
#> Warning: replacing previous import 'shinydashboard::dashboardPage' by
#> 'shinydashboardPlus::dashboardPage' when loading 'BASuite'
#> Warning: replacing previous import 'shinyWidgets::progressBar' by
#> 'shinydashboardPlus::progressBar' when loading 'BASuite'
#> Warning: replacing previous import 'shinydashboard::notificationItem' by
#> 'shinydashboardPlus::notificationItem' when loading 'BASuite'
#> Warning: replacing previous import 'shinyWidgets::alert' by 'shinyjs::alert'
#> when loading 'BASuite'
#> Warning: replacing previous import 'shiny::runExample' by 'shinyjs::runExample'
#> when loading 'BASuite'
#> ── R CMD check results ───────────────────────────────── BASuite 0.0.0.9000 ────
#> Duration: 1m 23.9s
#> 
#> ❯ checking whether package 'BASuite' can be installed ... [10s] WARNING
#>   See below...
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> 0 errors ✔ | 1 warning ✖ | 1 note ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
