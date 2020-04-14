
<!-- README.md is generated from README.Rmd. Please edit that file -->

# googleGroupR <img src='man/figures/logo.png' align="right" height="138.5" />

An R Wrapper for Google Groups Management using the AdminSDK.

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of googleGroupR is to make it easier to programmatically
interact with the Google AdminSDK from within R for Group management
activities including:

  - Creating/Deleting groups
  - Listing groups and group members
  - Adding/Removing users from groups

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thewileylab/googleGroupR")
```

## Example

googleGroupR can be used to list any groups that have been created by
your domain by utilizing the Google AdminSDK REST API:

``` r
library(googleGroupR)
list_groups(domain = 'your_gsuite_domain.org')
```

## Code of Conduct

Please note that the googleGroupR project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
