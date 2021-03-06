
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/fmichonneau/checker.svg?branch=master)](https://travis-ci.org/fmichonneau/checker)
[![Codecov test
coverage](https://codecov.io/gh/fmichonneau/checker/branch/master/graph/badge.svg)](https://codecov.io/gh/fmichonneau/checker?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# checker

The goal of checker is to check whether the links (including images,
CSS, …) included in your documents are valid.

**This is an early draft. Things can change. Only HTML files are
currently supported.**

## Installation

You can install `checker` with:

``` r
remotes::install_github("fmichonneau/checker")
```

## Basic usage

If you generated a website from Rmarkdown in a `_site` folder within
your working directory, you can run

``` r
library(checker)
check_links(dir = "_site")
```

## Code of Conduct

Please note that the `checker` project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
