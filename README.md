
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `SomaPCA` package

<!-- badges: start -->

[![build](https://img.shields.io/badge/build-passing-success.svg?logo=travis)](http://bitbucket.sladmin.com/projects/SV/repos/somapca/commits)
![coverage](https://img.shields.io/badge/coverage-79.2%25-success.svg?style=flat&logo=codecov)
![lint](https://img.shields.io/badge/lints-0-success.svg?style=flat&logo=gitlab)
[![pkgdown](https://img.shields.io/badge/pkgdown-_-critical.svg?logo=semantic-web&logoColor=red)](https://bitbucket.sladmin.com/pages/SV/somapca/bb-pkgdown/browse/index.html)
[![License:
GPL-3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

The `SomaPCA` package contains the general functions necessary for the
unsupervised exploratory analysis of SOMAmer reagent data using
Principal Component Analysis (PCA). The primary tools in this package
are dedicated to either the calculation of principal components, or the
visualization of loadings and projections of those components.

For cross-referenced help, running examples, and vignettes, please visit
the
[pkgdown](https://bitbucket.sladmin.com/pages/SV/somapca/bb-pkgdown/browse/index.html)
website.

------------------------------------------------------------------------

## Custom Installation

It is possible to install a specific version of `SomaPCA` (i.e. one
differing from your current installation of
[SLIDE](http://bitbucket.sladmin.com/projects/BFX/repos/slide/browse)),
but it comes with a possible trade-off in the reproducibility of your
work! So be *careful* and considerate of those who may follow you. The
method below ensures your installation is both traceable and
reproducible:

``` r
somaverse::install_sl_bitbucket("SomaPCA", "commit-SHA", "USERNAME")
```

------------------------------------------------------------------------

## Usage

To load `SomaPCA` simply make a call to `library()` as usual:

``` r
library(SomaPCA)
```

## Help summary of the package

``` r
library(help = SomaPCA)
```

------------------------------------------------------------------------

#### LICENSE

Please note that this SomaLogic, Inc. internal package is released with
a [LICENSE](LICENSE). By using in this package you agree to abide by its
terms.
