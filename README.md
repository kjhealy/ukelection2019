<!-- README.md is generated from README.Rmd. Please edit that file -->



# UK Election 2019 Vote Results

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/kjhealy/ukelection2019.svg?branch=master)](https://travis-ci.org/kjhealy/ukelection2019)
<!-- badges: end -->

A data package scraped from the BBC Election webpage.


## Installation

There are two ways to install the package.

### Install direct from GitHub

You can install the beta version of ukelection2019 from [GitHub](https://github.com/kjhealy/ukelection2019) with:

``` r
devtools::install_github("kjhealy/ukelection2019")
```

### Installation using `drat`

While using `install_github()` works just fine, it would be nicer to be able to just type `install.packages("ukelection2019")` or `update.packages("ukelection2019")` in the ordinary way. We can do this using Dirk Eddelbuettel's [drat](http://eddelbuettel.github.io/drat/DratForPackageUsers.html) package. Drat provides a convenient way to make R aware of package repositories other than CRAN.

First, install `drat`:


```r
if (!require("drat")) {
    install.packages("drat")
    library("drat")
}
```

Then use `drat` to tell R about the repository where `ukelection2019` is hosted:


```r
drat::addRepo("kjhealy")
```

You can now install `ukelection2019`:


```r
install.packages("ukelection2019")
```

To ensure that the `ukelection2019` repository is always available, you can add the following line to your `.Rprofile` or `.Rprofile.site` file:


```r
drat::addRepo("kjhealy")
```

With that in place you'll be able to do `install.packages("ukelection2019")` or `update.packages("ukelection2019")` and have everything work as you'd expect. 

Note that the drat repository only contains data packages that are not on CRAN, so you will never be in danger of grabbing the wrong version of any other package.


## Loading the data

The package works best with the [tidyverse](http://tidyverse.org/) libraries and the [simple features](https://r-spatial.github.io/sf/index.html) package for mapping.


```r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
#> ✔ tibble  2.1.3     ✔ dplyr   0.8.3
#> ✔ tidyr   1.0.0     ✔ stringr 1.4.0
#> ✔ readr   1.3.1     ✔ forcats 0.4.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ purrr::is_null() masks testthat::is_null()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ dplyr::matches() masks tidyr::matches(), testthat::matches()
```

Load the data:


```r
library(ukelection2019)
```

Look at it:


```r
ukvote2019
#> # A tibble: 3,320 x 13
#> # Groups:   candidate [3,300]
#>    cid   constituency electorate party_name candidate votes vote_share_perc…
#>    <chr> <chr>             <int> <chr>      <chr>     <int>            <dbl>
#>  1 W070… Aberavon          50747 Labour     Stephen … 17008             53.8
#>  2 W070… Aberavon          50747 Conservat… Charlott…  6518             20.6
#>  3 W070… Aberavon          50747 The Brexi… Glenda D…  3108              9.8
#>  4 W070… Aberavon          50747 Plaid Cym… Nigel Hu…  2711              8.6
#>  5 W070… Aberavon          50747 Liberal D… Sheila K…  1072              3.4
#>  6 W070… Aberavon          50747 Independe… Captain …   731              2.3
#>  7 W070… Aberavon          50747 Green      Giorgia …   450              1.4
#>  8 W070… Aberconwy         44699 Conservat… Robin Mi… 14687             46.1
#>  9 W070… Aberconwy         44699 Labour     Emily Ow… 12653             39.7
#> 10 W070… Aberconwy         44699 Plaid Cym… Lisa Goo…  2704              8.5
#> # … with 3,310 more rows, and 6 more variables: vote_share_change <dbl>,
#> #   total_votes_cast <int>, vrank <int>, turnout <dbl>, fname <chr>,
#> #   lname <chr>
```
