# baaugwo

The goal of baaugwo is to archive an R package from CRAN into either a tibble or a SQLite table.

baaugwo (包裹) is the cantonese pronounciation of the word 'package'.

## Installation


``` r
install_github("chainsawriot/baaugwo")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
baaugwo::fetch_pkg_src('rio')
```
