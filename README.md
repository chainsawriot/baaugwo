# baaugwo

The goal of baaugwo is to archive an R package from CRAN into either a tibble or a SQLite table.

baaugwo (包裹) is the cantonese pronounciation of the word 'package'.

The current version read only the DESCRIPTION, NAMESPACE, and all .R files in the /R directory. 

## Installation

``` r
install_github("chainsawriot/baaugwo")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
baaugwo::read_cranpkg('rio')
```
