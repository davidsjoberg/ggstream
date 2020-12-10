
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggstream

<!-- badges: start -->

<!-- badges: end -->

The goal of `ggstream` is to create a simple but powerful implementation
of streamplot/streamgraph in `ggplot2`. A streamplot is a stacked area
plot mostly used for time series.

This is a development, expect breaking changes before submission to
CRAN.

## Installation

You can install the development version of ggstream from github with:

``` r
remotes::install_github("davidsjoberg/ggstream")
```

## Examples

The characteristic streamplot which creates a symmetrical area chart
around the x axis.

<img src="man/figures/README-pressure-1.png" width="100%" />

Which is equivalent to a stacked area chart.

<img src="man/figures/README-pressure2-1.png" width="100%" />

## Basic usage

This is a basic example:

``` r
library(ggstream)

ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream()
```

<img src="man/figures/README-example-1.png" width="100%" />

`ggstream` also features a custom labeling geom that places decent
default labels.

``` r
ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream() +
  geom_stream_label(aes(label = genre))
```

<img src="man/figures/README-cars-1.png" width="100%" />
