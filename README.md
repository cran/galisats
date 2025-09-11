
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galisats <a href="https://lechjaszowski.github.io/galilean_satellites/"><img src="man/figures/logo.png" alt="galisats website" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/LechJaszowski/galilean_satellites/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LechJaszowski/galilean_satellites/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

If you are looking at Jupiter through binoculars or a telescope and
don’t know which moon is which, then use this package.

`galisats` is used to determine the positions of the four greatest
satellites of Jupiter (called Galilean satellites). Positions are shown
on the plot for any given time (UTC – Coordinated Universal Time) with
respect to the planet, as seen from the Earth.

The `galsat()` function calculates numerical values of the satellites’
positions:

x – the apparent rectangular coordinate of the satellite with respect to
the center of Jupiter’s disk in the equatorial plane in the units of
Jupiter’s equatorial radius; X is positive toward the west

y – the apparent rectangular coordinate of the satellite with respect to
the center of Jupiter’s disk from the equatorial plane in the units of
Jupiter’s equatorial radius; Y is positive toward the north

The function is based on algorithms in the book:

Astronomical Formulae for Calculators (4th edition), Jean Meeus,
Willmann-Bell Inc., 1988

The `delta_t()` function returns the value of delta-T in seconds unit.
It’s useful for converting the Coordinated Universal Time (UTC) to the
Ephemeris Time (ET). The conversion is handled as: ET = UTC + deltaT

## Installation

You can install the development version of galisats from \[GitHub\]
(<https://github.com/>) with:

``` r
# install.packages("devtools")
devtools::install_github("LechJaszowski/galilean_satellites")
```

## Example

There are examples of using `galsat()` and `delta_t()` functions:

``` r
library(galisats)
galsat(2025, 10, 13, 21, 40)
```

<img src="man/figures/README-example-1.png" width="100%" />

    #>       moon          x          y
    #> 1       Io   4.089507  0.1125276
    #> 2   Europa  -8.417919  0.1080519
    #> 3 Ganymede   6.644450 -0.3518624
    #> 4 Callisto -24.735531  0.2536266
    delta_t(1999, 10)
    #> [1] 63.78768
    delta_t(c(-200, 1610, 2030), c(1, 10, 12))
    #> [1] 12791.65348   107.80766    78.25045
