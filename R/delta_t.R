#' Return the value of delta-T in units of seconds
#'
#' @description
#' Converting the Coordinated Universal Time (UTC) to the Ephemeris Time (ET) is complex.
#' It is due to the unpredictable nature of the Earth's rotation, which is the basis for
#' UTC, whereas ET was based on the more uniform orbital motion of the Earth around the
#' Sun. The key to converting between these time scales lies in a value known as delta-T,
#' which is the difference between a uniform time scale and one based on
#' Earth's rotation. The conversion is handled as:
#' ET = UTC + deltaT
#' However, delta-T is not a constant value and cannot be calculated using a simple
#' formula. The delta-T values are derived from the historical records and from direct
#' observations. A series of polynomial expressions have been created to simplify the
#' evaluation of delta-T. The calculated values are valid for the years from -1999 to +3000.
#'
#' @details
#' More details:
#' Morrison, L. and Stephenson, F. R., "Historical Values of the Earth's Clock Error delta-T
#' and the Calculation of Eclipses", J. Hist. Astron., Vol. 35 Part 3, August 2004,
#' No. 120, pp 327-336 (2004)
#' Stephenson F.R., Historical Eclipses and Earth's Rotation, Cambridge Univ. Press, 1997
#'
#' @param year Type in the year (integer between -1999 and 3000).
#' @param month Type in the month (integer between 1 and 12).
#'
#' @returns
#' `numeric`: vector of numeric values
#'
#' @export
#'
#' @examples
#' delta_t(1999, 10)
#' delta_t(c(-200, 1610, 2030), c(1, 10, 12))


delta_t <- function(year, month) {
    # Evaluation of delta T for any interval years -1999 to 3000.
    # The result is given in seconds.
    # 'year' and 'month' may be vectors.

    if (!is.numeric(year) || !is.numeric(month)) {
        stop("Both 'year' and 'month' must be numeric.")
    }
    if (length(year) != length(month)) {
        stop("'year' and 'month' must have the same length.")
    }
    if (any(year < -1999 | year > 3000)) {
        stop("Year must be between -1999 and 3000.")
    }
    if (any(month < 1 | month > 12)) {
        stop("'month' must be between 1 and 12.")
    }
    if (any(year %% 1 != 0)) {
        stop("'year' must be integer.")
    }
    if (any(month %% 1 != 0)) {
        stop("'month' must be integer.")
    }

    y <- year + (month - 0.5) / 12
    deltat <- numeric(length(y))
    for (i in seq_along(y)) {
        yi <- y[i]
        if (yi < -500) {
            deltat[i] <- -20 + 32 * ((yi - 1820) / 100)^2
        } else if (yi < 500) {
            deltat[i] <- 10583.6 - 1014.41 * (yi / 100) + 33.78311 * (yi / 100)^2 -
                5.952053 * (yi / 100)^3 - 0.1798452 * (yi / 100)^4 +
                0.022174192 * (yi / 100)^5 + 0.0090316521 * (yi / 100)^6
        } else if (yi < 1600) {
            deltat[i] <- 1574.2 - 556.01 * ((yi - 1000) / 100) +
                71.23472 * ((yi - 1000) / 100)^2 + 0.319781 * ((yi - 1000) / 100)^3 -
                0.8503463 * ((yi - 1000) / 100)^4 - 0.005050998 * ((yi - 1000) / 100)^5 +
                0.0083572073 * ((yi - 1000) / 100)^6
        } else if (yi < 1700) {
            deltat[i] <- 120 - 0.9808 * (yi - 1600) - 0.01532 * (yi - 1600)^2 +
                (yi - 1600)^3 / 7129
        } else if (yi < 1800) {
            deltat[i] <- 8.83 + 0.1603 * (yi - 1700) - 0.0059285 * (yi - 1700)^2 +
                0.00013336 * (yi - 1700)^3 - (yi - 1700)^4 / 1174000
        } else if (yi < 1860) {
            deltat[i] <- 13.72 - 0.332447 * (yi - 1800) + 0.0068612 * (yi - 1800)^2 +
                0.0041116 * (yi - 1800)^3 - 0.00037436 * (yi - 1800)^4 +
                0.0000121272 * (yi - 1800)^5 - 0.0000001699 * (yi - 1800)^6 +
                0.000000000875 * (yi - 1800)^7
        } else if (yi < 1900) {
            deltat[i] <- 7.62 + 0.5737 * (yi - 1860) - 0.251754 * (yi - 1860)^2 +
                0.01680668 * (yi - 1860)^3 - 0.0004473624 * (yi - 1860)^4 +
                (yi - 1860)^5 / 233174
        } else if (yi < 1920) {
            deltat[i] <- -2.79 + 1.494119 * (yi - 1900) - 0.0598939 * (yi - 1900)^2 +
                0.0061966 * (yi - 1900)^3 - 0.000197 * (yi - 1900)^4
        } else if (yi < 1941) {
            deltat[i] <- 21.20 + 0.84493 * (yi - 1920) - 0.076100 * (yi - 1920)^2 +
                0.0020936 * (yi - 1920)^3
        } else if (yi < 1961) {
            deltat[i] <- 29.07 + 0.407 * (yi - 1950) - (yi - 1950)^2 / 233 +
                (yi - 1950)^3 / 2547
        } else if (yi < 1986) {
            deltat[i] <- 45.45 + 1.067 * (yi - 1975) - (yi - 1975)^2 / 260 -
                (yi - 1975)^3 / 718
        } else if (yi < 2005) {
            deltat[i] <- 63.86 + 0.3345 * (yi - 2000) - 0.060374 * (yi - 2000)^2 +
                0.0017275 * (yi - 2000)^3 + 0.000651814 * (yi - 2000)^4 +
                0.00002373599 * (yi - 2000)^5
        } else if (yi < 2050) {
            deltat[i] <- 62.92 + 0.32217 * (yi - 2000) + 0.005589 * (yi - 2000)^2
        } else if (yi < 2150) {
            deltat[i] <- -20 + 32 * ((yi - 1820) / 100)^2 - 0.5628 * (2150 - yi)
        } else {
            deltat[i] <- -20 + 32 * ((yi - 1820) / 100)^2
        }
    }
    deltat
}
