radians <- function(degree) {
    degree * pi / 180
}

degrees <- function(radian) {
    radian * 180 / pi
}

#' Calculate & draw the positions of the Galilean satellites
#'
#' @description
#' `galsat()` is used to determine the positions of the four greatest satellites
#' of Jupiter (called Galilean satellites). Positions are shown on the plot for
#' given UTC time (Coordinated Universal Time between year 0 and 3000) with respect
#' to the planet, as seen from the Earth.
#'
#' The `galsat()` function returns numerical values of the satellites' positions:
#'
#'   x - the apparent rectangular coordinate of the satellite with respect to the
#' center of Jupiter's disk in the equatorial plane in the units of Jupiter's
#' equatorial radius; X is positive toward the west
#'
#'   y - the apparent rectangular coordinate of the satellite with respect to the
#' center of Jupiter's disk from the equatorial plane in the units of Jupiter's
#' equatorial radius; Y is positive toward the north
#'
#' @details
#' The function is based on algorithms in the book:
#' Astronomical Formulae for Calculators (4th edition), Jean Meeus, Willmann-Bell Inc., 1988
#'
#' @param year Type in the year (integer number from 0 to 3000).
#' @param month Type in the month (integer number from 1 to 12).
#' @param day Type in the day (integer number from 1 to 31).
#' @param hour Type in the hour (integer number from 0 to 23).
#' @param minute Type in the minute (integer number from 0 to 59).
#'
#' @returns
#' `data.frame`: 4 observations of 3 variables:
#' $ moon: chr "Io" "Europa" "Ganymede" "Callisto"
#' $ x   : num
#' $ y   : num
#' Four rows - each row has the position (x,y) of one moon.
#' Additionally, the positions of the moons are shown graphically.
#'
#' @importFrom png readPNG
#'
#' @export
#'
#' @examples
#' galsat(2025, 10, 13, 23, 30)

galsat <- function(year, month, day, hour, minute) {

    # date & time validation
    date_string <- sprintf("%04d-%02d-%02d %02d:%02d", year, month, day, hour, minute)
    date_UTC <- as.POSIXct(date_string, format = "%Y-%m-%d %H:%M", tz = "UTC") # NA if invalid
    if (is.na(date_UTC)) {stop("Invalid date or time provided.")}

    # calculate delta-T (in seconds) for any year between 0 to 3000
    if (year < 0 || year > 3000) {
        stop("Year must be between 0 and 3000.")
    }
    if (year < 500) {
        deltat <- 10583.6 - 1014.41 * (year / 100) + 33.78311 * (year / 100)^2 -
            5.952053 * (year / 100)^3 - 0.1798452 * (year / 100)^4 +
            0.022174192 * (year / 100)^5 + 0.0090316521 * (year / 100)^6
    } else if (year < 1600) {
        deltat <- 1574.2 - 556.01 * ((year - 1000) / 100) +
            71.23472 * ((year - 1000) / 100)^2 + 0.319781 * ((year - 1000) / 100)^3 -
            0.8503463 * ((year - 1000) / 100)^4 - 0.005050998 * ((year - 1000) / 100)^5 +
            0.0083572073 * ((year - 1000) / 100)^6
    } else if (year < 1700) {
        deltat <- 120 - 0.9808 * (year - 1600) - 0.01532 * (year - 1600)^2 +
            (year - 1600)^3 / 7129
    } else if (year < 1800) {
        deltat <- 8.83 + 0.1603 * (year - 1700) - 0.0059285 * (year - 1700)^2 +
            0.00013336 * (year - 1700)^3 - (year - 1700)^4 / 1174000
    } else if (year < 1860) {
        deltat <- 13.72 - 0.332447 * (year - 1800) + 0.0068612 * (year - 1800)^2 +
            0.0041116 * (year - 1800)^3 - 0.00037436 * (year - 1800)^4 +
            0.0000121272 * (year - 1800)^5 - 0.0000001699 * (year - 1800)^6 +
            0.000000000875 * (year - 1800)^7
    } else if (year < 1900) {
        deltat <- 7.62 + 0.5737 * (year - 1860) - 0.251754 * (year - 1860)^2 +
            0.01680668 * (year - 1860)^3 - 0.0004473624 * (year - 1860)^4 +
            (year - 1860)^5 / 233174
    } else if (year < 1920) {
        deltat <- -2.79 + 1.494119 * (year - 1900) - 0.0598939 * (year - 1900)^2 +
            0.0061966 * (year - 1900)^3 - 0.000197 * (year - 1900)^4
    } else if (year < 1941) {
        deltat <- 21.20 + 0.84493 * (year - 1920) - 0.076100 * (year - 1920)^2 +
            0.0020936 * (year - 1920)^3
    } else if (year < 1961) {
        deltat <- 29.07 + 0.407 * (year - 1950) - (year - 1950)^2 / 233 +
            (year - 1950)^3 / 2547
    } else if (year < 1986) {
        deltat <- 45.45 + 1.067 * (year - 1975) - (year - 1975)^2 / 260 -
            (year - 1975)^3 / 718
    } else if (year < 2005) {
        deltat <- 63.86 + 0.3345 * (year - 2000) - 0.060374 * (year - 2000)^2 +
            0.0017275 * (year - 2000)^3 + 0.000651814 * (year - 2000)^4 +
            0.00002373599 * (year - 2000)^5
    } else if (year < 2050) {
        deltat <- 62.92 + 0.32217 * (year - 2000) + 0.005589 * (year - 2000)^2
    } else if (year < 2150) {
        deltat <- -20 + 32 * ((year - 1820) / 100)^2 - 0.5628 * (2150 - year)
    } else {
        deltat <- -20 + 32 * ((year - 1820) / 100)^2
    }

    # change UTC to ET
    date_ET <- date_UTC + deltat

    # extract components for ET
    yearET <- as.numeric(format(date_ET, "%Y"))
    monthET <- as.numeric(format(date_ET, "%m"))
    dayET <- as.numeric(format(date_ET, "%d"))
    hourET <- as.numeric(format(date_ET, "%H"))
    minuteET <- as.numeric(format(date_ET, "%M"))

    # calculations
    DDdd <- dayET + hourET / 24 + minuteET / 1440
    if (monthET >= 3) {y <- yearET; m <- monthET} else {y <- yearET - 1; m <- monthET + 12}
    JD <- floor(365.25 * y) + DDdd + 1720994.5 + floor(30.6001 * (m + 1))
    YYYYMMDDdd <- yearET + .01 * monthET + .0001 * DDdd
    if (YYYYMMDDdd >= 1582.1015) {B_c_aux <- floor(.01 * y);
    B_correction <- 2 - B_c_aux + floor(B_c_aux / 4);
    JD <- JD + B_correction}
    d <- JD - 2415020
    Vdeg <- 134.63 + .00111587 * d
    Mdeg <- 358.476 + .9856003 * d
    Ndeg <- 225.328 + .0830853 * d + .33 * sin(radians(Vdeg))
    Jdeg <- 221.647 + .9025179 * d - .33 * sin(radians(Vdeg))
    Adeg <- 1.916 * sin(radians(Mdeg)) + .02 * sin(radians(2 * Mdeg))
    Bdeg <- 5.552 * sin(radians(Ndeg)) + .167 * sin(radians(2 * Ndeg))
    K <- (Jdeg + Adeg - Bdeg) %% 360
    R_AU <- 1.00014 - .01672 * cos(radians(Mdeg)) - .00014 * cos(radians(2 * Mdeg))
    rj_AU <- 5.20867 - .25192 * cos(radians(Ndeg)) - .0061 * cos(radians(2 * Ndeg))
    delta_AU <- sqrt(R_AU ** 2 + rj_AU ** 2 - 2 * R_AU * rj_AU * cos(radians(K)))
    psi <- degrees(asin(R_AU * sin(radians(K)) / delta_AU))
    u1_deg <- 84.5506 + psi - Bdeg + 203.405863 * (d - delta_AU / 173)
    u2_deg <- 41.5015 + psi - Bdeg + 101.2916323 * (d - delta_AU / 173)
    u3_deg <- 109.977 + psi - Bdeg + 50.2345169 * (d - delta_AU / 173)
    u4_deg <- 176.3586 + psi - Bdeg + 21.4879802 * (d - delta_AU / 173)
    Gdeg <- 187.3 + 50.310674 * (d - delta_AU / 173)
    Hdeg <- 311.1 + 21.569229 * (d - delta_AU / 173)
    u1_correction <- .472 * sin(radians(2 * (u1_deg - u2_deg)))
    u2_correction <- 1.073 * sin(radians(2 * (u2_deg - u3_deg)))
    u3_correction <- .174 * sin(radians(Gdeg))
    u4_correction <- .845 * sin(radians(Hdeg))
    r1 <- 5.9061 - .0244 * cos(radians(2 * (u1_deg - u2_deg)))
    r2 <- 9.3972 - .0889 * cos(radians(2 * (u2_deg - u3_deg)))
    r3 <- 14.9894 - .0227 * cos(radians(Gdeg))
    r4 <- 26.3649 - .1944 * cos(radians(Hdeg))
    u1_corrected <- (u1_deg + u1_correction) %% 360
    u2_corrected <- (u2_deg + u2_correction) %% 360
    u3_corrected <- (u3_deg + u3_correction) %% 360
    u4_corrected <- (u4_deg + u4_correction) %% 360
    lambd <- 238.05 + .083091 * d + .33 * sin(radians(Vdeg)) + Bdeg
    De <- 3.07 * sin(radians(lambd + 44.5)) - 1.31 * (rj_AU - delta_AU) *
        sin(radians(lambd - 99.4)) / delta_AU - 2.15 * sin(radians(psi)) * cos(radians(lambd + 24))
    x1 <- r1 * sin(radians(u1_corrected))
    x2 <- r2 * sin(radians(u2_corrected))
    x3 <- r3 * sin(radians(u3_corrected))
    x4 <- r4 * sin(radians(u4_corrected))
    y1 <- -r1 * cos(radians(u1_corrected)) * sin(radians(De))
    y2 <- -r2 * cos(radians(u2_corrected)) * sin(radians(De))
    y3 <- -r3 * cos(radians(u3_corrected)) * sin(radians(De))
    y4 <- -r4 * cos(radians(u4_corrected)) * sin(radians(De))

    # resulting data structure (data frame)
    p <- data.frame(
        moon = c("Io", "Europa", "Ganymede", "Callisto"),
        x = c(x1, x2, x3, x4),
        y = c(y1, y2, y3, y4)
    )

    # inserting a title, date and time
    graphics::plot(c(-30, 30), c(-30, 30), type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
    graphics::text(0, 28, "SATELLITES OF JUPITER", col = "black", cex = 1.7, adj = 0.5)
    graphics::text(0, 23,
                   paste0('Date: ', year, '-', sprintf("%02d", month), '-', sprintf("%02d", day)),
                   col = "black", cex = 1.2, adj = 0.5)
    graphics::text(0, 18,
                   paste0('Time [UTC]: ', sprintf("%02d", hour), ':', sprintf("%02d", minute)),
                   col = "black", cex = 1.2, adj = 0.5)

    # inserting an image of Jupiter in the center of the plot
    jupiter <- png::readPNG(system.file("jupiter.png", package = "galisats"))
    graphics::rasterImage(jupiter, xleft = -1, ybottom = -1, xright = 1, ytop = 1)

    # drawing the moons with their labels
    if (sqrt(x1^2 + y1^2) > 1 | u1_corrected < 90 | u1_corrected > 270) {
        graphics::points(x1, y1, col = "red", pch = 20);
        graphics::text(x1, y1 + 3, "I", col = "red", cex = 0.8, adj = 0.5)
    }
    if (sqrt(x2^2 + y2^2) > 1 | u2_corrected < 90 | u2_corrected > 270) {
        graphics::points(x2, y2, col = "blue", pch = 20);
        graphics::text(x2, y2 + 3, "E", col = "blue", cex = 0.8, adj = 0.5)
    }
    if (sqrt(x3^2 + y3^2) > 1 | u3_corrected < 90 | u3_corrected > 270) {
        graphics::points(x3, y3, col = "green", pch = 20);
        graphics::text(x3, y3 + 3, "G", col = "green", cex = 0.8, adj = 0.5)
    }
    if (sqrt(x4^2 + y4^2) > 1 | u4_corrected < 90 | u4_corrected > 270) {
        graphics::points(x4, y4, col = "magenta", pch = 20);
        graphics::text(x4, y4 + 3, "C", col = "magenta", cex = 0.8, adj = 0.5)
    }
    return(p)
}
