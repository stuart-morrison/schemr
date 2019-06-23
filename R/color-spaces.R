colours_hex <- c("#008698", "#232C31", "#b5a991", "#c94b20", "#696A6D", "#A79344",
                 "#006472", "#4e4f51", "#963817", "#928262", "#7d6e33", "#28e5ff",
                 "#a4a5a7", "#e88e6f", "#d2cbbc", "#cfc189")

#############################################################################
#### Colour space conversion functions ####
#############################################################################

############################################################################
#### RGB and RGB hex ####
#############################################################################
#' @importFrom stringr str_pad
#' @export
rgb_to_hex <- function(red, green, blue) {

    if (any(red < 0 | red > 255) | any(green < 0 | green > 255) | any(blue < 0 | blue > 255)) {
        stop("Colour channels should be between 0 and 255.")
    }

    return(paste0("#", str_pad(string = as.hexmode(floor(red)), width = 2,
                               pad = "0", side = "left"),
                  str_pad(string = as.hexmode(floor(green)), width = 2,
                          pad = "0", side = "left"),
                  str_pad(string = as.hexmode(floor(blue)), width = 2,
                          pad = "0", side = "left")))
}

#### Convert hexadecimal colours to RGB
#' @export
hex_to_rgb <- function(hex_colours, normalise = FALSE) {

    stripped <- gsub(pattern = "#", replacement = "", x = hex_colours)

    # Test character lengther
    if (!all(nchar(stripped) == 6)) {
        bad_colours <- which(nchar(stripped) != 6)
        stop(paste("The following colours do not have six digits.\n", paste(bad_colours, collapse = ", ")))
    }

    # Test all parse to numeric
    if (any(is.na(strtoi(paste0("0x", stripped))))) {
        bad_colours <- which(is.na(strtoi(paste0("0x", stripped))))
        stop(paste("The following colours do not parse from hex to decimal.\n", paste(bad_colours, collapse = ", ")))
    }



    if (normalise) {
        return(data.frame(red = strtoi(paste0("0x", substr(stripped, 1, 2))) / 255,
                          green = strtoi(paste0("0x", substr(stripped, 3, 4))) / 255,
                          blue = strtoi(paste0("0x", substr(stripped, 5, 6))) / 255,
                          stringsAsFactors = FALSE))

    } else {
        return(data.frame(red = strtoi(paste0("0x", substr(stripped, 1, 2))),
                          green = strtoi(paste0("0x", substr(stripped, 3, 4))),
                          blue = strtoi(paste0("0x", substr(stripped, 5, 6))),
                          stringsAsFactors = FALSE))
    }

}

#############################################################################
#### RGB to XYZ to RGb ####
#############################################################################

# XYZ transformation matrix
#TODO: Options of transformation matrics
#' @export
transformation_matrix <-  matrix(c(0.5767309, 0.1855540, 0.1881852,
                                   0.2973769, 0.6273491, 0.0752741,
                                   0.0270343, 0.0706872,  0.9911085),
                                 nrow = 3, byrow = TRUE)

# Conversion from RGB space to XYZ space
#' @export
rgb_to_xyz <- function(rgb, m, normalise = FALSE) {

    if (normalise) {
        return(t(apply(X = rgb / 255, MARGIN = 1, FUN = function(x) m %*% x)))
    } else {
        return(t(apply(X = rgb, MARGIN = 1, FUN = function(x) m %*% x)))
    }

}

# Conversion from XYZ space to RGB space
#' @export
xyz_to_rgb <- function(xyz, m, normalise = FALSE) {

    if (normalise) {
        rgb <- as.data.frame(scale(t(apply(X = xyz, MARGIN = 1, FUN = function(x) m %*% x)), center = FALSE) * 255)
        colnames(rgb) <- c("red", "green", "blue")
        return(rgb)
    } else {
        rgb <- as.data.frame(t(apply(X = xyz, MARGIN = 1, FUN = function(x) m %*% x)))
        colnames(rgb) <- c("red", "green", "blue")
        return(rgb)
    }

}

#############################################################################
#### XYZ to Lab ####
#############################################################################

# Constants
#TODO: Check options for constants here
x_n <- 95.0489
y_n <- 100
z_n <- 108.8840


f <- function(num, dem) {
    delta <- 6 / 29

    t <- num / dem

    ifelse(t > (delta ^ 3), t ^ (1 / 3), (t / (3 * delta ^ 2)) + (4 / 29))
}

l_star <- function(.y, .y_n, .f) {
    return(116 * .f(.y, .y_n) - 16)
}

a_star <- function(.x, .x_n, .y, .y_n, .f) {
    return(500 * (.f(.x, .x_n) - .f(.y, .y_n)))
}

b_star <- function(.y, .y_n, .z, .z_n, .f) {
    return(200 * (.f(.y, .y_n) - .f(.z, .z_n)))
}

#' @export
xyz_to_lab <- function(xyz) {
    if (ncol(xyz) != 3) {
        stop("XYZ colours should be a matrix or data.frame with three columns.")
    }

    colours_lab <- data.frame(l = l_star(.y = xyz[ , 2], .y_n = y_n, .f = f),
                              a = a_star(.x = xyz[ , 1], .x_n = x_n, .y = xyz[ , 2], .y_n = y_n, .f = f),
                              b = b_star(.y = xyz[ , 2], .y_n = y_n, .z = xyz[ , 3], .z_n = z_n, .f = f))

    return(colours_lab)
}


#############################################################################
#### Lab to XYZ ####
#############################################################################

f_inv <- function(t) {
    delta <- 6 / 29

    ifelse(t > delta, t ^ 3, (3 * delta ^ 2) * (t - (4 / 29)))
}

lab_to_x <- function(.l, .a, .x_n, .f_inv) {
    return(.x_n * .f_inv(((.l + 16) / 116) + (.a / 500)))
}

lab_to_y <- function(.l, .y_n, .f_inv) {
    return(.y_n * .f_inv((.l + 16) / 116))
}

lab_to_z <- function(.l, .b, .z_n, .f_inv) {
    return(.z_n * .f_inv(((.l + 16) / 116) - (.b / 200)))
}

#' @export
lab_to_xyz <- function(lab) {
    if (ncol(lab) != 3) {
        stop("Lab colours should be a matrix or data.frame with three columns.")
    }

    colours_xyz <- data.frame(x = lab_to_x(.l = lab[ , 1], .a = lab[ , 2], .x_n = x_n, .f_inv = f_inv),
                              y = lab_to_y(.l = lab[ , 1], .y_n = y_n, .f_inv = f_inv),
                              z = lab_to_z(.l = lab[ , 1], .b = lab[ , 3], .z_n = z_n, .f_inv = f_inv))
    return(colours_xyz)

}

#############################################################################
#### RGB to Lab ####
#############################################################################

    #' @export
    rgb_to_lab <- function(rgb, m, normalise = FALSE) {
        xyz <- rgb_to_xyz(rgb = rgb, m = m, normalise = normalise)
        lab <- xyz_to_lab(xyz = xyz)
        return(lab)
    }

    #' @export
    lab_to_rgb <- function(lab, m, normalise = FALSE) {
        xyz <- lab_to_xyz(lab = lab)
        rgb <- xyz_to_rgb(xyz = xyz, m = solve(m), normalise = normalise)
        return(rgb)
    }

    #' @export
    hex_to_lab <- function(hex, m, normalise = FALSE) {
        rgb <- hex_to_rgb(hex_colours = hex, normalise = normalise)
        lab <- rgb_to_lab(rgb, m = m, normalise = normalise)
        return(lab)
    }

    #' @export
    lab_to_hex <- function(lab, m, normalise = FALSE) {
        rgb <- lab_to_rgb(lab = lab, m = solve(m), normalise = normalise)
        hex <- rgb_to_hex(rgb)
        return(hex)
    }
