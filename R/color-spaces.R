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
rgb_to_hex <- function(red, green, blue) {

    return(paste0("#", str_pad(string = as.hexmode(red), width = 2,
                               pad = "0", side = "left"),
                  str_pad(string = as.hexmode(green), width = 2,
                          pad = "0", side = "left"),
                  str_pad(string = as.hexmode(blue), width = 2,
                          pad = "0", side = "left")))
}

#### Convert hexadecimal colours to RGB
hex_to_rgb <- function(hex_colours, normalise = FALSE) {

    stripped <- gsub(pattern = "#", replacement = "", x = hex_colours)

    # Test character lengther
    if (!all(nchar(stripped) == 6)) {
        bad_colours <- which(nchar(stripped) != 6)
        warning(paste("The following colours do not have six digits.\n", paste(bad_colours, collapse = ", ")))
        stop()
    }

    # Test all parse to numeric
    if (any(is.na(strtoi(paste0("0x", stripped))))) {
        bad_colours <- which(is.na(strtoi(paste0("0x", stripped))))
        warning(paste("The following colours do not parse from hex to decimal.\n", paste(bad_colours, collapse = ", ")))
        stop()
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
transformation_matrix <-  matrix(c(0.5767309, 0.1855540, 0.1881852,
                                   0.2973769, 0.6273491, 0.0752741,
                                   0.0270343, 0.0706872,  0.9911085),
                                 nrow = 3, byrow = TRUE)

# Conversion from RGB space to XYZ space
rgb_to_xyz <- function(rgb, m, normalise = FALSE) {

    if (normalise) {
        return(t(apply(X = rgb / 255, MARGIN = 1, FUN = function(x) m %*% x)))
    } else {
        return(t(apply(X = rgb, MARGIN = 1, FUN = function(x) m %*% x)))
    }

}

# Conversion from XYZ space to RGB space
xyz_to_rgb <- function(xyz, m, scale = FALSE) {

    if (scale) {
        return(t(apply(X = xyz, MARGIN = 1, FUN = function(x) m %*% x)) * 255)
    } else {
        return(t(apply(X = xyz, MARGIN = 1, FUN = function(x) m %*% x)))
    }

}

colours_rgb <- hex_to_rgb(colours_hex, normalise = TRUE)
colours_xyz <- rgb_to_xyz(rgb = colours_rgb, m = transformation_matrix)

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


colours_lab <- data.frame(l = l_star(.y = colours_xyz[ , 2], .y_n = y_n, .f = f),
                          a = a_star(.x = colours_xyz[ , 1], .x_n = x_n, .y = colours_xyz[ , 2], .y_n = y_n, .f = f),
                          b = b_star(.y = colours_xyz[ , 2], .y_n = y_n, .z = colours_xyz[ , 3], .z_n = z_n, .f = f))

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

back_to_xyz <- data.frame(x = lab_to_x(.l = colours_lab$l, .a = colours_lab$a, .x_n = x_n, .f_inv = f_inv),
                          y = lab_to_y(.l = colours_lab$l, .y_n = y_n, .f_inv = f_inv),
                          z = lab_to_z(.l = colours_lab$l, .b = colours_lab$b, .z_n = z_n, .f_inv = f_inv))


back_to_rgb <- xyz_to_rgb(xyz = back_to_xyz, m = solve(transformation_matrix), scale = TRUE)

back_to_hex <- rgb_to_hex(round(back_to_rgb[ , 1]), round(back_to_rgb[ , 2]), round(back_to_rgb[ , 3]))


