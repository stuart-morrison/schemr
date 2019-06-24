#### Colour space conversion functions ####

#### RGB to hex and hex to RGB
#' @importFrom stringr str_pad
#' @export
rgb_to_hex <- function(rgb) {

    if (any(unlist(rgb[ , 1]) < 0 | unlist(rgb[ , 1]) > 255) |
        any(unlist(rgb[ , 2]) < 0 | unlist(rgb[ , 2]) > 255) |
        any(unlist(rgb[ , 3]) < 0 | unlist(rgb[ , 3]) > 255)) {
        stop("Colour channels should be between 0 and 255.")
    }

    return(paste0("#", str_pad(string = as.hexmode(floor(unlist(rgb[ , 1]))), width = 2,
                               pad = "0", side = "left"),
                  str_pad(string = as.hexmode(floor(unlist(rgb[ , 2]))), width = 2,
                          pad = "0", side = "left"),
                  str_pad(string = as.hexmode(floor(unlist(rgb[ , 3]))), width = 2,
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
        return(tibble(red = strtoi(paste0("0x", substr(stripped, 1, 2))) / 255,
                          green = strtoi(paste0("0x", substr(stripped, 3, 4))) / 255,
                          blue = strtoi(paste0("0x", substr(stripped, 5, 6))) / 255))

    } else {
        return(tibble(red = strtoi(paste0("0x", substr(stripped, 1, 2))),
                          green = strtoi(paste0("0x", substr(stripped, 3, 4))),
                          blue = strtoi(paste0("0x", substr(stripped, 5, 6)))))
    }

}

#### RGB to XYZ to RGB
# Conversion from RGB space to XYZ space
#' @export
rgb_to_xyz <- function(rgb, transformation = "Adobe", normalise = FALSE) {

    if (class(transformation) == "Matrix") {
        if (all(dim(transformation) == c(3L, 3L))) {
            m <- transformation
        } else {
            stop("Transformation should be a 3x3 matrix.")
        }
    } else {
        transformation_match <- match.arg(transformation, names(transformation_matrix), several.ok = FALSE)
        m <- transformation_matrix[[transformation_match]]
    }

    if (normalise) {
        xyz <- as_tibble(t(apply(X = rgb / 255, MARGIN = 1, FUN = function(x) m %*% x)))
        colnames(xyz) <- c("x", "y", "z")
        return(xyz)
    } else {
        xyz <- as_tibble(t(apply(X = rgb, MARGIN = 1, FUN = function(x) m %*% x)))
        colnames(xyz) <- c("x", "y", "z")
        return(xyz)
    }

}

# Conversion from XYZ space to RGB space
#' @export
xyz_to_rgb <- function(xyz, transformation = "Adobe", normalise = FALSE) {

    if (class(transformation) == "Matrix") {
        if (all(dim(transformation) == c(3L, 3L))) {
            m <- transformation
        } else {
            stop("Transformation should be a 3x3 matrix.")
        }
    } else {
        transformation_match <- match.arg(transformation, names(transformation_matrix), several.ok = FALSE)
        m <- solve(transformation_matrix[[transformation_match]])
    }

    if (normalise) {
        rgb <- as_tibble(scale(t(apply(X = xyz, MARGIN = 1, FUN = function(x) m %*% x)), center = FALSE) * 255)
        colnames(rgb) <- c("red", "green", "blue")
        return(rgb)
    } else {
        rgb <- as_tibble(t(apply(X = xyz, MARGIN = 1, FUN = function(x) m %*% x)))
        colnames(rgb) <- c("red", "green", "blue")
        return(rgb)
    }

}

#### XYZ to Lab
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
        stop("XYZ colours should be a matrix or tibble with three columns.")
    }

    colours_lab <- tibble(l = l_star(.y = unlist(xyz[ , 2]), .y_n = y_n, .f = f),
                              a = a_star(.x = unlist(xyz[, 1]), .x_n = x_n, .y = unlist(xyz[, 2]), .y_n = y_n, .f = f),
                              b = b_star(.y = unlist(xyz[, 2]), .y_n = y_n, .z = unlist(xyz[, 3]), .z_n = z_n, .f = f))

    return(colours_lab)
}


#### Lab to XYZ
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
        stop("Lab colours should be a matrix or tibble with three columns.")
    }

    colours_xyz <- tibble(x = lab_to_x(.l = unlist(lab[ , 1]), .a = unlist(lab[ , 2]), .x_n = x_n, .f_inv = f_inv),
                              y = lab_to_y(.l = unlist(lab[ , 1]), .y_n = y_n, .f_inv = f_inv),
                              z = lab_to_z(.l = unlist(lab[ , 1]), .b = unlist(lab[ , 3]), .z_n = z_n, .f_inv = f_inv))
    return(colours_xyz)

}

#### RGB/hex to Lab and RGB/hex to Lab
#' @export
rgb_to_lab <- function(rgb, transformation = "Adobe", normalise = FALSE) {
    xyz <- rgb_to_xyz(rgb = rgb, transformation = transformation, normalise = normalise)
    lab <- xyz_to_lab(xyz = xyz)
    return(lab)
}

#' @export
lab_to_rgb <- function(lab, transformation = "Adobe", normalise = FALSE) {
    xyz <- lab_to_xyz(lab = lab)
    rgb <- xyz_to_rgb(xyz = xyz, transformation = transformation, normalise = normalise)
    return(rgb)
}

#' @export
hex_to_lab <- function(hex, transformation = "Adobe", normalise = FALSE) {
    rgb <- hex_to_rgb(hex_colours = hex, normalise = normalise)
    lab <- rgb_to_lab(rgb, transformation = transformation, normalise = normalise)
    return(lab)
}

#' @export
lab_to_hex <- function(lab, transformation = "Adobe", normalise = FALSE) {
    rgb <- lab_to_rgb(lab = lab, transformation = transformation, normalise = normalise)
    hex <- rgb_to_hex(rgb)
    return(hex)
}
