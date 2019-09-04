#### Colour space conversion functions ####

#### RGB to hex and hex to RGB
#' Convert RGB colour channels to hex colour codes.
#' @importFrom stringr str_pad
#' @export
#' @param rgb A dataframe or matrix with red, green and blue colour channels located in the columns 1 to 3, respectively. Colour channel values should be between 0 and 255, inclusive.
#' @return A character vector with hex representations of RGB colour channels.
#' @examples
#' red <- sample(x = 1:255, size = 10, replace = TRUE)
#' green <- sample(x = 1:255, size = 10, replace = TRUE)
#' blue <- sample(x = 1:255, size = 10, replace = TRUE)
#' rgb_to_hex(data.frame(r = red, g = green, b = blue))
rgb_to_hex <- function(rgb) {

    # Extract colour channels from first three colummns
    red <- unlist(rgb[ , 1])
    green <- unlist(rgb[ , 2])
    blue <- unlist(rgb[ , 3])

    # Test if any RGB values are outside of [0, 255]
    if (any(red < 0 | red > 255) |
        any(green < 0 | green > 255) |
        any(blue < 0 | blue > 255)) {
        stop("Colour channels should be between 0 and 255.")
    }

    # Convert colour channels to hex
    hex_value <- paste0("#",
                        # Convert red
                        str_pad(string = as.hexmode(round(red, 0)),
                                width = 2, pad = "0", side = "left"),
                        # Convert green
                        str_pad(string = as.hexmode(round(green, 0)),
                                width = 2, pad = "0", side = "left"),
                        # Convert blue
                        str_pad(string = as.hexmode(round(blue, 0)),
                                width = 2, pad = "0", side = "left")
                        )

    return(hex_value)
}

#### Convert hexadecimal colours to RGB
#' Convert hexadecimal colours to RGB colour channels.
#' @export
#' @param hex A character vector containing hex representations of RGB colours.
#' @return A tibble of red, green and blue colour channels.
#' @examples
#' hex_to_rgb(c("#5f9e3a"))
hex_to_rgb <- function(hex) {

    # Take out any #s
    stripped <- gsub(pattern = "#", replacement = "", x = hex)

    # Test character length - each channel needs two hex digits
    if (!all(nchar(stripped) == 6)) {
        bad_colours <- which(nchar(stripped) != 6)
        stop(paste("The following colours do not have six digits:\n", paste(bad_colours, collapse = ", ")))
    }

    # Test all parse to numeric
    if (any(is.na(strtoi(paste0("0x", stripped))))) {
        bad_colours <- which(is.na(strtoi(paste0("0x", stripped))))
        stop(paste("The following colours do not parse from hex to decimal:\n", paste(bad_colours, collapse = ", ")))
    }

    return(tibble(red = strtoi(paste0("0x", substr(stripped, 1, 2))),
                  green = strtoi(paste0("0x", substr(stripped, 3, 4))),
                  blue = strtoi(paste0("0x", substr(stripped, 5, 6)))))

}

#### RGB to XYZ to RGB
#' Convert from RGB colour channels to XYZ space. 
#' @export
#' @param rgb A dataframe or matrix with red, green and blue colour channels located in the columns 1 to 3, respectively. Colour channel values should be between 0 and 255, inclusive.
#' @param transformation An option in \code{c("sRGB", "Adobe")} for a built-in transformation or, alternatively, a custom 3x3 transformation matrix.
#' @examples
#' red <- sample(x = 1:255, size = 10, replace = TRUE)
#' green <- sample(x = 1:255, size = 10, replace = TRUE)
#' blue <- sample(x = 1:255, size = 10, replace = TRUE)
#' rgb_to_xyz(data.frame(r = red, g = green, b = blue), transformation = "Adobe")
rgb_to_xyz <- function(rgb, transformation = "sRGB") {

    # Extract transformation matrix or default matrix
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

    # Unlist RGB from data structure
    temp_r <- unlist(rgb[[1]]) / 255
    temp_g <- unlist(rgb[[2]]) / 255
    temp_b <- unlist(rgb[[3]]) / 255


    # Convert RGB space to linear speace
    if (transformation_match == "sRGB") {
        temp_r <- srgb_transformation(temp_r)
        temp_g <- srgb_transformation(temp_g)
        temp_b <- srgb_transformation(temp_b)
    } else if (transformation_match == "Adobe") {
        temp_r <- adobe_transformation(temp_r)
        temp_g <- adobe_transformation(temp_g)
        temp_b <- adobe_transformation(temp_b)
    } else {
        temp_r <- temp_r * 100
        temp_g <- temp_g * 100
        temp_b <- temp_b * 100
    }

    # Apply linear transformation for RGB to XYZ
    xyz <- tibble(x = sum(c(temp_r, temp_g, temp_b) * m[1, ]),
                  y = sum(c(temp_r, temp_g, temp_b) * m[2, ]),
                  z = sum(c(temp_r, temp_g, temp_b) * m[3, ]))

    return(xyz)

}

# Conversion from XYZ space to RGB space
#' @export
xyz_to_rgb <- function(xyz, transformation = "sRGB") {

    # Extract transformation matrix or default type
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

    # Unlist x, y, z from data structure
    temp_x <- unlist(xyz[[1]]) / 100
    temp_y <- unlist(xyz[[2]]) / 100
    temp_z <- unlist(xyz[[3]]) / 100

    # Linear transformation from converted XYZ to RGB
    temp_r <- temp_x * m[1, 1] + temp_y * m[1, 2] + temp_z * m[1, 3]
    temp_g <- temp_x * m[2, 1] + temp_y * m[2, 2] + temp_z * m[2, 3]
    temp_b <- temp_x * m[3, 1] + temp_y * m[3, 2] + temp_z * m[3, 3]

    # Convert to linear space
    if (transformation_match == "sRGB") {
        temp_r <- srgb_transformation_inverse(temp_r)
        temp_g <- srgb_transformation_inverse(temp_g)
        temp_b <- srgb_transformation_inverse(temp_b)
    } else if (transformation_match == "Adobe") {
        temp_r <- adobe_transformation_inverse(temp_r)
        temp_g <- adobe_transformation_inverse(temp_g)
        temp_b <- adobe_transformation_inverse(temp_b)
    } else {
        temp_r <- temp_r * 255
        temp_g <- temp_g * 255
        temp_b <- temp_b * 255
    }

    return(tibble(red = temp_r,
                  green = temp_g,
                  blue = temp_b))

}


# Conversion from sRGB to linear space
srgb_transformation <- function(x) {
    output <- ifelse(test = x > 0.04045,
                     yes = ((x + 0.055) / 1.055) ^ 2.4,
                     no = x / 12.92)
    output <- output * 100
    return(output)
}

# Conversion from XYZ to linear space
srgb_transformation_inverse <- function(x) {
    output <- ifelse(test = x > 0.0031308,
                     yes = (1.055 * (x ^ (1 / 2.4))) - 0.055,
                     no = x * 12.92)
    output <- output * 255
    return(output)
}

# Conversion from Adobe RGB to linear space
adobe_transformation <- function(x) {
    output <- (x ^ 2.19921875) * 100
    return(output)
}

# Conversion from XYZ to linear space
adobe_transformation_inverse <- function(x) {
    output <- (x ^ (1 / 2.19921875)) * 255
    return(output)
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
rgb_to_lab <- function(rgb, transformation = "sRGB") {
    xyz <- rgb_to_xyz(rgb = rgb, transformation = transformation)
    lab <- xyz_to_lab(xyz = xyz)
    return(lab)
}

#' @export
lab_to_rgb <- function(lab, transformation = "sRGB") {
    xyz <- lab_to_xyz(lab = lab)
    rgb <- xyz_to_rgb(xyz = xyz, transformation = transformation)
    return(rgb)
}

#' @export
hex_to_lab <- function(hex, transformation = "sRGB") {
    rgb <- hex_to_rgb(hex_colours = hex)
    lab <- rgb_to_lab(rgb, transformation = transformation)
    return(lab)
}

#' @export
lab_to_hex <- function(lab, transformation = "sRGB") {
    rgb <- lab_to_rgb(lab = lab, transformation = transformation)
    hex <- rgb_to_hex(rgb)
    return(hex)
}
