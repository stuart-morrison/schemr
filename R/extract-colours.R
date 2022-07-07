#' Develop a usable colour palette form an image.
#' @importFrom OpenImageR readImage resizeImage superpixels
#' @importFrom apcluster apcluster negDistMat
#' @import magrittr
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom methods new
#' @export
#' @param image_path A character path to the image to cluster. Reads images of type .png, .jpeg, .jpg, .tiff.
#' @param resize_factor A numeric scalar that reduces (or increases) the size of the image before any processing.
#' @param colour_space The colour space of the original image. The clustering is undertaken in the Lab space. This is an an option in \code{c("sRGB", "Adobe")} for a built-in transformation or, alternatively, a custom 3x3 transformation matrix.
#' @param rgb_to_linear_func The clustering is undertaken in the Lab space. This is a function to convert RGB colour space into linear RGB space. Used only if a custom transformation matrix is provided. Transformation skips if no function is provided under a user-defined transformation matrix. See: https://en.wikipedia.org/wiki/SRGB.
#' @param rgb_to_nonlinear_func The clustering is undertaken in the Lab space. This is a function to convert linear RGB colour space into non-linear RGB space. Used only if a custom transformation matrix is provided. Transformation skips if no function is provided under a user-defined transformation matrix. See: https://en.wikipedia.org/wiki/SRGB.
#' @param method From \code{OpenImageR::superpixels}. A character string specifying the method to use. Either "slic" or "slico".
#' @param superpixel From \code{OpenImageR::superpixels}. A numeric value specifying the number of superpixels to use.
#' @param compactness From \code{OpenImageR::superpixels}. A numeric value specifying the compactness parameter. The compactness parameter is needed only if method is "slic". The "slico" method adaptively chooses the compactness parameter for each superpixel differently.
#' @param verbose From \code{OpenImageR::superpixels}. A boolean. If TRUE then information will be printed in the R session.
#' @param s From \code{apcluster::apcluster}. An l x l similarity matrix or a similarity function either specified as the name of a package-provided similarity function as character string or a user provided function object. s may also be a sparse matrix according to the Matrix package. Internally, apcluster uses the dgTMatrix class; all other sparse matrices are cast to this class (if possible, otherwise the function quits with an error). If s is any other object of class Matrix, s is cast to a regular matrix internally (if possible, otherwise the function quits with an error).
#' @param summary_method Function to summarise colours in clustered superpixels. Defaults to \code{mean}.
#' @param ... Other arguments to be passed to the apcluster algorithm. For the methods with signatures character,ANY and function,ANY, all other arguments are passed to the selected similarity function as they are; for the methods with signatures Matrix,missing and sparseMatrix,missing, further arguments are passed on to the apcluster methods with signatures Matrix,missing and dgTMatrix,missing, respectively.
#' @return A \code{schemr} object containing colour scheme colours and image properties and clusters.
image_to_pallette <- function(image_path, resize_factor = NULL, colour_space = "sRGB", rgb_to_linear_func = NULL,
                              rgb_to_nonlinear_func = NULL, method = "slic", superpixel = 200, compactness = 20,
                              verbose = TRUE, s = negDistMat(r = 2), summary_method = mean, ...) {

    # Read image path
    image <- readImage(image_path)

    # Resize if necessary
    if (!is.null(resize_factor)) {
        new_height <- dim(image)[1] * resize_factor
        new_width <- dim(image)[2] * resize_factor
        image %<>% resizeImage(image = ., width = new_height, height = new_width)

    }

    # Find the superpixel groups in the image
    superpixels <- OpenImageR::superpixels(
        input_image = image,
        method = method,
        superpixel = superpixel,
        compactness = compactness,
        return_slic_data = TRUE,
        return_labels = TRUE,
        write_slic = "",
        verbose = verbose
        )

    # Extract red vector
    red <- as.data.frame.table(superpixels$slic_data[ , , 1]) |>
                rename(red = Freq)

    # Extract green vector
    green <- as.data.frame.table(superpixels$slic_data[ , , 2]) |>
                rename(green = Freq)

    # Extract blue vector
    blue <- as.data.frame.table(superpixels$slic_data[ , , 3]) |>
                rename(blue = Freq)

    # Extract labels vector
    labels <- as.data.frame.table(superpixels$labels) |>
                rename(labels = Freq)

    # Create a tibble of RGB channels
    full_colour <- bind_cols(
        red |> select(red),
        green |> select(green),
        blue |> select(blue),
        labels |> select(labels)
        )

    # Convert to Lab spaces
    full_colour_lab <- rgb_to_lab(
        rgb = full_colour |>
            select(red, green, blue),
        transformation = colour_space,
        linear_func = rgb_to_linear_func
        ) |>
        bind_cols(labels |> select(labels))

    # Find mean for each super pixel
    superpixel_average <- full_colour_lab |>
                            group_by(labels) |>
                            summarise(l = mean(l),
                                      a = mean(a),
                                      b = mean(b))

    # Cluster with affinity propagation
    clusters <- apcluster(
        s = s,
        x = superpixel_average |> select(-labels),
        ...
        )

    # Extract clusters
    cluster_data <- map_df(
        .x = 1:length(clusters@clusters),
        .f = function(x) {
            temp_df <- tibble(cluster_index = x,
                              labels = clusters@clusters[[x]] |> as.numeric() - 1)
            }
        )

    # Average colours by cluster
    full_colour_lab_summarise <- full_colour_lab |>
                                    left_join(cluster_data,
                                              by = c("labels")) |>
                                        group_by(cluster_index) |>
                                        mutate(
                                            l = summary_method(l),
                                            a = summary_method(a),
                                            b = summary_method(b)
                                            ) |>
                                        ungroup()

    # Convert back to RGB space
    full_colour_rgb_summarise <- lab_to_rgb(
        lab = full_colour_lab_summarise |>
            select(l, a, b),
        transformation = colour_space,
        linear_func = rgb_to_nonlinear_func) |>
        bind_cols(full_colour_lab_summarise |>
                      select(labels, cluster_index)
                  )

    # Palette extraction from the clustered data
    palette <- full_colour_rgb_summarise |>
                    select(red, green, blue) |>
                    distinct() |>
                    rgb_to_hex()

    array_colour_summarise <- array(c(full_colour_rgb_summarise$red, full_colour_rgb_summarise$green,
                                      full_colour_rgb_summarise$blue),
                                    dim = c(dim(image)[1], dim(image)[2], 3))

    output <- new("schemr", image = image, clustered_image = array_colour_summarise,
                  palette = palette)

    return(output)

}
