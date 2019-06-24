#' @importFrom OpenImageR readImage resizeImage superpixels
#' @import apcluster
#' @import magrittr
#' @import dplyr
#' @export
img_to_pallette <- function(image_path, size_reduction = NULL, transformation = "Adobe",
                            normalise = FALSE) {


    image <- readImage(image_path)
    if (!is.null(size_reduction)) {
        new_height <- dim(image)[1] * size_reduction
        new_width <- dim(image)[2] * size_reduction
        image %<>% resizeImage(image = ., width = new_width, height = new_height)

    }

    superpixels <- OpenImageR::superpixels(input_image = image,
                               method = "slic",
                               superpixel = 200,
                               compactness = 20,
                               return_slic_data = TRUE,
                               return_labels = TRUE,
                               write_slic = "",
                               verbose = TRUE)

    # Extract red vector
    red <- as.data.frame.table(superpixels$slic_data[ , , 1]) %>%
                rename(red = Freq)

    # Extract green vector
    green <- as.data.frame.table(superpixels$slic_data[ , , 2]) %>%
                rename(green = Freq)

    # Extract blue vector
    blue <- as.data.frame.table(superpixels$slic_data[ , , 3]) %>%
                rename(blue = Freq)

    # Extract labels vector
    labels <- as.data.frame.table(superpixels$labels) %>%
                rename(labels = Freq)

    #TODO: Check that these are _definitely_ going to output in the same order for each space
    full_colour <- bind_cols(red,
                             green %>% select(green),
                             blue %>% select(blue),
                             labels %>% select(labels))

    #TODO: Super slow through here
    full_colour_lab <- rgb_to_lab(rgb = full_colour %>%
                                      select(red, green, blue),
                                  transformation = transformation,
                                  normalise = normalise) %>%
                            bind_cols(labels %>% select(labels))

    # Find mean for each super pixel
    superpixel_average <- full_colour_lab %>%
                            group_by(labels) %>%
                            summarise(l = mean(l),
                                      a = mean(a),
                                      b = mean(b))

    #### Cluster with affinity propagation ####
    clusters <- apcluster(negDistMat(r = 2),
                          superpixel_average %>% select(-labels))

    # Extract clusters
    #TODO: Gotta be able to apply this
    cluster_index <- 0
    cluster_data <- tibble()
    for (i in 1:length(clusters@clusters)) {
        cluster_data %<>% bind_rows(tibble(cluster_index = cluster_index,
                                           labels = clusters@clusters[[i]] %>% as.numeric() - 1))
        cluster_index <- cluster_index + 1
    }

    # Average colours by cluster
    full_colour_lab_summarise <- full_colour_lab %>%
                                left_join(cluster_data,
                                          by = c("labels")) %>%
                                group_by(cluster_index) %>%
                                mutate(l = mean(l),
                                          a = mean(a),
                                          b = mean(b)) %>%
                                ungroup()

    #TODO: super slow through here
    full_colour_rgb_summarise <- lab_to_rgb(lab = full_colour_lab_summarise %>%
                                                    select(l, a, b),
                                            transformation = transformation,
                                            normalise = normalise) %>%
                                    bind_cols(full_colour_lab_summarise %>%
                                                  select(labels, cluster_index))

    # Palette
    palette <- full_colour_rgb_summarise %>%
                select(red, green, blue) %>%
                distinct() %>%
                rgb_to_hex()

    array_colour_summarise <- array(c(full_colour_rgb_summarise$red, full_colour_rgb_summarise$green,
                                      full_colour_rgb_summarise$blue),
                                    dim = c(dim(image)[1], dim(image)[2], 3))

    output <- new("schemr", image = image, clustered_image = array_colour_summarise,
                  palette = palette)

    return(output)

    #TODO: Finalise output


}
