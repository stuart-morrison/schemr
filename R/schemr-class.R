# Create the schemr class, which holds the palette and image data
setClass("schemr", representation(image = "array", clustered_image = "array",
                                  palette = "character"))

# Schemr print method
print.schemr <- function(x) {
    print(x@palette)
}

# Schemr plot method
# Plot clustered version of the image
plot.schemr <- function(x) {
    (x@clustered_image / 255) %>%
        as.raster() %>%
        plot()
}

show.schemr <- function(x) {
    print(x@palette)
}
