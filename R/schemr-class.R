#' Create the schemr class, which holds the palette and image data
#' @importFrom graphics plot barplot text
#' @importFrom grDevices as.raster
#' @exportClass schemr
#' @field image An array of dimension (Image width) by (Image height) by (3 colour channels) that contains the data of the original image
#' @field clustered_image An array of dimension (Image width) by (Image height) by (3 colour channels) that contains the data of the image with clustered colour blocks
#' @field palette A character vector that contains the colours of the resulting colour palette
setRefClass(
    Class = "schemr",
    fields = list(
        image = "array",
        clustered_image = "array",
        palette = "character"
        ),
    methods = list(
        print = function(x) {
            "Print the colour palette."
            print(x$palette)
            },
        show = function(.self) {
            "Show the colour palette."
            print(.self$palette)
        }
        )
    )

### Set schemr class methods
# Set plot method for schemr class
#' Plot the clustered image data
#' @exportMethod plot
#' @param x A schemr class object
#' @param y Not used, NULL
#' @param ... Other arguments to pass onto `plot`
#' @return No return value, calls a raster plot of the clustered image data.
setMethod(
    f = "plot",
    signature = "schemr",
    definition = function(x, y = NULL, ...) plot(as.raster((x$clustered_image / 255)), ...)
    )

#' Plot the colour palette
#' @exportMethod palette
#' @param value A schemr class object
#' @return No return value, calls a barplot of the colour pallette.
setMethod(
    f = "palette",
    signature = "schemr",
    definition = function(value) {
        barplot(rep(1,length(value$palette)), col = value$palette, space = 0, border = NA, axes = FALSE)
        text(x = 1:length(value$palette) - 0.5, y = 0.5,labels = value$palette, col = "white", srt = 90, cex = 1.5)
        text(x = 1:length(value$palette) - 0.5, y = 0.3, labels = 1:length(value$palette), col = "white", cex = 1.5)
    }
    )
