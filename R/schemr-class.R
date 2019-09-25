# Create the schemr class, which holds the palette and image data
#' @exportClass schemr
setRefClass(Class = "schemr",
            fields = list(
                          image = "array",
                          clustered_image = "array",
                          palette = "character"
                          ),
            methods = list(
                           print = function(x) print(x$palette),
                           show = function(.self) print(.self$palette)
                           )
            )

### Set schemr class methods
# Set plot method for schemr class
#' @exportMethod plot
setMethod(f = "plot", signature = "schemr", definition = function(x, y = NULL, ...) plot(as.raster((x$clustered_image / 255)), ...))

