# Create the schemr class, which holds the palette and image data
setRefClass(Class = "schemr",
            fields = representation(
                                    image = "array",
                                    clustered_image = "array",
                                    palette = "character"
                                    )
            )

# Set schemr class methods
setMethod(f = "print", signature = "schemr", definition = function(x) x$palette)
setMethod(f = "show", signature = "schemr", definition = function(x) print(x$palette))
setMethod(f = "plot", signature = "schemr", definition = function(x) plot(as.raster((x$clustered_image / 255))))
