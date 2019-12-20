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

#' @exportMethod palette
setMethod(f = "palette", signature = "schemr",
          definition = function(value) {
              barplot(rep(1,length(value$palette)), col = value$palette, space = 0, border = NA, axes = FALSE)
              text(x = 1:length(value$palette) - 0.5, y = 0.5,labels = value$palette, col = "white", srt = 90)
              text(x = 1:length(value$palette) - 0.5, y = 0.3, labels = 1:length(value$palette), col = "white")
          }
          )
