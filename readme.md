Schemr
------

`schemr` is an R package for turning your photos into usable colour
schemes for R visualisations.

The key driver is the `img_to_pallette` function, which:

-   reads in images;
-   finds colour blobs within the image, representing the key
    highlighting colours; and
-   uses affinity propagation clustering to condense the set of key
    colours.

### Example 1

First we have a look at a photo of me camping.

``` r
library(OpenImageR)
library(magrittr)

# Read in the image
image <- readImage(path = "Images/camping.jpg")

# Shrink down the image
new_height <- dim(image)[1] * 0.4
new_width <- dim(image)[2] * 0.4
image %<>% resizeImage(image = ., width = new_width, height = new_height)

# Plot
plot(as.raster(image))
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

We see big blobs of blue and orange. Using schemr to extract these, we
get:

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

In addition, printing the class, shows the vector of hex RGB codes that
make up the clustered data:

``` r
schemr_image
```

    ##  [1] "#989a99" "#6f7571" "#606056" "#53514a" "#5f595b" "#8e878e" "#797379"
    ##  [8] "#4f494d" "#bababa" "#6e715d" "#756868" "#af4e56" "#28272c" "#4d8cd9"
    ## [15] "#53bce3" "#4e627e"
