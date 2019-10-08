[![Build Status](https://travis-ci.org/stuart-morrison/schemr.svg?branch=master)](https://travis-ci.org/stuart-morrison/schemr)

`schemr` package
================

Convert photos into useable colour schemes
------------------------------------------

`schemr` is an R package for turning your photos into usable colour
schemes for R visualisations.

The key driver is the `image_to_pallette` function, which:

-   reads in images;
-   finds colour blobs within the image, representing the key
    highlighting colours; and
-   uses affinity propagation clustering to condense the set of key
    colours.

### Photo example

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

``` r
library(schemr)

# Extract key colours from imageN
schemr_image <- image_to_pallette(image_path = "Images/camping.jpg", resize_factor = 0.4,
                                  verbose = FALSE, summary_method = median)

# Plot the image
plot(schemr_image)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

In addition, printing the class, shows the vector of hex RGB codes that
make up the clustered data:

``` r
schemr_image
```

    ##  [1] "#979a99" "#6e7571" "#605f55" "#52504a" "#5f595a" "#8e878e" "#787279"
    ##  [8] "#4f494d" "#b9b9b9" "#6e705c" "#756867" "#af4e55" "#28262b" "#4d8cd8"
    ## [15] "#53bce2" "#4e627e"

### Colour space conversions

`schemr` also contains functions to convert colour data both to and
from:

-   RGB space;
-   XYZ space; and
-   Lab space.

Colour conversion constants and functions are provided for sRGB and
Adobe 1998 RGB spaces, with user ability to apply other conversions for
other RGB spaces.

For example, using excellent colours from the
[`wesanderson`](https://github.com/karthik/wesanderson) package:

``` r
library(wesanderson)

# Extract some lovely Zissou colours
colour_hex <- wes_palettes$Zissou1

# Convert to Lab space
colour_lab <- hex_to_lab(hex = colour_hex, transformation = "Adobe")

# Convert Lab space to XYZ space
colour_xyz <- lab_to_xyz(lab = colour_lab)

# Convert XYZ space to RGB colour channels
colour_rgb <- xyz_to_rgb(xyz = colour_xyz, transformation = "Adobe")
```
