#### Colour space constants
# XYZ to RGB transformation matrices
# Bruce Lindbloom, RGB/XYZ Matrices, http://brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
transformation_matrix <-
    list(
        Adobe = structure(
            c(0.5767309, 0.2973769, 0.0270343,
              0.185554, 0.6273491, 0.0706872,
              0.1881852, 0.0752741, 0.9911085),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        sRGB = structure(
            c(0.4124564, 0.2126729, 0.0193339,
              0.3575761, 0.7151522, 0.119192,
              0.1804375, 0.072175, 0.9503041),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        )
    )

# Tristimulus values of the reference white point
# Illuminant D65
# https://en.wikipedia.org/wiki/CIELAB_color_space
x_n <- 95.0489
y_n <- 100
z_n <- 108.8840


