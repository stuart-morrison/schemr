#### Colour space constants
# XYZ to RGB transformation matrices
# Source: Colour-science, https://github.com/colour-science/colour/
transformation_matrix <-
    list(
        Adobe = structure(
            c(0.576669042910130, 0.297344975250536, 0.027031361386412,
              0.185558237906546, 0.627363566255466, 0.075291458493998,
              0.188228646234995, 0.075291458493998, 0.991337536837639),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        sRGB = structure(
            c(0.412390799265960, 0.212639005871510, 0.019330818715592,
              0.357584339383878, 0.715168678767756, 0.119194779794626,
              0.180480788401834, 0.072192315360734, 0.950532152249661),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        )
    )

# Tristimulus values of the reference white point
# Illuminant D65
# Source: https://en.wikipedia.org/wiki/CIELAB_color_space
x_n <- 95.0489
y_n <- 100
z_n <- 108.8840


