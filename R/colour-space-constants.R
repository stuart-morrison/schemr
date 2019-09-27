#### Colour space constants
# RGB to XYZ transformation matrices
# Source: Colour-science, https://github.com/colour-science/colour/
# ```python
# import numpy as np
# import colour
#
# np.set_printoptions(formatter={'float': '{:0.15f}'.format})
# colour.models.sRGB_COLOURSPACE.use_derived_transformation_matrices(True)
# colour.models.ADOBE_RGB_1998_COLOURSPACE.use_derived_transformation_matrices(True)
# print(colour.models.sRGB_COLOURSPACE.RGB_to_XYZ_matrix)
# print(colour.models.ADOBE_RGB_1998_COLOURSPACE.RGB_to_XYZ_matrix)
#```
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

# RGB to XYZ transformation matrices
# Source: Colour-science, https://github.com/colour-science/colour/
# ```python
# import numpy as np
# import colour
#
# np.set_printoptions(formatter={'float': '{:0.15f}'.format})
# colour.models.sRGB_COLOURSPACE.use_derived_transformation_matrices(True)
# colour.models.ADOBE_RGB_1998_COLOURSPACE.use_derived_transformation_matrices(True)
# print(colour.models.sRGB_COLOURSPACE.XYZ_to_RGB_matrix)
# print(colour.models.ADOBE_RGB_1998_COLOURSPACE.XYZ_to_RGB_matrix)
#```
transformation_matrix_inverse <-
    list(
        Adobe = structure(
            c(2.041587903810747, -0.969243636280880, 0.013444280632031,
              -0.565006974278860, 1.875967501507721, -0.118362392231018,
              -0.344731350778330, 0.041555057407176, 1.015174994391205),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),

        sRGB = structure(
            c(3.240969941904521, -0.969243636280880, 0.055630079696994,
             -1.537383177570093, 1.875967501507720, -0.203976958888977,
             -0.498610760293003, 0.041555057407176, 1.056971514242878),
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


