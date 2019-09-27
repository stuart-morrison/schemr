#### Colour space constants
# RGB to XYZ transformation matrices
# Source: Colour-science, https://github.com/colour-science/colour/
# ```python
# import numpy as np
# import colour
#
# np.set_printoptions(formatter={'float': '{:0.20f}'.format})
# colour.models.sRGB_COLOURSPACE.use_derived_transformation_matrices(True)
# colour.models.ADOBE_RGB_1998_COLOURSPACE.use_derived_transformation_matrices(True)
# print(colour.models.sRGB_COLOURSPACE.RGB_to_XYZ_matrix)
# print(colour.models.ADOBE_RGB_1998_COLOURSPACE.RGB_to_XYZ_matrix)
#```
transformation_matrix <-
    list(
        Adobe = structure(
            c(0.57666904291013043604, 0.29734497525053604772, 0.02703136138641233990,
              0.18555823790654629724, 0.62736356625546607635, 0.07068885253582722628,
              0.18822864623499471759, 0.07529145849399788981, 0.99133753683763892184),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        sRGB = structure(
            c(0.41239079926595950676, 0.21263900587151038368, 0.01933081871559183193,
              0.35758433938387801376, 0.71516867876775602753, 0.11919477979462597683,
              0.18048078840183431892, 0.07219231536073372757, 0.95053215224966081109),
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
# np.set_printoptions(formatter={'float': '{:0.20f}'.format})
# colour.models.sRGB_COLOURSPACE.use_derived_transformation_matrices(True)
# colour.models.ADOBE_RGB_1998_COLOURSPACE.use_derived_transformation_matrices(True)
# print(colour.models.sRGB_COLOURSPACE.XYZ_to_RGB_matrix)
# print(colour.models.ADOBE_RGB_1998_COLOURSPACE.XYZ_to_RGB_matrix)
#```
transformation_matrix_inverse <-
    list(
        Adobe = structure(
            c(2.04158790381074695119, -0.96924363628087994993, 0.01344428063203114906,
              -0.56500697427885993029, 1.87596750150772062504, -0.11836239223101836793,
              -0.34473135077832967044, 0.04155505740717563984, 1.01517499439120539861),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),

        sRGB = structure(
            c(3.24096994190452125650, -0.96924363628087972788, 0.05563007969699364924,
             -1.53738317757009324005, 1.87596750150772040300, -0.20397695888897651728,
             -0.49861076029300327450, 0.04155505740717560514, 1.05697151424287838495),
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


