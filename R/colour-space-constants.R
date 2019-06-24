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
        Apple = structure(
            c(0.4497288, 0.2446525, 0.0251848,
              0.3162486, 0.6720283, 0.1411824,
              0.1844926, 0.0833192, 0.9224628),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        Best = structure(
            c(0.6326696, 0.2284569, 0,
              0.2045558, 0.7373523, 0.0095142,
              0.1269946, 0.0341908, 0.8156958),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        Beta = structure(
            c(0.6712537, 0.3032726, 0,
              0.1745834, 0.6637861, 0.040701,
              0.1183829, 0.0329413, 0.784509),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        Bruce = structure(
            c(0.4674162, 0.2410115, 0.0219101,
              0.2944512, 0.6835475, 0.0736128,
              0.1886026, 0.075441, 0.9933071),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        CIE = structure(
            c(0.488718, 0.1762044, 0,
              0.3106803, 0.8129847, 0.0102048,
              0.2006017, 0.0108109, 0.9897952),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        ColorMatch = structure(
            c(0.5093439, 0.274884, 0.0242545,
              0.3209071, 0.6581315, 0.1087821,
              0.1339691, 0.0669845, 0.6921735),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        Don = structure(
            c(0.6457711, 0.2783496, 0.0037113,
              0.1933511, 0.6879702, 0.0179861,
              0.1250978, 0.0336802, 0.8035125),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        ECI = structure(
            c(0.6502043, 0.3202499, 0,
              0.1780774, 0.6020711, 0.067839,
              0.1359384, 0.0776791, 0.757371),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        Ekta = structure(
            c(0.5938914, 0.2606286, 0,
              0.2729801, 0.7349465, 0.0419969,
              0.0973485, 0.0044249, 0.7832131),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        NTSC = structure(
            c(0.6068909, 0.2989164, 0,
              0.1735011, 0.586599, 0.0660957,
              0.200348, 0.1144845, 1.1162243),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        PAL = structure(
            c(0.430619, 0.2220379, 0.0201853,
              0.3415419, 0.7066384, 0.1295504,
              0.1783091, 0.0713236, 0.9390944),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        ProPhoto = structure(
            c(0.7976749, 0.2880402, 0,
              0.1351917, 0.7118741, 0,
              0.0313534, 8.57e-05, 0.82521),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        `SMPTE-C` = structure(
            c(0.3935891, 0.2124132, 0.0187423,
              0.3652497, 0.7010437, 0.1119313,
              0.1916313, 0.0865432, 0.9581563),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        sRGB = structure(
            c(0.4124564, 0.2126729, 0.0193339,
              0.3575761, 0.7151522, 0.119192,
              0.1804375, 0.072175, 0.9503041),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        ),
        `Wide Gamut` = structure(
            c(0.7161046, 0.2581874, 0,
              0.1009296, 0.7249378, 0.0517813,
              0.1471858, 0.0168748, 0.7734287),
            .Dim = c(3L, 3L),
            .Dimnames = list(NULL, NULL)
        )
    )

# Tristimulus values of the reference white point
# Illuminant D50
# https://en.wikipedia.org/wiki/CIELAB_color_space
x_n <- 96.4212
y_n <- 100
z_n <- 82.5188

