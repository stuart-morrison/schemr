
## OpenImageR 1.3.0

* I updated the README.md file related to "[[Rcpp::plugins(cpp11)]]", which was removed in the previous version
* I fixed a bug in the internal *'func_chech_range()'* function (see issue: https://github.com/mlampros/OpenImageR/issues/28)


## OpenImageR 1.2.9

* I added an exception in the test-file *"test-norm_matrix_range.R"* related to Mac OSx and Power-PC (see issue: https://github.com/mlampros/OpenImageR/issues/27)
* I removed the "CXX_STD = CXX11" from the "Makevars" files, and the "[[Rcpp::plugins(cpp11)]]" from the "extract_inst_folder_headers.cpp" file due to the following NOTE from CRAN, "NOTE Specified C++11: please drop specification unless essential" (see also: https://www.tidyverse.org/blog/2023/03/cran-checks-compiled-code/#note-regarding-systemrequirements-c11)
* I removed the '-mthreads' compilation flags from the Makevars.win file because I received a CRAN NOTE


## OpenImageR 1.2.8

* I added the *'verify_image_extension()'* function to verify that the image path file extensions are supported from the OpenImageR package (see issue: https://github.com/mlampros/OpenImageR/issues/25) 


## OpenImageR 1.2.7

* I updated the output error message in all cases of 3-dimensional images where the third dimension had to be equal to 3


## OpenImageR 1.2.6

* The "delationErosion" function will be replaced with the "dilationErosion" function in version 1.3.0, because the name of the algorithm was wrong ('delation' actually corresponds to 'dilation'). See issue https://github.com/mlampros/OpenImageR/issues/24


## OpenImageR 1.2.5

* I fixed the (debian, fedora) *clang14* warning: "use of bitwise '&' with boolean operands"


## OpenImageR 1.2.4

* I fixed a *clang-UBSAN* warning related to the *warpAffine()* and *warpAffine_2d()* functions of the *OpenImageRheader.h* file (this warning was visible in the checks of the *fastGLCM* R package)


## OpenImageR 1.2.3

* I've added the *getAffineTransform()* and *warpAffine()* functions
* I've added the *Warp_Affine* vignette explaining the functionality of the included functions
* I've added tests for the *getAffineTransform()* and *warpAffine()* functions
* I updated the *COPYRIGHTS* and *CITATION* files
* I modified the *imageShow()* function by adding the *clear_viewer* parameter. If this parameter is TRUE then the previous image will be removed in the viewer before displaying the next one


## OpenImageR 1.2.2

* I've modified the *'plot_multi_images()'* method of the *GaborFeatureExtract* R6 class by adding the *titles* and the *axes* parameters.


## OpenImageR 1.2.1

* I've fixed an mistake in the code of the *RGB_to_HSV()* function in the *inst/include/OpenImageRheader.h* file related to an [StackOverflow issue](https://stackoverflow.com/q/70183472)
* I've added the *'normalize_pixels'* parameter to the *'resizeImage()'* function and I've set it to FALSE so that output pixel values are not normalized by default


## OpenImageR 1.2.0

* I've modified the *resize_nearest_rcpp()* function in the *inst/include/OpenImageRheader.h* file by adding a warning in case that the input parameters *width* and *height* do not match with the output *width* and *height*. See the [issue 21](https://github.com/mlampros/OpenImageR/issues/21) in the OpenImageR Github repository. Moreover, I've added tests for the case that the *resizeImage(method = "nearest")* function gives a warning.


## OpenImageR 1.1.9

* I fixed the error in the CRAN results due to mistakes in creation of matrices and arrays in the test (testthat) files.


## OpenImageR 1.1.8

* I've added the *CITATION* file in the *inst* directory


## OpenImageR 1.1.7

* I fixed an error in the *'equal_spaced'* version of the *crop_image_secondary()* function
* I fixed invalid URL's (both in R scripts and vignettes)
* I added the *superpixel_bbox()* function which returns the bounding box boundaries for the superpixels
* I added the *superpixel_bbox_subset()* function which returns the bounding box boundaries of a subset of superpixels
* I added the *padding()* function, which allows a user to pad / extend a matrix or array using a specified value


## OpenImageR 1.1.6

* I renamed the *load_3d_binary* to *load_binary* and now a user can load either a 2- or a 3-dimensional object. I also fixed a bug in the function.
* I modified the *superpixels* function, so that it can take 2-dimensional objects too (matrices) besides 3-dimensional images (no need to convert to 3-dimensional outside of the function).
* I added the parameter *vectorize_magnitude* to the *gabor_feature_extraction* method of the *GaborFeatureExtract* R6 class. In case that the *vectorize_magnitude* parameter is set to FALSE the user will receive the magnitude in form of a matrix ( for instance if the *scales* parameter is set to 2 and the (rows, cols) of the image are (128, 128) then the resulted magnitude will be a list of length 2 where each sublist will be a matrix with dimensions (128, 128) (pixel-wise magnitude values) )
* I modified the *meshgrid_x()* and *meshgrid_y()* Rcpp functions and now they return faster as before
* I modified the check-input-data-cases for the *RGB_to_Lab* and *RGB_to_HSV*
* I fixed the *"failure: the condition has length > 1"* CRAN error which appeared mainly due to the misuse of the base *class()* function in multiple code snippets in the package (for more info on this matter see: https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html)


## OpenImageR 1.1.5

* I renamed the internal function *norm_range_gauss* to *norm_matrix_range*. Now, the *norm_matrix_range* function is accessible when loading the OpenImageR package. Moreover, I excluded rounding [ round() ], which occured by default in the *norm_range_gauss* function.


## OpenImageR 1.1.4

* I fixed the duplicated vignette title warning


## OpenImageR 1.1.3

* I fixed a documentation error as suggested in [issue 11](https://github.com/mlampros/OpenImageR/issues/11) of the OpenImageR Github repository (converted radians to degrees in all scripts of the OpenImageR package).


## OpenImageR 1.1.2

* I converted the OpenImageR package to a header-only package, so that Rcpp functions can be called from another R package.
* I added the *SLIC* and *SLICO* superpixel algorithms as header files and as a function (*superpixels()*) which can be called from within R.
* I added the *load_3d_binary()* function in case that a user intends to load 3d-image data from a binary file (as is the case when a user writes data when using the *superpixels()* function).
* I added the RGB (Red-Green-Blue) to LAB (Lightness, A-colour-dimension, B-colour-dimension) colour conversion (*RGB_to_Lab()*).
* I added the RGB (Red-Green-Blue) to HSV (Hue, Saturation, Value) colour conversion (*RGB_to_HSV()*).
* I added the vignette "Image_segmentation_superpixels_clustering.Rmd"


## OpenImageR 1.1.1

* I modified the *rotate_nearest_bilinear* Rcpp function in the *utils.cpp* file to fix the ASAN-gcc error.


## OpenImageR 1.1.0

* I fixed the ASAN-gcc error.
* I commented 3 test cases for the Solaris OS, otherwise it gives an error (see the comments in the *test-gabor.R* tests file).
* I updated the Vignette *Gabor Feature extraction*.


## OpenImageR 1.0.9

I added the *GaborFeatureExtract* R6 class, which as the name suggests extracts Gabor features from images (I updated the documentation and Vignettes too)


## OpenImageR 1.0.8

I added the *DARMA_64BIT_WORD* flag in the Makevars file to allow the package processing big datasets


## OpenImageR 1.0.7

I removed the *threads* parameter from the *Augmentation* function as there is no significant improvement (in execution speed) between a threaded and a non-threaded version.
I modified the *OpenMP* clauses of the .cpp files to address the ASAN errors.


## OpenImageR 1.0.6

I fixed a bug in the *translation_mat* function of the *utils.cpp* source file (https://github.com/mlampros/OpenImageR/issues/6 and https://github.com/mlampros/OpenImageR/issues/7). Moreover, I added the *padded_value* parameter in the *translation* and *Augmentation* R functions to allow the user to adjust the shifted values ( in case that *shift_rows* or *shift_cols* is not 0 ). The *padded_value* parameter can take either a numeric value or a numeric vector depending on the dimensions of the image. Previously, the shifted values were replaced by default with 0.


## OpenImageR 1.0.5

I fixed a bug in the *dhash* function according to the [Changelog](https://github.com/JohannesBuchner/imagehash) of the imagehash python library. Moreover, I added a function to round float numbers in the *image_hashing.cpp* source file, otherwise by creating hash values using the *bilinear* interpolation method the returned values are incorrect.


## OpenImageR 1.0.4

I modified the R script files to accept extension types similar to .tiff such as .tif, .TIFF or .TIF


## OpenImageR 1.0.3

I modified the Augmentation function to correct an issue which occured due to the zca_comps parameter


## OpenImageR 1.0.2

I did OpenMP optional for all cpp files due to the fact that the OSX OS does not currently support openMP ( r-devel-osx-x86_64-clang )


I removed the floor() function from the lines 45, 46 of the hog_features.cpp, because otherwise Solaris OS throws the following error:

* error: call of overloaded ‘floor(int)’ is ambiguous


I can not fix the following error in the hog_features.cpp file for the r-oldrel-windows platform as it has either to do with the RcppArmadillo version or is GCC specific,

* error: could not convert '{{-0x00000000000000001, 0, 1}, {-0x00000000000000002, 0, 2}, {-0x00000000000000001, 0, 1}}' from '<brace-enclosed initializer list>' to 'arma::mat {aka arma::Mat<double>}'

Platforms, which support newer versions of GCC (GCC >= 4.9.0) and a recent version of RcppArmadillo (>= 0.7)  do not throw such an error.


## OpenImageR 1.0.1

I modified the Makevars files, so that OpenImageR can be installed error-free in the following OS's,

* r-devel-osx-x86_64-clang
* r-patched-solaris-sparc
* r-patched-solaris-x86
* r-oldrel-windows-ix86+x86_64


## OpenImageR 1.0.0

