## ---- eval = T----------------------------------------------------------------

require(OpenImageR)

path = system.file('tmp_images', 'landscape.jpg', package = "OpenImageR")

img = readImage(path)
print(dim(img))


## ---- out.width = "65%", out.height = "50%", fig.align = 'center', fig.cap = "Input Image", fig.alt="Input Image", echo = F, eval = T----

knitr::include_graphics(path)


## ---- eval = T----------------------------------------------------------------

r = ncol(img)
c = nrow(img)
offset = 50

original_points = matrix(data = c(0, 0, r, 0, 0, c),
                         nrow = 3,
                         ncol = 2,
                         byrow = TRUE)

transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
                            nrow = 3,
                            ncol = 2,
                            byrow = TRUE)

M_aff = getAffineTransform(original_points = original_points,
                           transformed_points = transformed_points)


## ---- eval = T----------------------------------------------------------------

print(M_aff)


## ---- eval = T----------------------------------------------------------------

res_3d = warpAffine(img = img,
                    M = M_aff,
                    R = r,
                    C = c,
                    verbose = TRUE)

str(res_3d)


## ---- out.width = "65%", out.height = "50%", fig.align = 'center', fig.cap = "Output of Warp Affine function", fig.alt="Output of Warp Affine function", echo = T, eval = T----

imageShow(res_3d, clear_viewer = FALSE)


