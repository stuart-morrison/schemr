context("test-color-spaces")

# Test colour space conversions
hex_codes <- c("#008698", "#232C31",  "#b5a991", "#c94b20", "#696A6D", "#A79344",
               "#006472", "#4e4f51",  "#963817", "#928262", "#7d6e33", "#28e5ff",
               "#a4a5a7",  "#e88e6f", "#d2cbbc", "#cfc189")

# Test that conversion functions pass without error
test_that("Colour space conversions pass without error", {
          expect_equal(sum(is.na(hex_to_lab(hex_codes))), 0)
          expect_equal(sum(is.na(hex_to_rgb(hex_codes))), 0)
          expect_equal(sum(is.na(hex_to_lab(hex_codes, transformation = "Adobe"))), 0)
          expect_equal(sum(is.na(rgb_to_hex(hex_to_rgb(hex_codes)))), 0)
          expect_equal(sum(is.na(lab_to_hex(hex_to_lab(hex_codes)))), 0)
          expect_equal(sum(is.na(lab_to_hex(hex_to_lab(hex_codes, transformation = "Adobe"), transformation = "Adobe"))), 0)
          })

### Test simple conversions
# sRGB
test_that("Colour space conversions and back again with sRGB", {
          expect_equal(rgb_to_hex(hex_to_rgb(hex_codes)), tolower(hex_codes))
          expect_equal(xyz_to_hex(hex_to_xyz(hex_codes)), tolower(hex_codes))
          expect_equal(lab_to_hex(hex_to_lab(hex_codes)), tolower(hex_codes))
          })

# Adobe
test_that("Colour space conversions and back again with sRGB", {
          expect_equal(xyz_to_hex(hex_to_xyz(hex_codes, transformation = "Adobe"), transformation = "Adobe"), tolower(hex_codes))
          expect_equal(lab_to_hex(hex_to_lab(hex_codes, transformation = "Adobe"), transformation = "Adobe"), tolower(hex_codes))
          })





