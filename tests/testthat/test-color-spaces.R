context("test-color-spaces")


# Test colour space conversions
hex_codes <- c("#008698", "#232C31",  "#b5a991", "#c94b20", "#696A6D", "#A79344",
               "#006472", "#4e4f51",  "#963817", "#928262", "#7d6e33", "#28e5ff",
               "#a4a5a7",  "#e88e6f", "#d2cbbc", "#cfc189")



test_that("Colour space conversions go through", {
          expect_equal(lab_to_hex(hex_to_lab(hex_codes)), tolower(hex_codes))
          expect_equal(lab_to_hex(hex_to_lab(hex_codes, transformation = "Adobe"), transformation = "Adobe"), tolower(hex_codes))
          expect_equal(lab_to_rgb(rgb_to_lab(hex_to_rgb(hex_codes))), hex_to_rgb(hex_codes))
          expect_equal(lab_to_rgb(rgb_to_lab(hex_to_rgb(hex_codes), transformation = "Adobe"), transformation = "Adobe"), hex_to_rgb(hex_codes))
          })
