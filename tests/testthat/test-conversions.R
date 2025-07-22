test_that("temperature conversion works", {
  expect_equal(convert_temperature(0, "C", "F"), 32)
  expect_equal(convert_temperature(100, "C", "K"), 373.15)
})

test_that("weight conversion works", {
  expect_equal(round(convert_weight(1, "kg", "g"), 2), 1000)
  expect_equal(round(convert_weight(1, "lb", "kg"), 2), 0.45)
})

test_that("volume conversion works", {
  expect_equal(convert_volume(1, "l", "ml"), 1000)
  expect_equal(round(convert_volume(1, "cup", "ml"), 0), 240)
})
