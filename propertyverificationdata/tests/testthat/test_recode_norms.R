library(propertyverificationdata)

context("Use the same min/max values for norms")

test_that("norms scores are recoded properly", {
  norms <- data.frame(
    imagery_mean=0:4,
    facts_mean=0:4,
    difficulty=-2:2,
    truth=-2:2
  ) %>%
    recode_norms

  expect_equal(norms$imagery_mean, 1:5)
  expect_equal(norms$facts_mean, 1:5)
  expect_equal(norms$difficulty, 1:5)
  expect_equal(norms$truth, 1:5)
})
