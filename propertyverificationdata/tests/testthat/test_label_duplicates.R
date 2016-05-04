library(testthat)
library(devtools)
library(dplyr)
load_all()

context("Label duplicated questions")

test_that("duplicated questions are labeled", {
  subj <- data_frame(
    subj_id = "Jed",
    question = c("a", "a", "b", "a", "b")
  )
  labeled <- label_exposure_order(subj)
  expect_equal(labeled$exposure_order, c(1, 2, 1, 3, 2))
})

test_that("exposure order is split into a factor", {
  subj <- data_frame(
    subj_id = "Jed",
    question = c("a", "a", "b", "a", "b")
  )
  labeled <- label_exposure_order(subj)
  expect_equal(labeled$exposure_split, c("first", "duplicate", "first", "duplicate", "duplicate"))
})

test_that("exposure order is centered on first exposure", {
  subj <- data_frame(
    subj_id = "Jed",
    question = c("a", "a", "b", "a", "b")
  )
  labeled <- label_exposure_order(subj)
  expect_equal(labeled$exposure_c_first, c(0, 1, 0, 1, 1))
})


context("Label duplicated propositions")

test_that("second occurrences are labeled", {
  is_duplicate <- label_duplicates(c("a", "a"))
  expect_equal(is_duplicate, c(FALSE, TRUE))
})

test_that("unique occurrences are unlabeled", {
  is_duplicate <- label_duplicates(c("a", "b"))
  expect_equal(is_duplicate, c(FALSE, FALSE))
})

test_that("more than two occurrences are labeled", {
  is_duplicate <- label_duplicates(c("a", "a", "a"))
  expect_equal(is_duplicate, c(FALSE, TRUE, TRUE))
})

test_that("it works with group by", {
  frame <- data_frame(
    subj_id = rep(c("subj1", "subj2"), each = 2),
    question_id = c("a", "b", "a", "a")
  )
  is_duplicate <- frame %>%
    group_by(subj_id) %>%
    mutate(is_duplicate = label_duplicates(question_id)) %>%
    .$is_duplicate
  expect_equal(is_duplicate, c(FALSE, FALSE, FALSE, TRUE))
})
