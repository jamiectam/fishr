test_that("cpue handles vectors of data", {
  catches <- c(100, 200, 300)
  efforts <- c(10, 10, 10)
  expected_results <- c(10, 20, 30)

  expect_equal(cpue(catches, efforts), expected_results, ignore_attr=TRUE)
})

test_that("cpue handles missing data", {
  expect_true(is.na(cpue(NA_real_, 10)))
})

test_that("cpue matches reference data", {
  result <- cpue(reference_data$catch, reference_data$effort)

  expect_equal(result, reference_data$expected_cpue, ignore_attr=TRUE)
})

test_that("cpue provides informative message when verbose", {
  expect_message(
    cpue(c(100, 200), c(10, 20), verbose = TRUE),
    "Processing 2 records"
  )
})

test_that("cpue is silent by default", {
  expect_no_message(cpue(100, 10))
})

## Snapshot tests

test_that("cpue errors when input is not numeric", {
  expect_snapshot(
    cpue("five", 10),
    error = TRUE
  )
})


test_that("cpue uses verbosity when option set to TRUE", {
  withr::local_options(fishr.verbose = TRUE) # will be reset when this test_that block finishes

  expect_snapshot(cpue(100, 10))
})

test_that("cpue is not verbose when option set to FALSE", {
  withr::local_options(fishr.verbose = FALSE) # will be reset when this test_that block finishes

  expect_silent(cpue(100, 10))
})

test_that("cpue verbosity falls back to FALSE when not set", {
  withr::with_options(
    list(fishr.verbose = NULL), # will be reset as soon as this code block executes
    expect_no_message(cpue(100, 10))
  )
})



