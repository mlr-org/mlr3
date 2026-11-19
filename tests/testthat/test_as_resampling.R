test_that("as_resampling conversion", {
  resampling = rsmp("subsampling")
  converted = as_resampling(resampling)
  cloned = as_resampling(resampling, clone = TRUE)

  expect_class(converted, "Resampling")
  expect_same_address(resampling, converted)
  expect_different_address(resampling, cloned)

  cloned$param_set$set_values(repeats = 2L)
  expect_identical(resampling$param_set$values$repeats, 30L)
  cloned = as_resamplings(list(resampling), clone = TRUE)[[1L]]
  cloned$param_set$set_values(repeats = 2L)
  expect_identical(resampling$param_set$values$repeats, 30L)

  expect_list(as_resamplings(resampling), types = "Resampling")
  expect_list(as_resamplings(list(resampling)), types = "Resampling")
})

test_that("error when arguments are misspelled", {
  expect_error(as_resampling(rsmp("holdout"), clone2 = TRUE), "Received the following")
})
