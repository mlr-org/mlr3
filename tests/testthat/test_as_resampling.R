test_that("as_resampling conversion", {
  resampling = rsmp("subsampling")
  converted = as_resampling(resampling)
  cloned = as_resampling(resampling, clone = TRUE)

  expect_class(converted, "Resampling")
  expect_same_address(resampling, converted)
  expect_different_address(resampling, cloned)

  expect_list(as_resamplings(resampling), types = "Resampling")
  expect_list(as_resamplings(list(resampling)), types = "Resampling")
})
