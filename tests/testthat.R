if (requireNamespace("testthat", quietly = TRUE)) {
  library("checkmate")
  library("testthat")
  library("mlr3")
  test_check("mlr3")
}
