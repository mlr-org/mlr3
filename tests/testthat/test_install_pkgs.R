test_that("extract_pkgs works", {
  # this is a noop, mlr3 is stripped from required packages
  expect_character(install_pkgs("mlr3"), len = 0)

  expect_set_equal(
    extract_pkgs("rpart"),
    "rpart"
  )

  expect_set_equal(
    extract_pkgs(lrn("classif.rpart")),
    c("mlr3", "rpart")
  )

  expect_set_equal(
    extract_pkgs(msr("classif.auc")),
    c("mlr3", "mlr3measures")
  )

  expect_set_equal(
    extract_pkgs(tgen("xor")),
    c("mlr3", "mlbench")
  )

  rr = resample(tsk("mtcars"), lrn("regr.featureless"), rsmp("holdout"))
  expect_set_equal(
    extract_pkgs(rr),
    c("mlr3", "stats")
  )

  bmr = as_benchmark_result(rr)
  expect_set_equal(
    extract_pkgs(rr),
    c("mlr3", "stats")
  )

  expect_set_equal(
    extract_pkgs(list(lrns(c("regr.rpart", "regr.featureless")))),
    c("mlr3", "rpart", "stats")
  )
})
