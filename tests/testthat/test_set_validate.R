test_that("set_validate works", {
  learner = lrn("classif.debug")
  expect_null(learner$validate)

  expect_invisible(set_validate(learner, 0.2))
  expect_equal(learner$validate, 0.2)

  set_validate(learner, "test")
  expect_equal(learner$validate, "test")

  set_validate(learner, NULL)
  expect_null(learner$validate)

  expect_error(set_validate(learner, 2))
  expect_error(set_validate(learner, "foo"))
})

test_that("set_validate fails for learners without validation", {
  expect_error(set_validate(lrn("classif.rpart"), 0.2), "does not support validation", class = "Mlr3ErrorInput")
})
