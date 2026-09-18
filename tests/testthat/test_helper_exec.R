test_that("set_encapsulation() supports all encapsulation methods of Learner$encapsulate()", {
  for (method in c("none", "try", "evaluate", "callr", "mirai")) {
    learner = lrn("classif.rpart")
    set_encapsulation(list(learner), method)
    expect_equal(unname(learner$encapsulation), rep(method, 2L), info = method)
    if (method == "none") {
      expect_null(learner$fallback)
    } else {
      expect_class(learner$fallback, "LearnerClassifFeatureless")
    }
  }

  learner = lrn("classif.rpart")
  set_encapsulation(list(learner), NA_character_)
  expect_equal(unname(learner$encapsulation), c("none", "none"))
})
