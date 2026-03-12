test_that("predict_newdata preserves positive class ordering", {

  df <- data.frame(
    x = rnorm(100),
    y = factor(rep(c("1","0"), each = 50), levels = c("0","1"))
  )

  task <- as_task_classif(df, target = "y", positive = "1")

  learner <- lrn("classif.featureless", predict_type = "prob")
  learner$train(task)

  pred1 <- learner$predict(task)
  pred2 <- learner$predict_newdata(df, task = task)

  expect_equal(pred1$prob, pred2$prob)
})