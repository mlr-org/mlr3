test_that("utf8 feature names", {
  withr::local_options(list(mlr3.allow_utf8_names = TRUE))

  tab = data.table("Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich" = rnorm(10), "𝛼" = rnorm(10), "Съешь ещё этих мягких французских булок, да выпей чаю" = runif(10))
  task = as_task_regr(tab, target = "𝛼")

  devtools::load_all("../mlr3learners")
  learners = mlr_learners$keys("^regr")
  for (learner in lrns(learners)) {
    # learner$train(task)
    # learner$model
    # learner$predict(task)
    rr = resample(task, learner, rsmp("holdout"))
  }


})
