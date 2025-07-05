test_that("autotest catches error in train", {
  learner = lrn("classif.debug", error_train = 1)
  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_integer(result$seed)
  expect_task(result$task)
  expect_learner(result$learner)
  expect_null(result$prediction)
  expect_null(result$score)
  expect_string(result$error)
})

test_that("autotest catches error in predict", {
  learner = lrn("classif.debug", error_predict = 1)
  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_integer(result$seed)
  expect_task(result$task)
  expect_learner(result$learner)
  expect_null(result$prediction)
  expect_null(result$score)
  expect_string(result$error)
})

test_that("autotest recognizes learner that does not use .get_weights()", {
  learner = R6Class("learner", inherit = LearnerClassifDebug, private = list(
    .train = function(task) {
      structure(
        list(response = as.character(sample(task$truth(), 1L))),
        class = "classif.debug_model"
      )
    }
  ))$new()

  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_string(result$error, pattern = "get_weights was not called")
})

test_that("autotest configure_learner works", {
  task = tsk("spam")
  learner = lrn("classif.debug", error_train = 1)
  # keep only feat_all_* tasks to save time
  exclude = "^feat_single|^missings|^weights|^utf8|^sanity"
  result = run_autotest(learner, 20, exclude)
  expect_false(result$ok)

  # example changes the learner's parameter during the autotest
  cfg_lrn = function(learner, task) {
    if (task$n_features > 1) {
      learner$param_set$set_values(error_train = 0)
    }
  }
  result = run_autotest(learner, 10, exclude = exclude, configure_learner = cfg_lrn)
  expect_true(result)
  expect_equal(learner$param_set$values$error_train, 1)
})
